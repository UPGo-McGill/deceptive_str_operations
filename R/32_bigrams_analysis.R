source("R/01_source.R")
library(caret)
library(tidytext)
library(quanteda)


# Load data ---------------------------------------------------------------

load("output/classified_texts.Rdata")

# Preparation for bigram analysis -----------------------------------------


# Bigrams analysis with tidytext ------------------------------------------

# Bigrams creation
bigrams_tidytext <- 
  classified_texts %>% 
  # Removing unnecessary characters (punctuations? numbers? etc.)
  mutate(review = str_remove_all(review, pattern = "[0-9]+|[[:punct:]]|\\(.*\\)")) %>% 
  # tokenization
  unnest_tokens(bigram, review, token = "ngrams", n = 2) %>% 
  # removing stopwords like "the", "a", "an", etc.
  separate(bigram, into = c("word1", "word2"), sep = " ") %>% 
  filter(!word1 %in% stop_words$word,
         !word2 %in% stop_words$word) %>%
  unite(bigram, c(word1, word2), sep = " ") %>% 
  # words stemming
  separate(bigram, into = c("word1", "word2"), sep = " ") %>%
  mutate(word1 = SnowballC::wordStem(word1),
         word2 = SnowballC::wordStem(word2)) %>%
  unite(bigram, c(word1, word2), sep = " ") 

# Visualization of most frequent bigrams
word_cloud <- 
  bigrams_tidytext %>%
  group_by(fake) %>% 
  count(bigram, sort = TRUE)

wordcloud::wordcloud(words = word_cloud$bigram, freq = word_cloud$n, 
                     ordered.colors = F, max.words = 50, 
                     colors = RColorBrewer::brewer.pal(3, "Set1")[factor(word_cloud$fake)])



# Bigrams analysis with quanteda ------------------------------------------
# Unigram for now ###

# Error: protect(): protection stack overflow when running the model! So decrease size
classified_texts <- sample_n(classified_texts, size = nrow(classified_texts)/7)

#Bigrams creation
bigrams_quanteda <-  
  #tokenization
  tokens(classified_texts$review, what = "word", remove_punct = T, 
         remove_symbols = T, 
         remove_numbers = T, 
         remove_separators = T) %>% 
  # removing stopwords like "the", "a", "an", etc. from tidytext package
  tokens_select(stop_words$word, selection = "remove") %>% 
  # words stemming
  tokens_wordstem(language = "english") %>% 
  # to document-feature matrix
  dfm() %>% 
  # trim because it's too large!
  dfm_trim(min_docfreq = 1, min_termfreq = 2)

# dfm to matrix to add the "fake" column
bigrams_quanteda <- cbind(fake = classified_texts$fake, 
                          as.data.frame(bigrams_quanteda))

# make syntactically valid names, for no future problems
names(bigrams_quanteda) <- make.names(names(bigrams_quanteda))

# problems due to duplicated column names
bigrams_quanteda <- bigrams_quanteda[, !duplicated(colnames(bigrams_quanteda))]

# create a stratified split
training_samples <- createDataPartition(classified_texts$fake, times = 1,
                               p = 0.7, list = FALSE)

train_data <- classified_texts[training_samples,]
test_data <- classified_texts[-training_samples,]

# create stratified folds for 10-fold cross validation repeated 
# 3 times (30 random stratified samples)
cv_folds <- createMultiFolds(train_data$fake, k = 10, times = 3)

cv_control <- trainControl(method = "repeatedcv", number = 10,
                         repeats = 3, index = cv_folds)

# Allow for working on x logical cores (working on all OS)
cl <- parallel::makeCluster(parallel::detectCores()-1, type = "SOCK", outfile = "")
doSNOW::registerDoSNOW(cl)

# test the model
model <- train(fake ~ ., data = bigrams_quanteda, method = "rpart", 
                    trControl = cv_control, tuneLength = 7)

# stop cluster
parallel::stopCluster(cl)

# Take a look at model
model
# With 2k of classified text, accuracy is 61% for uni-gram.
