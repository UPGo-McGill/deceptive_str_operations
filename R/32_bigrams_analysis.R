source("R/01_source.R")
library(caret)
library(tidytext)
library(quanteda)


# Load data ---------------------------------------------------------------

load("output/classified_texts.Rdata")

# Preparation for bigram analysis -----------------------------------------



# Tidytext - bigrams analysis - convenient for visualization --------------

# # Bigrams creation
# bigrams_tidytext <- 
#   classified_texts %>% 
#   # Removing unnecessary characters (punctuations? numbers? etc.)
#   mutate(review = str_remove_all(review, pattern = "[0-9]+|[[:punct:]]|\\(.*\\)")) %>% 
#   # tokenization
#   unnest_tokens(bigram, review, token = "ngrams", n = 2) %>% 
#   # removing stopwords like "the", "a", "an", etc.
#   separate(bigram, into = c("word1", "word2"), sep = " ") %>% 
#   filter(!word1 %in% stop_words$word,
#          !word2 %in% stop_words$word) %>%
#   unite(bigram, c(word1, word2), sep = " ") %>% 
#   # words stemming
#   separate(bigram, into = c("word1", "word2"), sep = " ") %>%
#   mutate(word1 = SnowballC::wordStem(word1),
#          word2 = SnowballC::wordStem(word2)) %>%
#   unite(bigram, c(word1, word2), sep = " ") 
# 
# # Visualization of most frequent bigrams
# word_cloud <- 
#   bigrams_tidytext %>%
#   group_by(fake) %>% 
#   count(bigram, sort = TRUE)
# 
# wordcloud::wordcloud(words = word_cloud$bigram, freq = word_cloud$n, 
#                      ordered.colors = F, max.words = 50, 
#                      colors = RColorBrewer::brewer.pal(3, "Set1")[factor(word_cloud$fake)])


# Quanteda - bigrams analysis ---------------------------------------------

# Error: protect(): protection stack overflow when running the model! So decrease size
classified_texts <- sample_n(classified_texts, size = nrow(classified_texts)/14)


# Quanteda - bigrams creation ---------------------------------------------

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
  # making bigrams
  tokens_ngrams(n = 2) %>% 
  # to document-feature matrix
  dfm() %>% 
  # trim because it's too large!
  dfm_trim(min_docfreq = 1, min_termfreq = 2) %>% 
  as.matrix()


# Quanteda - Term Frequency - Inverse Document Frequency (TF-IDF) ---------
# Since some reviews are longer than others, we do term frequency to normalize all
# reviews to be length independant. We also accounts for the frequency of bigrams
# appearing in virtually every documents with the IDF calculation. We multiply
# both TF and IDF for each cell in the matrix.

# normalize all documents with term frequency
bigrams_quanteda_tf <- apply(bigrams_quanteda, 1, 
                             function(row) row/sum(row))

# calculate the inverse document frequency
bigrams_quanteda_idf <- apply(bigrams_quanteda, 2, 
                              function(col) log10(length(col) / length(which(col > 0))))

# calculate TF-IDF 
bigrams_quanteda_tfidf <-  apply(bigrams_quanteda_tf, 2, 
                                 function(x, idf) x * idf, idf = bigrams_quanteda_idf)

# transpose the matrix
bigrams_quanteda_tfidf <- t(bigrams_quanteda_tfidf)

# check and fix incomplete cases
incomplete_cases <- which(!complete.cases(bigrams_quanteda_tfidf))
bigrams_quanteda_tfidf[incomplete_cases,] <- rep(0.0, ncol(bigrams_quanteda_tfidf))


# Quanteda - model testing ------------------------------------------------

# matrix to data.frame and addition of the "fake" column
bigrams_quanteda <- cbind(fake = classified_texts$fake, 
                          as.data.frame(bigrams_quanteda_tfidf))

# make syntactically valid names, for no future problems
names(bigrams_quanteda) <- make.names(names(bigrams_quanteda))

# fixing problems if duplicated column names (unusual, should not happen)
bigrams_quanteda <- bigrams_quanteda[, !duplicated(colnames(bigrams_quanteda))]

# create stratified folds for 10-fold cross validation repeated 
# 3 times (30 random stratified samples)
cv_folds <- createMultiFolds(bigrams_quanteda$fake, k = 10, times = 3)

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
# With 2k of classified text, accuracy is 61% for uni-gram, 55% for bigrams.


# Quanteda - Random Forest ------------------------------------------------



