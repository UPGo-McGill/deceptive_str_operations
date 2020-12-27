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

# decreased size needed due to lack of computational power
classified_texts <- sample_n(classified_texts, size = nrow(classified_texts)/14)


# Quanteda - bigrams creation ---------------------------------------------

bigrams <-  
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
  # tokens_ngrams(n = 2) %>% 
  # to document-feature matrix
  dfm() %>% 
  # trim because it's too large!
  dfm_trim(min_docfreq = 1, min_termfreq = 2) %>% 
  as.matrix()


# Term Frequency - Inverse Document Frequency (TF-IDF) ---------
# Since some reviews are longer than others, "term frequency" will normalize all
# reviews to be length independant. We also accounts for the frequency of bigrams
# appearing in virtually every documents with the IDF calculation. We multiply
# both TF and IDF for each cell in the matrix.

# normalize all documents with term frequency
bigrams_tf <- apply(bigrams, 1, 
                    function(row) row/sum(row))

# calculate the inverse document frequency
bigrams_idf <- apply(bigrams, 2, 
                     function(col) log10(length(col) / length(which(col > 0))))

# calculate TF-IDF 
bigrams_tfidf <-  apply(bigrams_tf, 2, 
                        function(x, idf) x * idf, idf = bigrams_idf)

# transpose the matrix
bigrams_tfidf <- t(bigrams_tfidf)

# check and fix incomplete cases
incomplete_cases <- which(!complete.cases(bigrams_tfidf))
bigrams_tfidf[incomplete_cases,] <- rep(0.0, ncol(bigrams_tfidf))


# Singular value decomposition (SVD) --------------------------------------

# reduce dimensionality so that it is more memory-efficient. Find a
# few approximate singular values and corresponding singular vectors of the
# matrix. The new 300 columns it returns are the single most significant
# representations of the data extracted out of the TFIDF matrix.
bigrams_irlba <- irlba::irlba(t(bigrams_tfidf), nv = 300, maxit = 600)
bigrams_irlba <- as_tibble(bigrams_irlba$v)


# Cosine similarity -------------------------------------------------------
# to check similarity of fake reviews in the vector space. Risky feature,
# it can quickly result in overfitting. 

bigrams_similarities <- lsa::cosine(t(as.matrix(bigrams_irlba)))

# take every review and find the mean cosine similarity to fake reviews. The
# hypothesis is that fake reviews are more similar to fake ones than genuine ones
fake_indexes <- which(classified_texts$fake == T)

bigrams_cs <- 
bigrams_irlba %>% 
  mutate(fake_similarity = as.numeric(0))

for(i in 1:nrow(bigrams_cs)){
  bigrams_cs$fake_similarity[i] <- mean(bigrams_similarities[i, fake_indexes])
}

# visualize
ggplot(data.frame(bigrams_cs, fake = classified_texts$fake), aes(fake_similarity, fill = fake))+
  theme_bw()+
  geom_histogram(binwidth = 0.005)+
  labs(y = "Review count",
       x = "Mean fake review cosine similarity",
       title = "Distribution of fake vs genuine using fake review cosine similarity")
# On a per review basis, what is the average cosine similarity between a review and
# all other fake reviews. On the middle of the graph, close to 0, there is a
# higher amount of genuine reviews, meaning they have no cosine similarity with 
# the fake reviews. The hypothesis that in general fake reviews should have 
# higher cosine similarity on average with other fake reviews then they do with 
# genuine reviews is true. Vice-versa is also true. 


# Addition of LIWC --------------------------------------------------------

liwc_results <- 
  (read.csv("output/liwc2015_results.csv", dec = ",") %>% 
     as_tibble() %>% 
     rename(review_ID = A,
            review = B))[-1,] %>% 
  mutate(review_ID = as.numeric(review_ID)) %>% 
  filter(review_ID %in% classified_texts$review_ID) %>% 
  select(review_ID, WC, WPS, Authentic, Analytic,  Tone, Exclam, #  Linguistic processes
           function., ppron, i, we, shehe, they, adj, # Function words
           affect, posemo, negemo, # Psychological processes - emotional
           social, family, friend, female, male, # Psychological processes - social
           cogproc, insight, cause, discrep, tentat, certain, differ, # Psychological processes - cognitive
           time, focuspast, focuspresent, focusfuture, space, relativ, # Psychological processes - time and space
           percept, see, hear, feel, # Psychological processes - perceptual
           bio, body, health, sexual, ingest, # Psychological processes - biological
           work, leisure, home, money, relig) # Personal concerns

bigrams_liwc <- 
  data.frame(review_ID = classified_texts$review_ID,
             fake = classified_texts$fake,
             bigrams_cs) %>% 
  inner_join(liwc_results, by = "review_ID") %>% 
  select(-review_ID)



# Random Forest -----------------------------------------------------------

train_data <- bigrams_liwc

# create stratified folds for 10-fold cross validation repeated 
# 3 times (30 random stratified samples)
cv_folds <- createMultiFolds(classified_texts$fake, k = 10, times = 3)

cv_control <- trainControl(method = "repeatedcv", number = 10,
                           repeats = 3, index = cv_folds)

# allow for working on x logical cores (working on all OS)
cl <- parallel::makeCluster(parallel::detectCores()-1, type = "SOCK")
doSNOW::registerDoSNOW(cl)

# use Random Forest with the default of 500 trees and allow caret to try 7 
# different values of mtry to find the mtry value that gives the best result
randomforest_res <- train(fake ~ ., data = train_data, method = "rf",
                trControl = cv_control, tuneLength = 7, importance = TRUE)

# Processing is done, stop cluster.
parallel::stopCluster(cl)

# info about the model
randomforest_res
# preds <- predict(randomforest_res, train_data)
# confusionMatrix(preds, train_data$fake)
confusionMatrix(train_data$fake, randomforest_res$finalModel$predicted)
randomForest::varImpPlot(randomforest_res$finalModel)
# On the last set of info, keep an eye on what is referenced and what is
# predicted. Tagging a genuine review as fake is worse than tagging a fake review
# as genuine. "Sensitivity" needs to be high.

# Accuracy of the model with uni-gram and 1k observations is, at the moment, 74%
# Most important variables are fake_similarity (maybe a sign of overfitting), the
# use of the first person pronouns and a focus on past tense.