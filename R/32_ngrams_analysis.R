source("R/01_source.R")
library(caret)
library(tidytext)
library(quanteda)


# Load data ---------------------------------------------------------------

load("output/classified_texts.Rdata")

# Preparation for bigram analysis -----------------------------------------



# Tidytext - ngrams analysis - convenient for visualization --------------

# # ngrams creation
# ngrams_tidytext <- 
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
# # Visualization of most frequent ngrams
# word_cloud <- 
#   ngrams_tidytext %>%
#   group_by(fake) %>% 
#   count(bigram, sort = TRUE)
# 
# wordcloud::wordcloud(words = word_cloud$bigram, freq = word_cloud$n, 
#                      ordered.colors = F, max.words = 50, 
#                      colors = RColorBrewer::brewer.pal(3, "Set1")[factor(word_cloud$fake)])






# Quanteda - ngrams analysis ---------------------------------------------

# decreased size needed due to lack of computational power
classified_texts <- sample_n(classified_texts, size = nrow(classified_texts)/14)

# create a 70%/30% stratified split
# Use caret to create a 70%/30% stratified split. Set the random
training_samples <- createDataPartition(classified_texts$fake, times = 1,
                               p = 0.7, list = FALSE)

train_data_raw <- classified_texts[training_samples,]
test_data_raw <- classified_texts[-training_samples,]


# Quanteda - ngrams creation ---------------------------------------------

# create ngrams in dfm to match it later with test data
ngrams_train_dfm <-  
  #tokenization
  tokens(train_data_raw$review, what = "word", remove_punct = T, 
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
  # trim because it's too large to convert in matrix!
  dfm_trim(min_docfreq = 1, min_termfreq = 2)

# convert it to matrix for next steps
ngrams_train <- 
  ngrams_train_dfm %>%
  as.matrix()


# Term Frequency - Inverse Document Frequency (TF-IDF) ---------
# Since some reviews are longer than others, "term frequency" will normalize all
# reviews to be length independant. We also accounts for the frequency of ngrams
# appearing in virtually every documents with the IDF calculation. We multiply
# both TF and IDF for each cell in the matrix.

# normalize all documents with term frequency
ngrams_train_tf <- apply(ngrams_train, 1, 
                    function(row) row/sum(row))

# calculate the inverse document frequency
ngrams_train_idf <- apply(ngrams_train, 2, 
                     function(col) log10(length(col) / length(which(col > 0))))

# calculate TF-IDF 
ngrams_train_tfidf <-  apply(ngrams_train_tf, 2, 
                        function(x, idf) x * idf, idf = ngrams_train_idf)

# transpose the matrix
ngrams_train_tfidf <- t(ngrams_train_tfidf)

# check and fix incomplete cases
incomplete_cases <- which(!complete.cases(ngrams_train_tfidf))
ngrams_train_tfidf[incomplete_cases,] <- rep(0.0, ncol(ngrams_train_tfidf))


# Singular value decomposition (SVD) --------------------------------------

# reduce dimensionality so that it is more memory-efficient. Find a
# few approximate singular values and corresponding singular vectors of the
# matrix. The new 300 columns it returns are the single most significant
# representations of the data extracted out of the TFIDF matrix.
ngrams_train_svd <- irlba::irlba(t(ngrams_train_tfidf), nv = 300, maxit = 600)

# these will be useful to convert the test data in the same svd semantic space
# as the training data. we use one row of the training data that's already been
# transformed by tf-idf.
sigma_inverse <- 1 / ngrams_train_svd$d
u_transpose <- t(ngrams_train_svd$u)
review <- ngrams_train_tfidf[1,]
review_hat <- sigma_inverse * u_transpose %*% review

# only take the relevant part, for us, of the value
ngrams_train_svd <- as_tibble(ngrams_train_svd$v)


# # Cosine similarity -------------------------------------------------------
# # to check similarity of fake reviews in the vector space. Risky feature,
# # it can quickly result in overfitting. This iw why it's marked with ###
# 
# ngrams_train_similarities <- lsa::cosine(t(as.matrix(ngrams_train_svd)))
# 
# # take every review and find the mean cosine similarity to fake reviews. The
# # hypothesis is that fake reviews are more similar to fake ones than genuine ones
# fake_indexes <- which(train_data_raw$fake == T)
# 
# ngrams_train_cs <- 
# ngrams_train_svd %>% 
#   mutate(fake_similarity = as.numeric(0))
# 
# for(i in 1:nrow(ngrams_train_cs)){
#   ngrams_train_cs$fake_similarity[i] <- mean(ngrams_train_similarities[i, fake_indexes])
# }
# 
# # visualize
# ggplot(data.frame(ngrams_train_cs, fake = train_data_raw$fake), aes(fake_similarity, fill = fake))+
#   theme_bw()+
#   geom_histogram(binwidth = 0.005)+
#   labs(y = "Review count",
#        x = "Mean fake review cosine similarity",
#        title = "Distribution of fake vs genuine using fake review cosine similarity")
# # On a per review basis, what is the average cosine similarity between a review and
# # all other fake reviews. On the middle of the graph, close to 0, there is a
# # higher amount of genuine reviews, meaning they have no cosine similarity with 
# # the fake reviews. The hypothesis that in general fake reviews should have 
# # higher cosine similarity on average with other fake reviews then they do with 
# # genuine reviews is true. Vice-versa is also true. 


# Addition of LIWC --------------------------------------------------------

liwc_results_train_data <- 
  (read.csv("output/liwc2015_results.csv", dec = ",") %>% 
     as_tibble() %>% 
     rename(review_ID = A,
            review = B))[-1,] %>% 
  mutate(review_ID = as.numeric(review_ID)) %>% 
  filter(review_ID %in% train_data_raw$review_ID) %>% 
  select(review_ID, WC, WPS, Authentic, Analytic,  Tone, Exclam, #  Linguistic processes
           function., ppron, i, we, shehe, they, adj, # Function words
           affect, posemo, negemo, # Psychological processes - emotional
           social, family, friend, female, male, # Psychological processes - social
           cogproc, insight, cause, discrep, tentat, certain, differ, # Psychological processes - cognitive
           time, focuspast, focuspresent, focusfuture, space, relativ, # Psychological processes - time and space
           percept, see, hear, feel, # Psychological processes - perceptual
           bio, body, health, sexual, ingest, # Psychological processes - biological
           work, leisure, home, money, relig) # Personal concerns

# liwc only
train_data_liwc <- 
  data.frame(review_ID = train_data_raw$review_ID,
             fake = train_data_raw$fake) %>% 
  inner_join(liwc_results_train_data, by = "review_ID") %>% 
  select(-review_ID)

# ngrams with liwc
ngrams_train_liwc <- 
  data.frame(review_ID = train_data_raw$review_ID,
             fake = train_data_raw$fake,
             ngrams_train_svd) %>% 
  inner_join(liwc_results_train_data, by = "review_ID") %>% 
  select(-review_ID)

# ngrams only
train_data_ngrams <- 
data.frame(fake = train_data_raw$fake,
           ngrams_train_svd)

# Fit the model -----------------------------------------------------------

train_data <- ngrams_train_liwc

# create stratified folds for 10-fold cross validation repeated 
# 3 times (30 random stratified samples)
cv_folds <- createMultiFolds(train_data_raw$fake, k = 10, times = 3)

cv_control <- trainControl(method = "repeatedcv", number = 10,
                           repeats = 3, index = cv_folds,
                           savePredictions = "final",
                           classProbs = TRUE)

# allow for working on x logical cores (working on all OS)
cl <- makeCluster(detectCores()-1, type = "SOCK")
doSNOW::registerDoSNOW(cl)

# Next, use Random Forest with the default of 500 trees and allow caret to try 7 
# different values of mtry to find the mtry value that gives the best result.
# RF for both ngrams and liwc
randomforest_res <- train(make.names(fake) ~ ., data = train_data, method = "rf",
                          trControl = cv_control, tuneLength = 7, importance = TRUE)

# GLM with ngrams and liwc
cv_control <- trainControl(method = "cv", 
                           savePredictions = "final",
                           classProbs = TRUE)
glm_res <- train(make.names(fake) ~ ., data = train_data, method = "glm", family = "binomial",
                 trControl = cv_control)

# RF only for ngrams
train_data <- train_data_ngrams
cv_folds <- createMultiFolds(train_data_raw$fake, k = 10, times = 3)
cv_control <- trainControl(method = "repeatedcv", number = 10,
                           repeats = 3, index = cv_folds,
                           savePredictions = "final",
                           classProbs = TRUE)
randomforest_res_ngrams <- train(make.names(fake) ~ ., data = train_data, method = "rf",
                                 trControl = cv_control, tuneLength = 7, importance = TRUE)

#RF only for liwc
train_data <- train_data_liwc
cv_folds <- createMultiFolds(train_data_raw$fake, k = 10, times = 3)
cv_control <- trainControl(method = "repeatedcv", number = 10,
                           repeats = 3, index = cv_folds,
                           savePredictions = "final",
                           classProbs = TRUE)
randomforest_res_liwc <- train(make.names(fake) ~ ., data = train_data, method = "rf",
                               trControl = cv_control, tuneLength = 7, importance = TRUE)




# Processing is done, stop cluster.
stopCluster(cl)

# info about the model
randomforest_res

confusionMatrix(train_data$fake, 
                # when I use classProbs at cv_control to be able to use evalm,
                # I need to use make.names(fake) when I fit the model. It adds
                # a dot at the end of each T or F, so I take it out for this
                # function to work.
                as.factor(str_replace_all(randomforest_res$finalModel$predicted, "\\.", "")))

randomForest::varImpPlot(randomforest_res$finalModel)
# On the last set of info, keep an eye on what is referenced and what is
# predicted. Tagging a genuine review as fake is worse than tagging a fake review
# as genuine. "Sensitivity" needs to be high.

# Accuracy of the model with uni-gram and 1k observations is, at the moment, 74%
# Most important variables are fake_similarity (maybe a sign of overfitting), the
# use of the first person pronouns and the time variables.



# get even more info about the model, like the receiver operating characteristic 
# curve. I use this measure because I want to decrease false positives, even if 
# means it will rise my false negatives. In this academic context, a genuine 
# review classified as fake is more hurtful than the a fake review classified 
# as genuine. the ROC will help choose a threshold.
eval <- MLeval::evalm(list(randomforest_res, randomforest_res_ngrams, 
                           randomforest_res_liwc, glm_res), 
                      gnames = c("rf", "rf_ngrams", "rf_liwc", "glm"))

eval$roc
eval$prg
eval$cc


# Prepare the test data ---------------------------------------------------

ngrams_test_dfm <-  
  #tokenization
  tokens(test_data_raw$review, what = "word", remove_punct = T, 
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
  dfm_trim(min_docfreq = 1, min_termfreq = 2)


# make sure train and test data fit it in the same vector space, and convert
# it to matrix for next steps
ngrams_test <- 
  dfm_select(ngrams_test_dfm, pattern = ngrams_train_dfm,
             selection = "keep") %>% 
  as.matrix()


# projecting the test data into the same tf-idf vector space of the training data
# normalize all documents with term frequency
ngrams_test_tf <- apply(ngrams_test, 1, 
                          function(row) row/sum(row))

# calculate TF-IDF with the training data's idf, to keep same vector space
ngrams_test_tfidf <-  apply(ngrams_test_tf, 2, 
                              function(x, idf) x * idf, idf = ngrams_train_idf)

# transpose the matrix
ngrams_test_tfidf <- t(ngrams_test_tfidf)

# check and fix incomplete cases
incomplete_cases <- which(!complete.cases(ngrams_test_tfidf))
ngrams_test_tfidf[incomplete_cases,] <- rep(0.0, ncol(ngrams_test_tfidf))

# double check
dim(ngrams_train_tfidf) - dim(ngrams_test_tfidf) # 0 = same nb of columns


# the test data is in the same tf-idf vector space as the training data, so we
# can project it with the same svd (prepare higher in the script with the
# training data)
ngrams_test_svd <- t(sigma_inverse * u_transpose %*% t(ngrams_test_tfidf))


# create the test data frame and calculate fake_similarity (# now because it led
# to overfitting)
ngrams_test_svd <- as_tibble(ngrams_test_svd)
# ngrams_test_similarities <- rbind(ngrams_test_svd, ngrams_train_svd[fake_indexes,])
# ngrams_test_similarities <- lsa::cosine(t(ngrams_test_similarities))
# 
# ngrams_test_svd <- 
# ngrams_test_svd %>% 
#   mutate(fake_similarity = as.numeric(0))
# 
# fake_cols_train_data <- (nrow(ngrams_test_svd) + 1):ncol(ngrams_test_similarities)
# 
# for(i in 1:nrow(ngrams_test_svd)) {
#   # The following line has the bug fix.
#   ngrams_test_svd$fake_similarity[i] <- mean(ngrams_test_similarities[i, fake_cols_train_data])  
# }
# 
# 
# # if there are issues as result of stopword removal or something, fix it to 0.
# ngrams_test_svd$fake_similarity[!is.finite(ngrams_test_svd$fake_similarity)] <- 0


# addition of liwc
liwc_results_test_data <- 
  (read.csv("output/liwc2015_results.csv", dec = ",") %>% 
     as_tibble() %>% 
     rename(review_ID = A,
            review = B))[-1,] %>% 
  mutate(review_ID = as.numeric(review_ID)) %>% 
  filter(review_ID %in% test_data_raw$review_ID) %>% 
  select(review_ID, WC, WPS, Authentic, Analytic,  Tone, Exclam, #  Linguistic processes
         function., ppron, i, we, shehe, they, adj, # Function words
         affect, posemo, negemo, # Psychological processes - emotional
         social, family, friend, female, male, # Psychological processes - social
         cogproc, insight, cause, discrep, tentat, certain, differ, # Psychological processes - cognitive
         time, focuspast, focuspresent, focusfuture, space, relativ, # Psychological processes - time and space
         percept, see, hear, feel, # Psychological processes - perceptual
         bio, body, health, sexual, ingest, # Psychological processes - biological
         work, leisure, home, money, relig) # Personal concerns

ngrams_test_liwc <- 
  data.frame(review_ID = test_data_raw$review_ID,
             fake = test_data_raw$fake,
             ngrams_test_svd) %>% 
  inner_join(liwc_results_test_data, by = "review_ID") %>% 
  select(-review_ID)

# make predictions on the test data set using the model
test_data <- ngrams_test_liwc

preds <- predict(randomforest_res, test_data)

# take a look at the results
confusionMatrix(as.factor(str_replace_all(preds, "\\.", "")), test_data$fake)

# Without the fake_similarity feature, accuracy is 66% with unigrams and LIWC,
# and 69% for sensitivity. With bigrams, 66% accuracy and 65% sensitivity. In
# our cases, I think we should favorize unigrams.


# I can change the threshold value to favor sensitivity over specificity (Less
# genuine predicted as fake, but more fake predicted as genuine)
test_result <- 
  test_data %>% 
  modelr::add_predictions(randomforest_res, type = "prob") %>%
  as_tibble() %>% 
  mutate(predicted_fake = ifelse(pred$TRUE. > 0.75, TRUE, FALSE)) %>% 
  select(fake, predicted_fake)

confusionMatrix(as.factor(test_result$predicted_fake), test_result$fake)

test_result <- 
  test_data %>% 
  modelr::add_predictions(randomforest_res_liwc, type = "prob") %>%
  as_tibble() %>% 
  mutate(predicted_fake = ifelse(pred$TRUE. > 0.6, TRUE, FALSE)) %>% 
  select(fake, predicted_fake)

confusionMatrix(as.factor(test_result$predicted_fake), test_result$fake)

test_result <- 
  test_data %>% 
  modelr::add_predictions(randomforest_res_ngrams, type = "prob") %>%
  as_tibble() %>% 
  mutate(predicted_fake = ifelse(pred$TRUE. > 0.5, TRUE, FALSE)) %>% 
  select(fake, predicted_fake)

confusionMatrix(as.factor(test_result$predicted_fake), test_result$fake)

test_result <- 
  test_data %>% 
  modelr::add_predictions(glm_res, type = "prob") %>%
  as_tibble() %>% 
  mutate(predicted_fake = ifelse(pred$TRUE. > 0.5, TRUE, FALSE)) %>% 
  select(fake, predicted_fake)

confusionMatrix(as.factor(test_result$predicted_fake), test_result$fake)

