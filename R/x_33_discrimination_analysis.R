source("R/01_source.R")
library(caret)
library(tidytext)
library(quanteda)


# Load data ---------------------------------------------------------------

classified_texts_import <- qread("output/classified_texts.qs")

classified_texts <- 
  rbind(classified_texts_import[classified_texts_import$fake == T,],
        sample_n(classified_texts_import[classified_texts_import$fake == F,], 
                 size = nrow(classified_texts_import[classified_texts_import$fake == T,])*1))

rm(classified_texts_import)


# Preparation for bigram analysis -----------------------------------------


# Quanteda - ngrams creation ---------------------------------------------

# create ngrams in dfm to match it later with test data
ngrams_train_dfm <-  
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
  dfm() 

ngrams_dfm_trimed <- 
  ngrams_train_dfm %>% 
  # trim because it's too large, with my memory, to convert in matrix! 
  dfm_trim(min_docfreq = 1, min_termfreq = 50)

# Look at sparsity (% of cells being 0). If it's too high, ultimately I won't
# be able to convert my dfm to a matrix
sparsity(ngrams_dfm_trimed)

# convert it to matrix for next steps
ngrams <- 
  ngrams_dfm_trimed %>%
  as.matrix()

ncol(ngrams)

# Free up space in my memory
rm(classified_texts_import, ngrams_train_dfm)

# Term Frequency - Inverse Document Frequency (TF-IDF) ---------
# Since some reviews are longer than others, "term frequency" will normalize all
# reviews to be length independant. We also accounts for the frequency of ngrams
# appearing in virtually every documents with the IDF calculation. We multiply
# both TF and IDF for each cell in the matrix.

# normalize all documents with term frequency
ngrams_tf <- apply(ngrams, 1, 
                         function(row) row/sum(row))

# calculate the inverse document frequency
ngrams_idf <- apply(ngrams, 2, 
                          function(col) log10(length(col) / length(which(col > 0))))

# Free up space in my memory
rm(ngrams)

# calculate TF-IDF 
ngrams_tfidf <-  apply(ngrams_tf, 2, 
                             function(x, idf) x * idf, idf = ngrams_idf)

# Free up space in my memory
rm(ngrams_tf)

# transpose the matrix
ngrams_tfidf <- t(ngrams_tfidf)

# check and fix incomplete cases
incomplete_cases <- which(!complete.cases(ngrams_tfidf))
ngrams_tfidf[incomplete_cases,] <- rep(0.0, ncol(ngrams_tfidf))


# Singular value decomposition (SVD) --------------------------------------

# reduce dimensionality so that it is more memory-efficient. Find a
# few approximate singular values and corresponding singular vectors of the
# matrix. The new 300 columns it returns are the single most significant
# representations of the data extracted out of the TFIDF matrix.
ngrams_svd <- irlba::irlba(t(ngrams_tfidf), nv = 300, maxit = 600)

# these will be useful to convert the test data in the same svd semantic space
# as the training data. we use one row of the training data that's already been
# transformed by tf-idf.
sigma_inverse <- 1 / ngrams_svd$d
u_transpose <- t(ngrams_svd$u)
review <- ngrams_tfidf[1,]
review_hat <- sigma_inverse * u_transpose %*% review

# only take the relevant part, for us, of the value
ngrams_svd <- as_tibble(ngrams_svd$v)


# Addition of LIWC --------------------------------------------------------

liwc_results_train_data <- 
  qread("output/liwc2015_results.qs") %>% 
  filter(review_ID %in% classified_texts$review_ID)

# liwc only
train_data_liwc <- 
  data.frame(review_ID = classified_texts$review_ID,
             fake = classified_texts$fake) %>% 
  inner_join(select(classified_texts, -review), by = "review_ID") %>% 
  select(-review_ID)

# ngrams with liwc
ngrams_liwc <- 
  data.frame(review_ID = classified_texts$review_ID,
             fake = classified_texts$fake,
             ngrams_svd) %>% 
  inner_join(liwc_results_train_data, by = "review_ID") %>% 
  select(-review_ID)

# ngrams only
train_data_ngrams <- 
  data.frame(fake = classified_texts$fake,
             ngrams_svd)

# Fit the model -----------------------------------------------------------

training.samples <- ngrams_liwc$fake %>%
  createDataPartition(p = 0.8, list = FALSE)
train.data <- ngrams_liwc[training.samples, ]
test.data <- ngrams_liwc[-training.samples, ]


# Fit the model
model <- MASS::lda(fake~., data = train.data)

qsave(model, file = "output/model.qs")

test.data_2 <-
  rbind(sample_n(test.data[test.data$fake == T,],
                 size = nrow(test.data[test.data$fake == T,])/1),
        sample_n(test.data[test.data$fake == F,],
                 size = nrow(test.data[test.data$fake == F,])/1))

# Make predictions
predictions <- model %>% predict(test.data_2)
# Model accuracy
mean(predictions$class==test.data_2$fake)
# 
# 
# # info about the model
# # model
plot(model)
# 
lda.data <- cbind(test.data_2, predict(model, test.data_2)$x)
lda.data %>%
  # mutate(LD1 = abs(LD1) * LD1) %>% # equivalent of ^2 but keeping sign
  ggplot(aes(LD1)) +
  geom_density(aes(fill = fake), alpha = 0.4)+
  scale_fill_manual(values=c("#FFFF00", "#00FFFF")) #+
  # xlim(-7,7)

cbind(fake = as.logical(test.data_2$fake),
      predict = as.logical(predictions$class)) %>%
  as_tibble() %>%
  dplyr::count(fake, predict) %>%
  dplyr::group_by(fake) %>%
  dplyr::mutate(percent = n/sum(n))

confusionMatrix(predictions$class, test.data_2$fake)


rm(liwc_results_train_data, ngrams_liwc, ngrams_svd, train_data_liwc, 
   train_data_ngrams, train.data)
# # PCA test ----------------------------------------------------------------
# train.pca <- prcomp(train.data[,c(2:ncol(train.data))], center = TRUE, scale. = TRUE)
# train.pca
# str(train.pca)
# 
# # library(devtools)
# library(ggbiplot)
# ggbiplot(train.pca, ellipse=TRUE, var.axes=FALSE, groups=train.data$fake)



# Apply to entire population of reviews -----------------------------------

review_text <- qread("output/m_review_text.qs")
liwc_results <- qread("output/liwc2015_results.qs")


apply_model <- function(review_text_df, min_termfreq_dfm_trim = 25){
  
  ngrams_pop_dfm <-  
    #tokenization
    tokens(review_text_df$review, what = "word", remove_punct = T, 
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
    dfm() 
  
  ngrams_pop_dfm_trimed <- 
    ngrams_pop_dfm %>% 
    # trim if needed
    dfm_trim(min_docfreq = 1, min_termfreq = min_termfreq_dfm_trim)

  # make sure train and pop data fit it in the same vector space, and convert
  # it to matrix for next steps
  ngrams_pop <- 
    dfm_select(ngrams_pop_dfm_trimed, pattern = ngrams_dfm_trimed,
               selection = "keep") %>% 
    as.matrix()
  
  # projecting the pop data into the same tf-idf vector space of the training data
  # normalize all documents with term frequency
  ngrams_pop_tf <- apply(ngrams_pop, 1, 
                         function(row) row/sum(row))
  
  # calculate TF-IDF with the training data's idf, to keep same vector space
  ngrams_pop_tfidf <-  apply(ngrams_pop_tf, 2, 
                             function(x, idf) x * idf, idf = ngrams_idf)
  
  # transpose the matrix
  ngrams_pop_tfidf <- t(ngrams_pop_tfidf)
  
  # check and fix incomplete cases
  incomplete_cases <- which(!complete.cases(ngrams_pop_tfidf))
  ngrams_pop_tfidf[incomplete_cases,] <- rep(0.0, ncol(ngrams_pop_tfidf))
  
  # double check
  # dim(ngrams_tfidf) - dim(ngrams_pop_tfidf) # second number = 0 means same nb of columns
  
  # the pop data is in the same tf-idf vector space as the training data, so we
  # can project it with the same svd (prepare higher in the script with the
  # training data)
  ngrams_pop_svd <- t(sigma_inverse * u_transpose %*% t(ngrams_pop_tfidf))
  
  
  # create the pop data frame and calculate fake_similarity (# now because it led
  # to overfitting)
  ngrams_pop_svd <- as_tibble(ngrams_pop_svd)

  
  # addition of liwc
  liwc_results_test_data <- 
    liwc_results %>% 
    filter(review_ID %in% review_text_df$review_ID)
  
  ngrams_pop_liwc <- 
    data.frame(review_ID = review_text_df$review_ID,
               ngrams_pop_svd) %>% 
    inner_join(liwc_results_test_data, by = "review_ID") %>% 
    select(-review_ID)
  
  # predict
  predictions <- model %>% predict(ngrams_pop_liwc)
  
  review_text_df <- 
    review_text_df %>% 
    inner_join(predictions %>% as.tibble() %>% select(x) %>% 
                 cbind(select(review_text_df, review_ID)), by= "review_ID")
  
  review_text_df
  
  #OU get un vecteur de score seulement. ensuite on va juste le rajouter en colonne, il sera déjà en ordre sur les reviews_text
  
}


library(future)
library(foreach)
library(progressr)
library(doFuture)
registerDoFuture()
plan(multisession)

x <- nrow(review_text)
batch_size <- 4

with_progress({
  
  pb <- progressor(x)
  handlers(list(
    handler_progress(
      format   = ":spin :current/:total [:bar] :percent in :elapsed ETA: :eta",
      width    = 60,
      complete = "="
    )
  ))
  
  total_iterations <- ceiling(x / batch_size)
  
  iteration <- 1
  
  results <- vector("list", total_iterations)
  
  while (iteration <= total_iterations) {
    
    results[[iteration]] <- 
      
      foreach(i = ((iteration - 1) * batch_size + 1):min(iteration * batch_size, x), .combine = c) %dopar% {
        pb()
        apply_model(review_text, 25)
      }  
    
    iteration <- iteration + 1
    
    # qsave(results, file = "output/results_model.qs")
  }
})


results