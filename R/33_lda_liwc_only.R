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

liwc_results_train_data <- 
  (read.csv("output/liwc2015_results.csv", dec = ",") %>% 
     as_tibble() %>% 
     dplyr::rename(review_ID = A,
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

# liwc only
train_data_liwc <- 
  data.frame(review_ID = classified_texts$review_ID,
             fake = classified_texts$fake) %>% 
  inner_join(liwc_results_train_data, by = "review_ID") %>% 
  select(-review_ID)

# Fit the model -----------------------------------------------------------

training.samples <- train_data_liwc$fake %>%
  createDataPartition(p = 0.8, list = FALSE)
train.data <- train_data_liwc[training.samples, ]
test.data <- train_data_liwc[-training.samples, ]


# Fit the model
model <- MASS::lda(fake~., data = train.data)


test.data_2 <-
  rbind(sample_n(test.data[test.data$fake == T,],
                 size = nrow(test.data[test.data$fake == T,])/1),
        sample_n(test.data[test.data$fake == F,],
                 size = nrow(test.data[test.data$fake == F,])/1))


# Make predictions
predictions <- model %>% predict(test.data_2)
# Model accuracy
mean(predictions$class==test.data_2$fake)


# info about the model
# model
plot(model)

lda.data <- cbind(test.data_2, predict(model, test.data_2)$x)
ggplot(lda.data, aes(LD1)) +
  geom_density(aes(fill = fake), alpha = 0.4)+
  scale_fill_manual(values=c("#FFFF00", "#00FFFF"))

predictions <- 
predictions %>% 
  as.tibble() %>% 
  mutate(class_2 = ifelse(x > 1, T, F))

cbind(fake = as.logical(test.data_2$fake),
      predict = as.logical(predictions$class_2)) %>%
  as_tibble() %>%
  dplyr::count(fake, predict) %>%
  dplyr::group_by(fake) %>%
  dplyr::mutate(percent = n/sum(n))

confusionMatrix(predictions$class, test.data_2$fake)


# building roc curve
test.data.roc <- predict(model, newdata = test.data)
# Get the posteriors as a dataframe.
test.data.roc.posteriors <- as.data.frame(test.data.roc$posterior)
# Evaluate the model
pred <- ROCR::prediction(test.data.roc.posteriors[,2], test.data$fake)
roc.perf = ROCR::performance(pred, measure = "tpr", x.measure = "fpr")
auc.train <- ROCR::performance(pred, measure = "auc")
auc.train <- auc.train@y.values
plot(roc.perf)
abline(a=0, b= 1)
text(x = .25, y = .65 ,paste("AUC = ", round(auc.train[[1]],3), sep = ""))


model$scaling %>%
  as.tibble() %>%
  mutate(value = (model$scaling %>% rownames()),
         LD1 = abs(LD1)) %>% 
  arrange(LD1) %>% as.data.frame()


# rm(liwc_results_train_data, ngrams_liwc, ngrams_svd, train_data_liwc, 
#    train_data_ngrams, train.data)
# # PCA test ----------------------------------------------------------------
# train.pca <- prcomp(train.data[,c(2:ncol(train.data))], center = TRUE, scale. = TRUE)
# train.pca
# str(train.pca)
# 
# # library(devtools)
# library(ggbiplot)
# ggbiplot(train.pca, ellipse=TRUE, var.axes=FALSE, groups=train.data$fake)



# Apply to entire population of reviews -----------------------------------

load("output/review_processed.Rdata")
rm(review, review_user)

# divide in three for memory reasons

# addition of liwc
liwc_results_test_data <- 
  (read.csv("output/liwc2015_results.csv", dec = ",") %>% 
     as_tibble() %>% 
     rename(review_ID = A,
            review = B))[-1,] %>% 
  mutate(review_ID = as.numeric(review_ID)) %>% 
  filter(review_ID %in% review_text$review_ID) %>% 
  select(review_ID, WC, WPS, Authentic, Analytic,  Tone, Exclam, #  Linguistic processes
         function., ppron, i, we, shehe, they, adj, # Function words
         affect, posemo, negemo, # Psychological processes - emotional
         social, family, friend, female, male, # Psychological processes - social
         cogproc, insight, cause, discrep, tentat, certain, differ, # Psychological processes - cognitive
         time, focuspast, focuspresent, focusfuture, space, relativ, # Psychological processes - time and space
         percept, see, hear, feel, # Psychological processes - perceptual
         bio, body, health, sexual, ingest, # Psychological processes - biological
         work, leisure, home, money, relig) %>% # Personal concerns
  select(-review_ID)

# predict
predictions <- model %>% predict(liwc_results_test_data)

review_text <- 
  review_text %>% 
  inner_join(predictions %>% as.tibble() %>% select(x) %>% 
               cbind(select(review_text, review_ID)), by= "review_ID")


review_text_pred_liwc <- review_text

qsavem(review_text_pred_liwc, file = "output/review_text_pred_liwc.qs")




# LOOK AT SCORE PER REVIEWER INSTEAD OF LISTINGS AND REVIEWEE!