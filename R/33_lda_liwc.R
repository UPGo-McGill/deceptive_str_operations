source("R/01_source.R")
library(caret)
# library(tidytext)
# library(quanteda)


# Load data ---------------------------------------------------------------

classified_texts_import <- qread("output/classified_texts.qs")
liwc2015_results <- qread("output/liwc2015_results.qs")

# Keep only one review of those that are not unique, so that they don't take
# too much space in the model
classified_texts_import <- 
classified_texts_import %>% 
  distinct(review, .keep_all=T)

classified_texts <- 
  rbind(classified_texts_import[classified_texts_import$fake == T,],
        sample_n(classified_texts_import[classified_texts_import$fake == F,], 
                 size = nrow(classified_texts_import[classified_texts_import$fake == T,])*1))

liwc_results_train_data <- 
  liwc2015_results %>% 
  filter(review_ID %in% classified_texts$review_ID)

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


# Make predictions on the test data
predictions <- model %>% predict(test.data)
# Model accuracy
mean(predictions$class==test.data$fake)


# info about the model
plot(model)

lda.data <- cbind(test.data, predict(model, test.data)$x)
ggplot(lda.data, aes(LD1)) +
  geom_density(aes(fill = fake), alpha = 0.4)+
  scale_fill_manual(values=c("#FFFF00", "#00FFFF"))

predictions <- 
predictions %>% 
  as.tibble() %>% 
  mutate(class_2 = ifelse(x > 0, T, F))

cbind(fake = as.logical(test.data$fake),
      predict = as.logical(predictions$class_2)) %>%
  as_tibble() %>%
  dplyr::count(fake, predict) %>%
  dplyr::group_by(fake) %>%
  dplyr::mutate(percent = n/sum(n))

confusionMatrix(predictions$class, test.data$fake)


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

# qsavem(training.samples, train.data, test.data, model, 
#        predictions, lda.data, file = "output/liwc_lda_model.qsm")


# Apply to entire population of reviews -----------------------------------

review_text <- qread("output/m_review_text.qs")

# addition of liwc
liwc_results_test_data <- 
  liwc2015_results %>% 
  # filter(review_ID %in% review_text$review_ID) %>%
  select(-review_ID)

# predict
predictions <- model %>% predict(liwc_results_test_data)

review_text_pred_liwc <- 
  review_text %>% 
  inner_join(predictions %>% as.tibble() %>% select(x) %>% 
               cbind(select(review_text, review_ID)), by= "review_ID")

review_text_pred_liwc <- 
review_text_pred_liwc %>% 
  select(names(review_text_pred_liwc)[1:5]) %>% 
  mutate(chance_fake = as.vector(pull(review_text_pred_liwc[,6])))

qsave(review_text_pred_liwc, file = "output/review_text_pred_liwc.qs")