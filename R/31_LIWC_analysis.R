source("R/01_source.R")
library(caret)
library(tidytext)
library(quanteda)


# Load libraries ----------------------------------------------------------

load("output/review_processed.Rdata")
load("output/classified_texts.Rdata")


# Try at using the LIWC dictionary in R directly --------------------------
# 
# # devtools::install_github("kbenoit/quanteda.dictionaries")
# library(quanteda.dictionaries)
# 
# dict_liwc_2015 <- dictionary(file = "dict/LIWC2015_English_Flat.dic",
#                              format = "LIWC")
# 
# liwc_results <-
#   liwcalike(classify_texts$review, dictionary = "dict_liwc_2015") #%>%
#   mutate(docname = classify_texts$review_ID)

# Since the latter is not working, probably deprecated, the text analysis is
# done directly in the LIWC2015 software
# Send every review to the software


# Export and import reviews / LIWC results  -------------------------------

write_csv(select(review_text, review_ID, review), file = "data/review_texts.csv")

# Import LIWC results
liwc_results <- 
  (read.csv("output/liwc2015_results.csv", dec = ",") %>% 
     as_tibble() %>% 
     rename(review_ID = A,
            review = B))[-1,] %>% 
  mutate(review_ID = as.numeric(review_ID)) %>% 
  filter(review_ID %in% review_text$review_ID)

# LIWC results for classified (fake vs genuine) texts
classified_texts_liwc <- 
  liwc_results %>% 
  inner_join(select(classified_texts, review_ID, fake), by = "review_ID")

# Quickly see which variables vary the most when a review is fake vs genuine
classified_texts_liwc_diff <- 
  classified_texts_liwc %>% 
  group_by(fake) %>% 
  summarize(across(cols = everything(), .fns = mean, na.rm = T)) %>%
  ungroup() %>% 
  select(-c(review_ID, review, fake))

(classified_texts_liwc_diff %>% 
    add_row(classified_texts_liwc_diff[1,] - classified_texts_liwc_diff[2,]))[3,] %>% 
  pivot_longer(cols = everything(), names_to = "name", values_to = "value") %>% 
  filter(value != 0) %>% 
  mutate(value = abs(value)) %>% 
  arrange(value) %>% 
  print(n = 100)


# LIWC nodel testing ------------------------------------------------------

# Split the data into training and test set
training_samples <-
  classified_texts_liwc$fake %>%
  createDataPartition(p = 0.7, list = FALSE)

train_data <- classified_texts_liwc[training_samples, ]
test_data <- classified_texts_liwc[-training_samples, ]

prop.table(table(train_data$fake))
# createDataPartition keeps the split proportion between fake and genuine 
# reviews. However we don't know what is the real proportion!

# Fit the model
model_testing <- glm(fake ~ WC + WPS + Authentic + Analytic +  Tone + Exclam + #  Linguistic processes
                       function. + ppron + i + we + shehe + they + adj + # Function words
                       affect + posemo + negemo + # Psychological processes - emotional
                       social + family + friend + female + male + # Psychological processes - social
                       cogproc + insight + cause + discrep + tentat + certain + differ + # Psychological processes - cognitive
                       time + focuspast + focuspresent + focusfuture + space + relativ + # Psychological processes - time and space
                       percept + see + hear + feel + # Psychological processes - perceptual
                       bio + body + health + sexual + ingest + # Psychological processes - biological
                       work + leisure + home + money + relig, # Personal concerns
                     data = train_data, family = binomial)

# summary(model_testing)

# Test model
probabilities <- model_testing %>% predict(test_data, type = "response")
predicted_classes <- ifelse(probabilities > 0.5, TRUE, FALSE)
mean(predicted_classes == test_data$fake)
# Outcome: 0.97,  because my sample size for fake review is too small 
# compared to the sample size of genuine reviews. Virtually every review in the
# test_data are predicted to be genuine, and since the vast majority of reviews in 
# my sample are genuine, then the outcome of the model is very high: it is only 
# wrong in the case where a review was fake in the sample, but is predicted
# as genuine by the model like every other review. But how much is it okay to 
# intentionally shrink my "genuine" sample size, if I don't know the real 
# proportion of "fake" vs "genuine" reviews? If I shrink my genuine sample size
# to the same number of observations than the sample I have of fake reviews, then
# I come down to an outcome of 0.67%, and the model probably overestimates the 
# number of fake reviews if applied to the entire population.

# Outcome per category (fake vs genuine)
tibble(fake = test_data$fake, predicted_class = predicted_classes) %>% 
  mutate(accurate = ifelse(fake == predicted_class, T, F)) %>% 
  group_by(fake) %>% 
  summarize(outcome = mean(accurate))

# When the full sample of genuine review is counted, the model is virtually never
# wrong for genuine review, but is virtually all the time wrong for fake reviews.

# If I downsize the size of the sample of the genuine reviews for the same size
# of the sample of fake reviews, then the model is right 70% of the time for 
# genuine review, and right 64% of the time for fake review.


# Fit and apply model for all data ----------------------------------------

model <- glm(fake ~ WC + WPS + Authentic + Analytic +  Tone + Exclam + #  Linguistic processes
               function. + ppron + i + we + shehe + they + adj + # Function words
               affect + posemo + negemo + # Psychological processes - emotional
               social + family + friend + female + male + # Psychological processes - social
               cogproc + insight + cause + discrep + tentat + certain + differ + # Psychological processes - cognitive
               time + focuspast + focuspresent + focusfuture + space + relativ + # Psychological processes - time and space
               percept + see + hear + feel + # Psychological processes - perceptual
               bio + body + health + sexual + ingest + # Psychological processes - biological
               work + leisure + home + money + relig, # Personal concerns
             data = classified_texts_liwc, family = binomial)

liwc_results %>% 
  modelr::add_predictions(model, type = "response") %>%
  as_tibble() %>% 
  select(review_ID, review, pred) %>%
  mutate(predicted_fake = ifelse(pred > 0.5, TRUE, FALSE)) %>% 
  count(predicted_fake)
# The model predicts that a third of the review population are fake. Unlikely!