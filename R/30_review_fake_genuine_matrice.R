source("R/01_source.R")
library(caret)

load("data/review_processed.Rdata")
load("data/str_processed.Rdata")


# Matrix creation --------------------------------------------------------

# # Filter out if not enough words for reliable analysis
review_text_more_words <-
review_text %>%
  filter(lengths(str_split(review, " ")) > 4)

# Keeping only the more recent version of review_user
review_user_fk <- 
  review_user %>% 
  arrange(desc(date)) %>% 
  distinct(user_ID, .keep_all = T) %>% 
  select(-date)

# Relevant info about properties
property_fk <- 
  property %>% 
  st_drop_geometry() %>% 
  select(property_ID, host_ID, created, old_host, all_PIDs)

# Matrix creation
matrice <- 
  review %>% 
  select(review_ID, date, user_ID, property_ID, city) %>% 
  inner_join(review_user_fk, by = "user_ID") %>% 
  select(review_ID, property_ID, date, user_ID, user_city, member_since, city) %>% 
  inner_join(property_fk, by = "property_ID") %>% 
  rename(review_date = date,
         property_city = city,
         property_created = created) %>% 
  mutate(user_ID = ifelse(!is.na(old_host) & user_ID == old_host, host_ID, user_ID), # if user_ID = old_host, take host_ID
         property_created = str_extract(property_created, "^.{7}"),
         review_date = str_extract(review_date, "^.{7}"),
         member_since = str_extract(member_since, "^.{7}"))


# Potential fake reviews regrading user behavior  -------------------------

#' RELEVANT VARIABLES TO DETECT FAKE REVIEWS :
#' A review let inside the first month of activity, user and host in the same
#' city, a user letting multiple reviews at the same hosts' properties, a user
#' letting reviews to a host inside his own network of hosts, exact same 
#' review let by same user

# letting more than 2 reviews at same host
multiple_reviews_same_host <- 
  matrice %>% 
  count(user_ID, host_ID, sort = T) %>% 
  filter(n>2) %>%
  inner_join(matrice, by = c("user_ID", "host_ID")) %>% 
  pull(review_ID)

# a user/network letting the same reviews at multiple places
same_review <- ### Maybe excluding one-word or two-words review a good idea?
  review_text_more_words %>% 
  count(user_ID, review, sort=T) %>% 
  filter(n>1) %>% 
  inner_join(review_text_more_words, by = c("user_ID", "review")) %>% 
  pull(review_ID)

# identifying which are the potential fake reviews vs genuine
fake_reviews <- 
  matrice %>%
  mutate(fake = F,
         fake = case_when(
           user_ID == host_ID ~ T, # User = Host
           user_city == property_city & property_created == review_date ~ T, # user_city = property_city & property_created = review_date
           review_ID %in% multiple_reviews_same_host ~ T,
           review_ID %in% same_review ~ T,
           F == F ~ F
         ))

fake_reviews <- 
  review_text_more_words %>% 
  filter(review_ID %in% !! (fake_reviews %>% filter(fake == T) %>% pull(review_ID))) %>% 
  select(review_ID, review) %>% 
  mutate(fake = T)


# Potential genuine reviews regarding user behavior -----------------------

#' RELEVANT VARIABLES TO DETECT GENUINE REVIEWS :
#' A review let after 6 months of listing creation, user and listing of different 
#' city, a user letting a single review to the host, unique review let by the
#' user, review at least 3 months after creation of account, 

one_review_per_host <- 
  matrice %>% 
  count(user_ID, host_ID, sort = T) %>% 
  filter(n == 1) %>%
  inner_join(matrice, by = c("user_ID", "host_ID")) %>% 
  pull(review_ID)

unique_review <- 
  review_text_more_words %>% 
  count(user_ID, review, sort=T) %>% 
  filter(n == 1) %>% 
  inner_join(review_text_more_words, by = c("user_ID", "review")) %>% 
  pull(review_ID)

genuine_reviews <- 
  matrice %>%
  mutate(genuine = F,
         genuine = case_when(
           user_ID != host_ID &
             user_city != property_city &
             ym(review_date) >= ym(property_created) + months(6) &
             ym(review_date) >= ym(member_since) + months(3) &
             review_ID %in% one_review_per_host  &
             review_ID %in% unique_review ~ T,
           F == F ~ F
         ))

genuine_reviews <- 
  review_text_more_words %>% 
  filter(review_ID %in% !! (genuine_reviews %>% filter(genuine == T) %>% pull(review_ID))) %>% 
  select(review_ID, review) %>% 
  mutate(fake = F)



# Prepare a statistical model --------------------------------------------

# Both fake and genuine text reviews in one dataframe
classified_texts <-
  rbind(fake_reviews, sample_n(genuine_reviews, nrow(fake_reviews)))

# # Try at using the LIWC dictionary in R directly
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

# Export all reviews
write_csv(select(review_text_more_words, review_ID, review), file = "data/review_text_more_words.csv")

# Import LIWC results
liwc_results <- 
  (read.csv("output/liwc2015_results.csv", dec = ",") %>% 
     as_tibble() %>% 
     rename(review_ID = A,
            review = B))[-1,] %>% 
  mutate(review_ID = as.numeric(review_ID)) %>% 
  filter(review_ID %in% review_text_more_words$review_ID)
  
# LIWC results for classified (fake vs genuine) texts
classified_texts <- 
liwc_results %>% 
  inner_join(select(classified_texts, review_ID, fake), by = "review_ID")

# Quickly see which variables vary the most when a review is fake vs genuine
classified_texts_diff <- 
  classified_texts %>% 
  group_by(fake) %>% 
  summarize(across(cols = everything(), .fns = mean, na.rm = T)) %>%
  ungroup() %>% 
  select(-c(review_ID, review, fake))

(classified_texts_diff %>% 
  add_row(classified_texts_diff[1,] - classified_texts_diff[2,]))[3,] %>% 
  pivot_longer(cols = everything(), names_to = "name", values_to = "value") %>% 
  filter(value != 0) %>% 
  mutate(value = abs(value)) %>% 
  arrange(desc(value)) %>% View
  

# Model testing -----------------------------------------------------------

# Split the data into training and test set
training_samples <-
  classified_texts$fake %>%
  createDataPartition(p = 0.8, list = FALSE)

train_data <- classified_texts[training_samples, ]
test_data <- classified_texts[-training_samples, ]

# Fit the model
model <- glm(fake ~ Authentic + Analytic + function. + Tone +
               affect + 
               adj +
               ppron + i + we + shehe + they +
               time + focuspast + focuspresent + focusfuture +
               space + relativ +
               work + leisure + home,
             data = classified_texts, family = binomial)

# summary(model)

# Test model
probabilities <- model %>% predict(test_data, type = "response")
predicted_classes <- ifelse(probabilities > 0.5, "TRUE", "FALSE")
mean(predicted_classes == test_data$fake)
# Outcome: 0.66. Pretty bad at the moment.


# Apply it to all reviews
classified_texts %>% 
  modelr::add_predictions(model, type = "response") %>%
  as_tibble() %>% 
  select(review_ID, review, pred, fake) %>% View


# Fit the model -----------------------------------------------------------
