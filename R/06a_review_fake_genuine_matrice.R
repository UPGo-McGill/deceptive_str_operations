source("R/01_source.R")


# Load data ---------------------------------------------------------------

property <- qread("output/property.qs")

# Load the reviews that have been let only to properties under analysis
review <- qread("output/m_review.qs")
review_text <- qread("output/m_review_text.qs")
review_user <- qread("output/m_review_user.qs")


# Filter out reviews let in other geographies -----------------------------

review <- 
  review %>% 
  filter(property_ID %in% unique(c(property$property_ID, unlist(property$all_PIDs))))

review_text <- 
  review_text %>% 
  filter(review_ID %in% !! review$review_ID)

review_user <- 
  review_user %>% 
  filter(user_ID %in% review$user_ID)

# Matrix creation --------------------------------------------------------

# Keeping only the more recent version of review_user
review_user_fk <- 
  review_user %>% 
  arrange(desc(date)) %>% 
  distinct(user_ID, .keep_all = T) %>% 
  select(-date)

# Relevant info about properties
property_fk <- 
  property %>% 
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
  # matrice %>%
  # count(user_ID, host_ID, sort = T) %>%
  # filter(n>2) %>%
  # inner_join(matrice, by = c("user_ID", "host_ID")) %>%
  # pull(review_ID)
  matrice %>%
  group_by(user_ID, host_ID) %>%
  filter(n() > 2) %>%
  pull(review_ID)

# a user/network letting the same reviews at multiple places
same_review <-
  # review_text %>% 
  # count(user_ID, review, sort=T) %>% 
  # filter(n>2) %>% # There seems to have an issue on Airbnb's side with double up
  # # reviews. 
  # inner_join(review_text, by = c("user_ID", "review")) %>%
  # pull(review_ID)
  review_text %>% 
  group_by(user_ID, review) %>% 
  filter(n()>2) %>% # There seems to have an issue on Airbnb's side with double up
  # reviews. 
  pull(review_ID)


# identifying which are the potential fake reviews vs genuine
fake_reviews_uc <- 
  matrice %>%
  mutate(fake = F,
         fake = case_when(
           user_ID == host_ID ~ T, # User = Host
           user_city == property_city & property_created == review_date ~ T,
           review_ID %in% multiple_reviews_same_host ~ T,
           review_ID %in% same_review ~ T,
           F == F ~ F
         ))

fake_reviews_uc <- 
  review_text %>% 
  filter(review_ID %in% !! (fake_reviews_uc %>% filter(fake == T) %>% pull(review_ID))) %>% 
  select(review_ID, review) %>% 
  mutate(fake = T)


# Potential genuine reviews regarding user behavior -----------------------

#' RELEVANT VARIABLES TO DETECT GENUINE REVIEWS :
#' A review let after 6 months of listing creation, user and listing of different 
#' city, a user letting a single review to the host, unique review let by the
#' user, review at least 3 months after creation of account, 

one_review_per_host <- 
  # matrice %>% 
  # count(user_ID, host_ID, sort = T) %>% 
  # filter(n == 1) %>%
  # inner_join(matrice, by = c("user_ID", "host_ID")) %>% 
  # pull(review_ID)
  # INSTEAD
  matrice %>% 
  group_by(user_ID, host_ID) %>% 
  filter(n() == 1) %>%
  pull(review_ID)


unique_review <- 
  # review_text %>% 
  # count(review, sort=T) %>% 
  # filter(n == 1) %>% 
  # inner_join(review_text, by = c("review")) %>% 
  # pull(review_ID)
  # INSTEAD:
  review_text %>% 
  group_by(review) %>% 
  filter(n() == 1) %>% 
  pull(review_ID)

genuine_reviews <- 
  matrice %>%
  filter(!review_ID %in% fake_reviews_uc$review_ID) %>% 
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
  review_text %>% 
  filter(review_ID %in% !! (genuine_reviews %>% filter(genuine == T) %>% pull(review_ID))) %>% 
  select(review_ID, review) %>% 
  mutate(fake = F)



# Prepare for statistical model -------------------------------------------

# Both fake and genuine text reviews in one dataframe, using random undersampling. 
# I should think about trying oversampling, or cost-sensitive classification
classified_texts <-
  rbind(fake_reviews_uc, genuine_reviews) %>%  #sample_n(genuine_reviews, nrow(fake_reviews_uc)*3)) %>%  
  # My issue here is that I have way more
  # "genuine" than "fake" reviews. I keep a 90%/10% ratio
  mutate(fake = as.factor(fake)) %>% 
  distinct(review, .keep_all=T) # duplicated reviews are worth more if we let them in the training dataset


# Save --------------------------------------------------------------------

qsave(classified_texts, file = "output/classified_texts.qs")
qsave(fake_reviews_uc, file = "output/fake_reviews_uc.qs")
