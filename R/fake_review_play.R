property <- qread(here("output", "property.qs"), parallel::detectCores())
review <- qread(here("output", "m_review.qs"))
review_text_pred <- qread(here("output","review_text_pred_liwc.qs"))

review <- 
  review %>% 
  filter(property_ID %in% property$property_ID)

per_user <- 
review_text_pred %>% 
  group_by(user_ID) %>% 
  summarize(mean = mean(chance_fake), sd = sd(chance_fake), nb_reviews = n())

fake_reviewers <- 
per_user %>% 
  filter(mean-sd >= 0, nb_reviews >= 5) %>% 
  pull(user_ID)

property_fake_reviewed <- 
review %>% 
  filter(user_ID %in% fake_reviewers) %>% 
  group_by(property_ID) %>% 
  summarize(nb_fake_review_from_fake_reviewers = n()) %>% 
  inner_join(property)

property_fake_reviewed %>% 
  count(city) %>% 
  arrange(-n)

property_fake_reviewed %>% 
  count(FREH) 
