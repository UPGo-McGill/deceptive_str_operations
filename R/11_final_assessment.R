source("R/01_source.R")

property <- qread("output/property.qs")
host_networks <- qread("output/host_networks.qs")
review_text_pred_liwc <- qread("output/review_text_pred_liwc.qs")
host_face_confidence <- qread("output/host_face_confidence_networks.qs")
name_mention <- qread("output/name_mention.qs")
review <- qread("output/m_review.qs")


# Hosts listing recycling score -------------------------------------------

listing_recycling <- 
property %>% 
  rowwise() %>% 
  mutate(nb_all_PIDs = sum(!is.na(all_PIDs))) %>% 
  group_by(host_ID) %>% 
  summarize(nb_recycled = sum(nb_all_PIDs), nb_listings = n()) %>% 
  transmute(host_ID,
            listing_recycling_score = nb_recycled/nb_listings)

# Host recycling score ----------------------------------------------------

host_networks <- 
host_networks %>% 
  rename(host_recycling_score = nb_old_host) %>%
  select(host_ID, host_recycling_score)


# Fake reviewing score ----------------------------------------------------

per_user <- 
  review_text_pred_liwc %>% 
  group_by(user_ID) %>% 
  summarize(mean_fake = mean(chance_fake, na.rm=T), 
            sd = sd(chance_fake, na.rm=T), nb_reviews = n())

fake_reviewers <- 
  per_user %>% 
  filter(mean_fake-sd > 0, nb_reviews >= 5) %>% 
  pull(user_ID)

fake_reviewing <- 
  review %>% 
  filter(user_ID %in% fake_reviewers) %>% 
  right_join(select(property, property_ID, host_ID), by = "property_ID") %>% 
  group_by(host_ID) %>% 
  summarize(fake_review_score = sum(!is.na(review_ID)))

# Face detection score ----------------------------------------------------

host_face_confidence <- 
  host_face_confidence %>% 
  select(host_ID, face_confidence)

# Name mention in reviews score -------------------------------------------

name_mention <- 
name_mention %>% 
  mutate(name_mention_score = name_mention/nb_reviews) %>% 
  right_join(distinct(select(property, host_ID), host_ID), by = "host_ID") %>% 
  select(host_ID, name_mention_score)

# All together ------------------------------------------------------------

all_together <- 
listing_recycling %>% 
  left_join(host_networks) %>% 
  left_join(fake_reviewing) %>% 
  left_join(host_face_confidence) %>% 
  left_join(name_mention)

all_together <- 
all_together %>% 
  left_join({property %>% 
      mutate(commercial = ifelse(FREH == T | multi == T, T, F)) %>% 
      arrange(-commercial) %>% 
      distinct(host_ID, .keep_all=T) %>% 
      select(host_ID, commercial)}) %>% 
  select(-host_ID)

all_together %>% 
  filter(commercial) %>% 
  filter(listing_recycling_score > 0) %>% 
  nrow() / 
  all_together %>% 
  filter(commercial) %>% 
  nrow()


round(cor(all_together, use = "complete.obs"), 2)
# 
# library(psych)
# 
# pairs.panels(all_together)
# corr.test(all_together)
# error.dots(all_together)
# 
# principal(all_together)
# factor.pa(all_together)
# fa(all_together)
# ICLUST(all_together)
# 
# alpha(all_together, check.keys = TRUE)
# omega(all_together)
# 
# all_together.keys <- make.keys(all_together, list(all_together = c(1,2,3,-4,-5)))
# 
# scoreItems(all_together.keys, all_together)
# 