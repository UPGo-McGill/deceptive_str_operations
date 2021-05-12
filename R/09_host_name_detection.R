source("R/01_source.R")

# Load data ---------------------------------------------------------------

review <- qread("output/net_review.qs")
review_text <- qread("output/net_review_text.qs",
                     nthreads = parallel::detectCores())
review_user <- qread("data/review_user_hosts.qs")
property <- qread("output/property.qs",
                  nthreads = parallel::detectCores()-1)


# Matrice preparation -----------------------------------------------------
property <- 
property %>% 
  mutate(initial_host_ID = coalesce(old_host, host_ID))

host_to_scrape <- 
property %>% 
  filter(!initial_host_ID %in% review_user$user_ID) %>% 
  distinct(initial_host_ID)

# qsave(host_to_scrape, file = "output/host_to_scrape.qs")

hosts_names <- 
property %>% 
  select(initial_host_ID) %>% 
  inner_join(mutate(review_user, user_ID = as.character(user_ID)),
             by = c("initial_host_ID" = "user_ID")) %>% 
  distinct(initial_host_ID, user_name) %>% 
  group_by(initial_host_ID) %>% 
  summarize(user_name = list(user_name)) %>% 
  rowwise() %>%
  mutate(# sometimes user name changes after some time
         user_name = str_c(user_name, collapse = "|"),
         user_name = str_remove_all(user_name, "\\(|\\)"),
         # if there are three words, very likely the middle word will be &
         user_name = str_replace_all(user_name, " .* ", "|")) %>% 
  ungroup() %>% 
  rename(host_name = user_name)

review_host <- 
review %>% 
  left_join(select(property, property_ID, initial_host_ID)) %>% 
  select(review_ID, initial_host_ID)
  
review_text_host_name <- 
review_text %>% 
  left_join(review_host) %>% 
  left_join(hosts_names)


# Analysis ----------------------------------------------------------------


name_mention_host <-
review_text_host_name %>%
  # only look at host names we know
  filter(!is.na(host_name)) %>%
  # mutate(host_name = str_remove_all(host_name, "\\(|\\)")) %>%
  mutate(name_mention = ifelse(str_detect(review, pattern = str_glue("{host_name}")), T, F)) %>%
  group_by(initial_host_ID) %>%
  summarize(name_mention = sum(name_mention , na.rm=T), nb_reviews = n())

name_mention <- 
  review_text_host_name %>%
  # only look at host names we know
  filter(!is.na(host_name)) %>% 
  # mutate(host_name = str_remove_all(host_name, "\\(|\\)")) %>% 
  mutate(name_mention = ifelse(str_detect(review, pattern = str_glue("{host_name}")), T, F)) %>%
  group_by(property_ID) %>%
  summarize(name_mention = sum(name_mention , na.rm=T), nb_reviews = n())


name_mention_host <-
  name_mention_host %>%
  left_join(select(property, host_ID, old_host), by = c("initial_host_ID" = "old_host")) %>%
  mutate(new_host_ID = coalesce(host_ID, initial_host_ID)) %>%
  select(new_host_ID, name_mention, nb_reviews) %>%
  rename(host_ID = new_host_ID) %>%
  group_by(host_ID) %>%
  summarize(name_mention = sum(name_mention), nb_reviews = sum(nb_reviews))


# Save --------------------------------------------------------------------

qsave(name_mention, file = "output/name_mention.qs")
qsave(name_mention_host, file = "output/name_mention_host.qs")
