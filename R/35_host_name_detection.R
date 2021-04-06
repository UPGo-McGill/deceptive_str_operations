source("R/01_source.R")


# Load data ---------------------------------------------------------------

qload("output/review_processed.qsm",
      nthreads = parallel::detectCores())
property <- qread("output/property.qs",
      nthreads = parallel::detectCores()-1)


# Matrice preparation -----------------------------------------------------

review_text_host_name <- 
review_text %>% 
  left_join(select((review %>% 
                      left_join((property %>% 
                                   select(property_ID, host_ID)))), property_ID, host_ID),
            by = "property_ID") %>% 
  distinct()

review_text_host_name <- 
review_text_host_name %>% 
  select(-user_ID) %>% 
  rename(user_ID = host_ID) %>% 
  left_join(select(mutate(review_user, user_ID = as.character(user_ID)), user_ID, user_name)) %>% 
  mutate(user_name = str_to_lower(user_name)) %>% 
  distinct()


# Analysis ----------------------------------------------------------------

review_text_host_name %>% 
  group_by(review_ID) %>% 
  filter(n()>1)
# interesting. Sometimes, the name changes! Which is odd, and worth mentioning.

review_text_host_name <-
review_text_host_name %>% 
  filter(!is.na(user_name)) %>% 
  mutate(user_name = str_replace_all(user_name, " .* ", "|"))

review_text_host_name %>% 
  filter(str_detect(review, pattern = str_glue("{user_name}"))) %>% View
