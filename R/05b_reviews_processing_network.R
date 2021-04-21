
# Load libraries and data -------------------------------------------------
source("R/01_source.R")

review <- qread("data/review_all.qs",
                nthreads = parallel::detectCores())

review_text <- qread("data/review_text.qs",
                     nthreads = parallel::detectCores())

review_user <- qread("data/review_user.qs",
                     nthreads = parallel::detectCores())

property <- qread("output/property.qs",
                  nthreads = parallel::detectCores())

# Keep only reviews in properties under study -----------------------------
review <- 
review %>% 
  filter(property_ID %in% unique(c(property$property_ID, unlist(property$all_PIDs))))

review_text <- 
review_text %>% 
  filter(review_ID %in% !! review$review_ID)

review_user <- 
  review_user %>% 
  filter(user_ID %in% review$user_ID)


# Change the property_ID to the new one after image matching --------------
property <- qread(here("output", "property.qs"), parallel::detectCores())

property_unnested <- 
  property %>% 
  select(property_ID, all_PIDs) %>% 
  unnest(all_PIDs) %>% 
  rename(new_ID = property_ID,
         old_ID = all_PIDs)

review <- 
  review %>% 
  left_join(property_unnested, by = c("property_ID" = "old_ID")) %>% 
  mutate(property_ID = coalesce(new_ID, property_ID)) %>% 
  select(-new_ID)

# Save every reviews in English -------------------------------------------

qsave(review, file = "output/net_review.qs",
      nthreads = parallel::detectCores())

qsave(review_text, file = "output/net_review_text.qs",
      nthreads = parallel::detectCores())

qsave(review_user, file = "output/net_review_user.qs",
      nthreads = parallel::detectCores())
