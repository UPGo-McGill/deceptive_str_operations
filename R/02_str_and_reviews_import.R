source("R/01_source.R")
library(upgo)
library(strr)

upgo_connect(review = T,
             review_user = T, 
             review_text = T)


# STR ---------------------------------------------------------------------

property <- 
  property_remote %>% 
  filter(country == "Canada") %>% 
  collect()

daily <- 
  daily_all %>% 
  filter(property_ID %in% !!property$property_ID) %>% 
  collect()

host <-
  host_remote %>% 
  filter(host_ID %in% !!property$host_ID) %>% 
  collect() %>% 
  strr_expand()


# Reviews -----------------------------------------------------------------

review <- 
  review_all %>% 
  filter(country == "Canada") %>% 
  collect()

review_text <- 
  review_text_all %>% 
  filter(review_ID %in% !! review$review_ID) %>% 
  collect()

review_user <- 
  review_user_all %>% 
  filter(user_ID %in% !! review$user_ID) %>% 
  collect()


# Disconnect and save -----------------------------------------------------

upgo_disconnect()

qsavem(property, daily, host, file = "data/str_raw.qsm",
       nthreads = availableCores())
qsavem(review, review_text, review_user, file = "data/review.qsm",
       nthreads = availableCores())