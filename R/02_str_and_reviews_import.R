source("R/01_source.R")
library(upgo)
library(strr)

upgo_connect(review = T,
             review_user = T, 
             review_text = T)


# STR ---------------------------------------------------------------------

for(a in 1:100) {
  try({
    property <- 
      property_remote %>% 
      filter(country == "Canada") %>% 
      collect()
    break
  })
}

for(a in 1:100) {
  try({
    daily <- 
      daily_remote %>% 
      filter(property_ID %in% !!property$property_ID) %>% 
      collect()
    break
  })
}

for(a in 1:100) {
  try({
    host <-
      host_remote %>% 
      filter(host_ID %in% !!property$host_ID) %>% 
      collect() %>% 
      strr_expand()
    break
  })
}


# Reviews -----------------------------------------------------------------

for(a in 1:100) {
  try({
    review <- 
      review_remote %>% 
      filter(country == "Canada") %>% 
      collect()
    break
  })
}

for(a in 1:100) {
  try({
    review_text <- 
      review_text_remote %>% 
      filter(review_ID %in% !! review$review_ID) %>% 
      collect()
    break
  })
}

for(a in 1:100) {
  try({
    review_user <- 
      review_user_remote %>% 
      filter(user_ID %in% !! review$user_ID) %>% 
      collect()
    break
  })
}


# Disconnect and save -----------------------------------------------------

upgo_disconnect()

qsavem(property, daily, host, file = "data/str_raw.qsm",
       nthreads = parallel::detectCores()-1)
qsavem(review, review_text, review_user, file = "data/review.qsm",
       nthreads = parallel::detectCores()-1)