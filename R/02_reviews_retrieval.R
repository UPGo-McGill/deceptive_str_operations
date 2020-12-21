source("R/01_source.R")

library(upgo)
library(strr)


upgo_connect(review = T,
             review_user = T, 
             review_text = T)

for (i in 1:25) {
  try({
    review <- 
      review_all %>% 
      filter(country == "Canada", city == "Montreal") %>% 
      collect()
    break
  })
}

for (i in 1:25) {
  try({
    review_text <- 
      review_text_all %>% 
      filter(review_ID %in% !! review$review_ID) %>% 
      collect()
    break
  })
}

      
for (i in 1:25) {
  try({
    review_user <- 
      review_user_all %>% 
      filter(user_ID %in% !! review$user_ID) %>% 
      collect()
    break
  })
}


upgo_disconnect()

save(review, review_text, review_user, file = "review.Rdata")