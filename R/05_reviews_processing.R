
# Load libraries and data -------------------------------------------------
source("R/01_source.R")
# library(googleLanguageR)

load("data/review_mtl.Rdata")


# Which package seems best to detect text language ------------------------

# if (!require("pacman")) install.packages("pacman") # for package management
# pacman::p_load("tidyverse") 
# pacman::p_load("textcat")
# pacman::p_load("cld2")
# pacman::p_load("cld3")
# 
# review_text %>% 
#   slice(200000:201000) %>% 
#   mutate(textcat = textcat(x = review), 
#          cld2 = cld2::detect_language(text = review, plain_text = FALSE), 
#          cld3 = cld3::detect_language(text = review))

# cld2 is the most effective


# Detect language ---------------------------------------------------------

review_text <- 
  review_text %>% 
  mutate(lang = cld2::detect_language(text = review, plain_text = FALSE))


# # Translation manipulation ------------------------------------------------
# 
# review_text_other_lang <- 
#   review_text %>% 
#   filter(lang != "en")
# 
# # To speed things up, we need a function to iterate over maximum 204800 bytes 
# # of characters, or maximum of 128 segments. That's 128 reviews at a time.
# 
# translate_iterations <- function(data, chunk_size = 128) {
#   
#   ## Prepare batches -----------------------------------------------------------
#   
#   iterations <- 1
#   
#   
#   ### SET BATCH PROCESSING STRATEGY ############################################
#   
#   if (nrow(data) > chunk_size) {
#     
#     iterations <- ceiling(nrow(data) / chunk_size)
#   }
#   
#   
#   ### PROCESS FOR SMALL TABLE ##################################################
#   
#   if (iterations == 1) {
#     # Run function
#     data$review <-
#       data$review %>%
#       gl_translate(target = "en") %>%
#       select(translatedText) %>%
#       pull()
#     
#   ### PROCESS FOR LARGE TABLE ##################################################
#     
#   } else {
#     
#     # Split data into chunks
#     chunk_list <- vector("list", iterations)
#     
#     # Process each batch sequentially
#     
#     for (i in seq_len(iterations)) {
#       
#       
#       range_1 <- floor((i-1) * nrow(data) / iterations + 1)
#       range_2 <- floor(i * nrow(data) / iterations)
#       
#       data_list <- review_text_other_lang[range_1:range_2,]
#       
#       chunk_list[[i]] <-     
#         data_list$review <-
#         data_list$review %>%
#         gl_translate(target = "en") %>%
#         select(translatedText) %>%
#         pull()
#       
#       chunk_list[[i]] <- data.table::rbindlist(chunk_list[[i]])
#       
#     }
#     
#     # Bind batches together
#     data <- data.table::rbindlist(chunk_list)
#     
#   }
#   return(data)
# }
# 
# # Translate what is not in English
# review_text_other_lang <- 
#   translate_iterations(review_text_other_lang)
# 
# # Put translated text back in review_text
# review_text <- 
# review_text %>% 
#   filter(!review_ID %in% review_text_other_lang$review_ID) %>% 
#   rbind(review_text_other_lang) %>% 
#   select(-lang)


# Without Google API ------------------------------------------------------

review_text <- 
  review_text %>% 
  filter(lang == "en") %>% 
  select(-lang)


# All review texts to lowercase --------------------------------------------

review_text <- 
review_text %>% 
  mutate(review = str_to_lower(review))


# Exclusion of automated reviews ------------------------------------------

review_text <-
  review_text %>%
  filter(!str_detect(review, "this is an automated posting"))

review <-
  review %>%
  filter(review_ID %in% review_text$review_ID)


# Filter out if not enough words for reliable analysis --------------------

### ONLY DO THAT LATER, FOR THE MODEL. WE WANT TO KEEP ALL OF THESE FOR THE 
### NETWORK ANALYSIS

review_text <-
  review_text %>%
  filter(lengths(str_split(review, " ")) > 4)

review <- 
  review %>% 
  filter(review_ID %in% review_text$review_ID)

review <- 
  review %>% 
  filter(user_ID %in% review_text$user_ID)


### WILL HAVE TO SEPARATED TWO DF. ONE WITH ALL REVIEWS, THE OTHER ONLY FOR
### THE PROPERTIES UNDER ANALYSIS

# Save every reviews in English -------------------------------------------

qsavem(review, review_text, review_user, file = "output/review_processed.qsm",
       nthreads = parallel::detectCores()-1)
