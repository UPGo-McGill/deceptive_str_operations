library(tidyverse)
library(qs)
library(magick)
library(image.libfacedetection)
library(future)
library(foreach)
library(progressr)
library(doFuture)
registerDoFuture()
plan(multisession)

# Load data ---------------------------------------------------------------

host_photos <- qread("output/host_photos.qs")


# Detect if face in image -------------------------------------------------

# https://www.r-bloggers.com/2019/03/human-face-detection-with-r/
# install.packages("image.libfacedetection", repos = "https://bnosac.github.io/drat")

face_detect <- function(url){
  if (is.na(url)) {
    return(NA)
  } else{
    #tryCatch because sometimes the link leads to 404 Not Found
    tryCatch(
      expr = {
        image <- image_read(url)
        faces <- image_detect_faces(image)
        if (nrow(faces$detections) == 0) {
          0
        } else {
          faces$detections$confidence[order(rev(faces$detections$confidence))[1]]
        }
      },
      error = function(e){
        return(NA)
      }
    )
  }
}

x <- nrow(host_photos)
batch_size <- 5000

with_progress({
  
  pb <- progressor(x)
  handlers(list(
    handler_progress(
      format   = ":spin :current/:total [:bar] :percent in :elapsed ETA: :eta",
      width    = 60,
      complete = "="
    )
  ))
  
  total_iterations <- ceiling(x / batch_size)
  
  iteration <- 1
  
  results <- vector("list", total_iterations)
  
  while (iteration <= total_iterations) {
    
    results[[iteration]] <- 
      
      foreach(i = ((iteration - 1) * batch_size + 1):min(iteration * batch_size, x), .combine = c) %dopar% {
        pb()
        face_detect(pull(host_photos[i,2]))
      }  
    
    iteration <- iteration + 1
    
    qsave(results, file = "output/results_host_face.qs")
  }
})

host_face_confidence <-
  host_photos %>%
  mutate(face_confidence = unlist(results))


# Match these face_confidence with host networks --------------------------

host_networks <- qread("output/host_networks.qs")

host_face_confidence_networks <- 
  host_networks %>% 
  rowwise() %>% 
  mutate(face_confidence = (host_face_confidence %>%
                              filter(host_ID %in% all_host_IDs) %>% 
                              summarize(face_confidence = mean(face_confidence, na.rm=T)) %>% 
                              pull(face_confidence))) %>% 
  ungroup() %>% 
  mutate(face_confidence = ifelse(is.nan(face_confidence), NA, face_confidence))


# Save result -------------------------------------------------------------

qsave(host_face_confidence, file = "output/host_face_confidence.qs")
qsave(host_face_confidence_networks, file = "output/host_face_confidence_networks.qs")

# 
# qload("output/str_processed.qsm")
# 
# 
# # Look at different confidence levels results -----------------------------
# 
# confidence_studied <- 
# host_face_confidence %>% 
#   filter(!is.na(face_confidence),
#          face_confidence >=80)
# 
# for(i in 1:nrow(confidence_studied)){
#   print(image_read(pull(confidence_studied[i,2])))
#   Sys.sleep(0.1)
# }
# 
# 
# # Start an analysis -------------------------------------------------------
# 
# library(sf)
# 
# property %>% 
#   st_drop_geometry() %>% 
#   filter(scraped >= "2019-01-01") %>% 
#   group_by(host_ID) %>% 
#   summarize(commercial = sum(commercial)) %>% 
#   mutate(commercial = ifelse(commercial > 0, T, F)) %>% 
#   # inner_join for now because I only have 20k host photos
#   inner_join(filter(select(host_face_confidence, -url), !is.na(face_confidence)), by = "host_ID") %>%
#   group_by(commercial) %>% 
#   summarize(zero_face = sum(face_confidence == 0, na.rm=T), listings_nb = n()) %>% 
#   mutate(per = scales::percent(zero_face/listings_nb, accuracy = 0.01))
# 
# ## When we're looking at the most recent only, commercial have higher scores.
# ## Let's try to do a plot with a time variable?