property <- qread("output/property.qs")


property %>% 
  arrange(-match_diff_city) %>% 
  filter(match_diff_city == 69) %>% 
  count(city)
select(ab_image_url, match_diff_city)


matches_dif_city %>% 
  filter(x_pid == "ab-23180738")


property %>% 
  filter(property_ID == "ab-21720691") %>% 
  select(ab_image_url)

property %>% 
  filter(property_ID %in% (matches_dif_city %>% 
                             filter(x_pid == "ab-34965188"))$y_pid) %>% 
  View

property %>% 
  filter(!city %in% c("Davenport", "Kissimmee")) %>% 
  arrange(-match_diff_city) %>%
  select(ab_image_url)

confidence_studied <- 
  property %>% 
  filter(!city %in% c("Davenport", "Kissimmee")) %>% 
  arrange(-match_diff_city) %>%
  filter(match_diff_city >0) %>% 
  select(ab_image_url)

for(i in 1:nrow(confidence_studied)){
  print(magick::image_read(pull(confidence_studied[i,1])))
  Sys.sleep(0.1)
}


#' Regarding fake listings, it seems there is two stories. The first one is the high number
#' of listings that seems to be from the same resort, in Kissimmee and Davenport. Maybe
#' located between the two of them, and this is why the city is sometimes different
#' for the same photo of that resort?
#' 
#' There's also another one, like the property in NY we see that has been matched 10 times
#' in San Diego. We know it's something that's happening, being deceptive by showing
#' a photo different from the listing offered. However, this is not something we can dive
#' deeper in due to the limited amount of cities under analysis.