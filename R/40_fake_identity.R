source("R/01_source.R")


# Load data ---------------------------------------------------------------

host_face_confidence <- qread("output/host_face_confidence.qs")
property <- qread("output/property.qs")


# Analysis ----------------------------------------------------------------

multi_cities <- 
property %>% 
  group_by(host_ID) %>% 
  count(city) %>% 
  ungroup() %>% 
  count(host_ID, sort=T, name = "index") %>% 
  left_join(host_face_confidence) %>% 
  mutate(index = ifelse(index > 1, TRUE, FALSE)) %>% 
  group_by(index) %>% 
  summarize(avg = mean(face_confidence, na.rm = TRUE)/100) %>% 
  mutate(domain = "Multiple cities")

#' A host that is in multiple city (commercial host) will have a dramatic 20% 
#' less chance of providing a face on his host_photo.

multi_listings <- 
property %>% 
  count(host_ID, sort=T, name = "index") %>% 
  left_join(host_face_confidence) %>% 
  mutate(index = ifelse(index > 1, TRUE, FALSE)) %>%
  group_by(index) %>% 
  summarize(avg = mean(face_confidence, na.rm = TRUE)/100) %>% 
  mutate(domain = "Multiple listings")

no_face_avg <-
  rbind(multi_cities, multi_listings) %>% 
  mutate(per = scales::percent(avg),
         index = factor(index, levels = c("TRUE", "FALSE"))) %>% 
  ggplot()+
  geom_bar(aes(x = index, y = avg, fill = index), stat="identity", 
           fill = c(gp_duo4, gp_duo4))+
  facet_wrap("domain")+
  scale_y_continuous(labels = scales::percent)+
  geom_text(aes(x = index, y = avg, label=per), vjust=2)+
  theme_minimal()+
  xlab(NULL)+
  ylab("Face detection average")+
  theme(legend.position = "none")

ggsave("output/figures/no_face_avg.png", plot = no_face_avg, width = 8, 
       height = 5, units = "in")
