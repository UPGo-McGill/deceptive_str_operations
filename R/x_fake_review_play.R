library(here)
source(here("R", "01_source.R"))

property <- qread(here("output", "property.qs"), parallel::detectCores())
review <- qread(here("output", "m_review.qs"))
review_text_pred <- qread(here("output","review_text_pred_liwc.qs"))

per_user <- 
review_text_pred %>% 
  group_by(user_ID) %>% 
  summarize(mean = mean(chance_fake), sd = sd(chance_fake), nb_reviews = n())

fake_reviewers <- 
per_user %>% 
  filter(mean-sd >= 0, nb_reviews >= 5) %>% 
  pull(user_ID)

property_fake_reviewed <- 
review %>% 
  filter(user_ID %in% fake_reviewers) %>% 
  group_by(property_ID) %>% 
  summarize(nb_fake_review_from_fake_reviewers = n()) %>% 
  inner_join(property)

fake_reviewed_city <- 
property_fake_reviewed %>% 
  count(city) %>% 
  arrange(-n)

fake_reviewed_city %>% 
  ggplot()+
  geom_col(aes(city, n), fill = color_palette[3])+
  geom_text(aes(x = city, y = n, label=n), vjust=c(rep(2,9), -0.5))+
  theme_minimal()+
  xlab(NULL)+
  ylab("Number of properties")+
  theme(legend.position = "none")





property_fake_reviewed %>% 
  count(FREH) 


property_fake_reviewed %>% 
  group_by(host_ID) %>% 
  summarize(nb_fake_review_from_fake_reviewers = sum(nb_fake_review_from_fake_reviewers), nb_properties = n()) %>% 
  arrange(-nb_fake_review_from_fake_reviewers) %>% 
  ggplot()+
  geom_line(aes(nb_fake_review_from_fake_reviewers, nb_properties))+
  # xlim(0,20)+
  # ylim(0,10)+
  geom_smooth(aes(nb_fake_review_from_fake_reviewers, nb_properties))





FREH <- qread(here("output","FREH.qs"))

FREH <- 
  FREH %>% 
  mutate(date = str_replace(date, "\\d\\d$", "01"),
         date = as.Date(date)) %>% 
  distinct()

review %>% 
  filter(user_ID %in% fake_reviewers,
         property_ID %in% property$property_ID) %>% 
  select(date, property_ID) %>% 
  mutate(fake_review = TRUE) %>% 
  full_join(FREH, by = c("date", "property_ID")) %>% 
  left_join(select(property, property_ID, city)) %>% 
  mutate(FREH = coalesce(FREH, FALSE),
         fake_review = coalesce(fake_review, FALSE)) %>% 
  count(fake_review, FREH)

review_fake <- 
review %>% 
  filter(user_ID %in% fake_reviewers,
         property_ID %in% property$property_ID)


review %>% 
  select(date, property_ID) %>% 
  mutate(r_year = as.numeric(str_extract(date, "^\\d{4}")),
         r_month = as.numeric(str_remove(str_extract(date, "-\\d{2}"), "-"))) %>% 
  left_join(select(property, property_ID, created), by = "property_ID") %>% 
  mutate(p_year = as.numeric(str_extract(created, "^\\d{4}")),
         p_month = as.numeric(str_remove(str_extract(created, "-\\d{2}"), "-"))) %>% 
  mutate(review_3_months_after_creation = ifelse(r_year == p_year & (r_month == p_month | 
                                                   r_month-1 == p_month | 
                                                   r_month-2 == p_month | 
                                                   r_month-3 == p_month), TRUE, FALSE)) %>% 
  count(review_3_months_after_creation) %>% 
  mutate(per = n/sum(n))


review %>% 
  filter(property_ID %in% property$property_ID) %>% 
  select(date, property_ID) %>% 
  left_join(select(property, property_ID, created), by = "property_ID") %>% 
  mutate(created = str_replace(created, "\\d{2}$", "01"),
         created = as.Date(created)) %>% 
  mutate(months_diff = (zoo::as.yearmon(date)-zoo::as.yearmon(created))*12,
         months_diff = ifelse(months_diff < 0, 0, months_diff)) %>%
  summarize(mean(months_diff), median(months_diff), sd(months_diff))





host_nb_properties <- 
property %>% 
  count(host_ID)
  

saved <- 
property_fake_reviewed %>% 
  group_by(host_ID) %>% 
  summarize(nb_fake_review_from_fake_reviewers = sum(nb_fake_review_from_fake_reviewers)) %>% 
  left_join(host_nb_properties)


saved %>% 
  group_by(n) %>% 
  summarize(nb_fake_review_from_fake_reviewers = mean(nb_fake_review_from_fake_reviewers)) %>% 
  ggplot()+
  geom_col(aes(n, nb_fake_review_from_fake_reviewers))+
  xlim(0,100)


