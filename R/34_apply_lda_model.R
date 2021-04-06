source("R/01_source.R")

qload("output/review_text_pred.qs")
qload("output/review_text_pred_liwc.qs")

review_text_pred_liwc %>% 
  group_by(property_ID) %>% 
  filter(n() >= 5) %>%
  summarize(mean = mean(x), sd = sd(x), sum = n())  %>% 
  mutate(quantile_mean = percent_rank(mean),
         quantile_sd = percent_rank(sd)) %>% 
  filter(quantile_mean >= 0.9 &
         quantile_sd <= 0.1) %>% View


review_text_pred_liwc %>% 
  filter(property_ID == "ab-18021067") %>% View

review_text_pred_liwc %>% 
  mutate(mmm = ifelse(x>0, T, F)) %>% 
  count(mmm)
