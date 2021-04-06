source("R/01_source.R")
# devtools::install_github("UPGo-McGill/matchr")
library(vctrs)
library(matchr)

progressr::handlers(global = TRUE)


# Load necessary files ----------------------------------------------------

ab_results <- qread("data/temp_results.qs")


# Prepare tables ----------------------------------------------------------

field(ab_results$x_sig, "file") <- 
  ab_results$x_sig %>% 
  field("file") %>% 
  str_replace_all("/.*/.*/.*/.*/", "/Users/maxim/OneDrive - McGill University/DocumentsHP/Universite/MUP McGill/Fall 2020/3 - SRP/deceptive_str_operations/data/matched_property_photos/")

field(ab_results$y_sig, "file") <- 
  ab_results$y_sig %>% 
  field("file") %>% 
  str_replace_all("/.*/.*/.*/.*/", "/Users/maxim/OneDrive - McGill University/DocumentsHP/Universite/MUP McGill/Fall 2020/3 - SRP/deceptive_str_operations/data/matched_property_photos/")

# Open shiny and save results ---------------------------------------------

ab_change <-
  ab_results  %>% 
  filter(correlation < 0.9995) %>% 
  compare_images(batch_size = 500L)


# Save both results in one simple df --------------------------------------

untouched <- 
  ab_results %>% 
  filter(correlation >= 0.9995)

ab_change_matches <- 
ab_change %>% 
  rename(match = new_match_status) %>% 
  filter(match == "match")

# Extract property_IDs
ab_change_matches <- 
ab_change_matches %>% 
  mutate(x_pid = field(x_sig, "file"),
         x_pid = str_remove_all(x_pid, 
                                pattern = "/Users/maxim/OneDrive - McGill University/DocumentsHP/Universite/MUP McGill/Fall 2020/3 - SRP/deceptive_str_operations/data/matched_property_photos/"),
         x_pid = str_remove_all(x_pid, 
                                pattern = ".jpg"),
         y_pid = field(y_sig, "file"),
         y_pid = str_remove_all(y_pid, 
                                pattern = "/Users/maxim/OneDrive - McGill University/DocumentsHP/Universite/MUP McGill/Fall 2020/3 - SRP/deceptive_str_operations/data/matched_property_photos/"),
         y_pid = str_remove_all(y_pid, 
                                pattern = ".jpg")) %>% 
  select(x_pid, y_pid)

untouched_matches <- 
  untouched %>% 
  mutate(x_pid = field(x_sig, "file"),
         x_pid = str_remove_all(x_pid, 
                                pattern = "/Users/maxim/OneDrive - McGill University/DocumentsHP/Universite/MUP McGill/Fall 2020/3 - SRP/deceptive_str_operations/data/matched_property_photos/"),
         x_pid = str_remove_all(x_pid, 
                                pattern = ".jpg"),
         y_pid = field(y_sig, "file"),
         y_pid = str_remove_all(y_pid, 
                                pattern = "/Users/maxim/OneDrive - McGill University/DocumentsHP/Universite/MUP McGill/Fall 2020/3 - SRP/deceptive_str_operations/data/matched_property_photos/"),
         y_pid = str_remove_all(y_pid, 
                                pattern = ".jpg")) %>% 
  select(x_pid, y_pid)

# Bind both df
matches <- 
rbind(ab_change_matches,untouched_matches)


# Save matches df and other to keep track ---------------------------------

qsave(matches, "output/matches.qs")
qsavem(ab_results, ab_change, untouched, file = "output/keep_track_matches.qsm")
