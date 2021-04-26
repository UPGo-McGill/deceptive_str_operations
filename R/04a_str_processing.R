source("R/01_source.R")


# Load data ---------------------------------------------------------------

property <- qread("data/property.qs",
                  nthreads = parallel::detectCores())

daily <- qread("data/daily.qs",
                  nthreads = parallel::detectCores())

host <- qread("data/host.qs",
                  nthreads = parallel::detectCores())

matches <- qread("output/matches.qs")


# Filter in only entire homes ---------------------------------------------

property <- 
  property %>% 
  filter(listing_type == "Entire home/apt",
         startsWith(property_ID, "ab"),
         housing)

# Filter out cities with same city name in different states ---------------

property <- 
property %>% 
  group_by(region, city) %>% 
  filter(n()>100) %>% 
  ungroup()


# Filter out properties that seems to be a lag ----------------------------

property <- 
property %>% 
  filter(!(host_ID == "108156815" & created == "2019-12-13" &
           scraped == "2019-12-27"))


# Filter daily and host considering changes above -------------------------

daily <-
  daily %>%
  filter(property_ID %in% property$property_ID)

host <- 
  host %>% 
  filter(host_ID %in% property$host_ID) %>% 
  distinct(host_ID)

# Going tu be used like this for the face detection analysis
qsave(host, file = "output/host.qs",
      nthreads = parallel::detectCores())


### MATCHES ###############################################################


# Identify groupings ------------------------------------------------------

# Keep only matches that have a property_ID in my relevant df
matches <- 
  matches %>% 
  filter(x_pid %in% property$property_ID,
         y_pid %in% property$property_ID)

# I need to make sure that I don't nest together properties that are NOT from
# the same city! 
matches_same_city <- 
matches %>%
  left_join(select(property, property_ID, city), by = c("x_pid" = "property_ID")) %>%
  rename(x_city = city) %>% 
  left_join(select(property, property_ID, city), by = c("y_pid" = "property_ID")) %>%
  rename(y_city = city) %>% 
  filter(x_city == y_city) %>% 
  select(x_pid, y_pid)


# Convert to list of pairs
pair_list <- pmap(matches_same_city, c)

# Merge lists
reduce <- function(x) {
  
  Reduce(function(a, b) {
    merge_index <- lapply(a, intersect, b)
    
    if (sum(lengths(merge_index)) > 0) {
      merge_index <- which(lengths(merge_index) > 0)
      merged <- a[merge_index]
      merged <- unlist(merged)
      merged <- union(merged, b)
      merged <- list(sort(merged))
      not_merged <- a[-merge_index]
      out <- c(merged, not_merged)
    } else out <- c(a, list(b))
  }, x, init = list())

}

groupings <- reduce(pair_list)

# qsave(groupings, file = "output/match_groupings.qs")

# Modify host_ID from groupings -------------------------------------------

host_IDs <-
  groupings %>%
  map(~{
    property %>%
      filter(property_ID %in% .x) %>%
      pull(host_ID) %>%
      unique()
  })

host_IDs <- reduce(host_IDs)
host_IDs <- map(host_IDs, sort)
host_IDs <- host_IDs[lengths(host_IDs) > 0]

host_change_table <-
  map_dfr(host_IDs, ~tibble(host_ID = .x, new_host = .x[[1]]))

property <-
  property %>%
  left_join(host_change_table) %>%
  mutate(old_host = if_else(is.na(new_host), NA_character_, host_ID),
         host_ID = if_else(is.na(new_host), host_ID, new_host)) %>%
  select(-new_host)

daily <-
  daily %>%
  left_join(host_change_table) %>%
  mutate(old_host = if_else(is.na(new_host), NA_character_, host_ID),
         host_ID = if_else(is.na(new_host), host_ID, new_host)) %>%
  select(-new_host)

rm(host_change_table, host_IDs)


# Get matches -------------------------------------------------------------

# Get final activity date
property <-
  daily %>%
  filter(status != "B") %>%
  group_by(property_ID) %>%
  filter(date == max(date)) %>%
  slice(1) %>%
  ungroup() %>%
  select(property_ID, active = date) %>%
  left_join(property, .) %>% 
  select(property_ID:last_active, active, everything())

group_matches <-
  groupings %>%
  map(~{
    property %>%
      filter(property_ID %in% .x) %>%
      mutate(active = if_else(is.na(active), created, active)) %>%
      filter(active >= created)
  })

group_matches <- group_matches[map_int(group_matches, nrow) > 0]
group_matches <- group_matches %>% map(arrange, created)

group_matches <-
  group_matches %>%
  map(~{
    next_created <- c(.x$created, NA)
    # Drop the first element to shift all created dates up a row
    next_created <- next_created[2:length(next_created)]
    .x %>%
      mutate(next_created = next_created) %>%
      filter(active < next_created | is.na(next_created))
  })

group_matches <- group_matches[map_int(group_matches, nrow) > 1]


# Collapse property_IDs ---------------------------------------------------

property_change_table <-
  map_dfr(group_matches,
          ~tibble(
            property_ID = .x$property_ID,
            new_PID =
              filter(.x, active - created == max(active - created)) %>%
              slice(1) %>%
              pull(property_ID),
            new_created = min(.x$created, na.rm = TRUE),
            new_scraped = max(.x$scraped, na.rm = TRUE),
            new_active = max(.x$active, na.rm = TRUE)
          ))

daily <-
  daily %>%
  left_join(property_change_table) %>%
  mutate(old_PID = if_else(is.na(new_PID), NA_character_, property_ID),
         property_ID = if_else(is.na(new_PID), property_ID, new_PID)) %>%
  select(-new_PID, -new_created, -new_scraped, -new_active)

property_change_collapsed <-
  property_change_table %>%
  group_by(new_PID, new_created, new_scraped, new_active) %>%
  summarize(all_PIDs = list(property_ID))

property_to_delete <-
  property_change_table %>%
  filter(property_ID != new_PID)

property <-
  property %>%
  left_join(property_change_collapsed, by = c("property_ID" = "new_PID")) %>%
  filter(!property_ID %in% property_to_delete$property_ID) %>%
  mutate(created = if_else(!is.na(new_created), new_created, created),
         scraped = if_else(!is.na(new_scraped), new_scraped, scraped),
         active = if_else(!is.na(new_active), new_active, active)) %>%
  select(-new_created, -new_scraped, -new_active)

rm(group_matches, property_change_collapsed, property_change_table,
   property_to_delete)


# Column if same photo used in different cities ---------------------------
matches_diff_city <- 
  matches %>%
  left_join(select(property, property_ID, city), by = c("x_pid" = "property_ID")) %>%
  rename(x_city = city) %>% 
  left_join(select(property, property_ID, city), by = c("y_pid" = "property_ID")) %>%
  rename(y_city = city) %>% 
  filter(x_city != y_city) %>%
  select(x_pid, y_pid)

property <- 
property %>% 
  rowwise() %>% 
  mutate(match_diff_city = (matches_diff_city %>% 
                          filter(x_pid %in% property_ID |
                                   x_pid %in% unlist(all_PIDs) |
                                   y_pid %in% property_ID |
                                   y_pid %in% unlist(all_PIDs)) %>% 
                          nrow())) %>% 
  ungroup()


# To follow which other properties in other city they matched to
pair_list <- pmap(matches_diff_city, c)
match_groupings_diff_city <- reduce(pair_list)

qsave(match_groupings_diff_city, file = "output/match_groupings_diff_city.qs",
      nthreads = parallel::detectCores())

# Column for how many times it matched in same city -----------------------

property <- 
property %>% 
  rowwise() %>% 
  mutate(matched = (matches_same_city %>% 
                      filter(x_pid == property_ID | y_pid == property_ID) %>% 
                      nrow())) %>% 
  ungroup()


# How many host accounts in the network -----------------------------------

host_networks <- 
  property %>% 
  group_by(host_ID) %>% 
  summarize(all_host_IDs = list(unique(old_host))) %>% 
  rowwise() %>% 
  mutate(nb_old_host = sum(!is.na(all_host_IDs))) %>% 
  ungroup()

  
# Commercial listing column added to property -----------------------------
library(strr)

FREH <- 
  daily %>% 
  strr_FREH() %>% 
  filter(FREH)

# For multilisting calculation, I can take the previous host df and calculate
# with it. Daily already has host_ID updated regarding matches.
host <- qread("data/host.qs",
              nthreads = parallel::detectCores())

daily <-
  daily %>%
  strr_multi(host) %>%
  as_tibble()


# Once FREH or multi in lifetime? -----------------------------------------

property <- 
  property %>% 
  mutate(FREH = ifelse(property_ID %in% FREH$property_ID, T, F),
         multi = ifelse(property_ID %in% (daily %>% 
                                            filter(multi) %>% 
                                            distinct(property_ID) %>% 
                                            pull(property_ID)), T, F))


# Save data ---------------------------------------------------------------

qsave(property, file = "output/property.qs",
      nthreads = parallel::detectCores())

qsave(daily, file = "output/daily.qs",
      nthreads = parallel::detectCores())

qsave(FREH, file = "output/FREH.qs",
      nthreads = parallel::detectCores())

qsave(host_networks, file = "output/host_networks.qs",
      nthreads = parallel::detectCores())
