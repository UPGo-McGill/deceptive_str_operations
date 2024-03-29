source("R/01_source.R")


# Load data ---------------------------------------------------------------

property_nm <- qread("data/property.qs",
                  nthreads = parallel::detectCores())

daily_nm <- qread("data/daily.qs",
               nthreads = parallel::detectCores())

host <- qread("data/host.qs",
              nthreads = parallel::detectCores())


# Filter in only entire homes ---------------------------------------------

property_nm <- 
  property_nm %>% 
  filter(listing_type == "Entire home/apt",
         startsWith(property_ID, "ab"),
         housing)

# Filter out cities with same city name in different states ---------------

property_nm <- 
  property_nm %>% 
  group_by(region, city) %>% 
  filter(n()>100) %>% 
  ungroup()


# Filter out properties that seems to be a lag ----------------------------

property_nm <- 
  property_nm %>% 
  filter(!(host_ID == "108156815" & created == "2019-12-13" &
             scraped == "2019-12-27"))


# Filter daily and host considering changes above -------------------------

daily <-
  daily %>%
  filter(property_ID %in% property_nm$property_ID)

host <- 
  host %>% 
  filter(host_ID %in% property_nm$host_ID) %>% 
  distinct(host_ID)

# Commercial listing column added to property_nm -----------------------------
library(strr)

FREH_nm <- 
  daily_nm %>% 
  strr_FREH() %>% 
  filter(FREH)

daily_nm <-
  daily_nm %>%
  strr_multi(host) %>%
  as_tibble()


# Once FREH_nm or multi in lifetime? -----------------------------------------

property_nm <- 
  property_nm %>% 
  mutate(FREH = ifelse(property_ID %in% FREH_nm$property_ID, T, F),
         multi = ifelse(property_ID %in% (daily_nm %>% 
                                            filter(multi) %>% 
                                            distinct(property_ID) %>% 
                                            pull(property_ID)), T, F))

# Save data ---------------------------------------------------------------

qsave(property_nm, file = "output/property_nm.qs",
      nthreads = parallel::detectCores())

qsave(daily_nm, file = "output/daily_nm.qs",
      nthreads = parallel::detectCores())

qsave(FREH_nm, file = "output/FREH_nm.qs",
      nthreads = parallel::detectCores())
