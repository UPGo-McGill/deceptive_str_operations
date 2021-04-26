source("R/01_source.R")

# Load data ---------------------------------------------------------------

host <- qread("output/host.qs",
      nthreads = parallel::detectCores()-1)


# Create a lost host df ---------------------------------------------------

# all_hosts <- 
#   property %>% 
#   st_drop_geometry() %>% 
#   distinct(host_ID) %>% 
#   rbind(
#     property %>% 
#       st_drop_geometry() %>% 
#       distinct(old_host) %>% 
#       rename(host_ID = old_host)
#   ) %>% 
#   pull(host_ID)
# 
# rm(property, daily)


# Scrape photos from Airbnb -----------------------------------------------

library(RSelenium)
# https://cran.r-project.org/web/packages/RSelenium/vignettes/basics.html
# https://stackoverflow.com/questions/45395849/cant-execute-rsdriver-connection-refused

# shell('docker run -d -p 4445:4444 selenium/standalone-firefox')

# Initiate the connection, remember remoteServerAddr needs to be replaced with the IP address you have 
# received from the Docker Terminal
remDr <- remoteDriver(remoteServerAddr = "localhost", port = 4445L, browserName = "firefox")
remDr$open()
Sys.sleep(3)

# Provide the URL and let the driver load it
remDr$navigate("https://www.airbnb.ca/login")
Sys.sleep(3)

# Click on the first of that class, which is continue with email
continue_email <- remDr$findElement(using = 'class', "_bc4egv")
continue_email$clickElement()
Sys.sleep(3)

#Enter email et password, press enter
email_adress <- remDr$findElement(using = 'name', "email")
email_adress$sendKeysToElement(list("shiny.maxbdb3@gmail.com"))
Sys.sleep(3)

password <- remDr$findElement(using = 'name', "password")
password$sendKeysToElement(list("TryScrape2", key = "enter"))
Sys.sleep(5)

#pour shiny.maxbdb et shiny.maxbdb2 c'est ScrapeTry2 le mdp

# Very very useful tool, take a screenshot of where we are on the page

remDr$screenshot(display = TRUE)


# Scrape every hosts photos with progress bar -----------------------------

photo_fun <- function(host_ID){
  
  tryCatch(
    expr = {
      
      remDr$navigate(paste0("https://www.airbnb.ca/users/show/", host_ID))
      
      load_check <- FALSE
      iters <- 0
      while (!load_check && iters <= 5) {
        iters <- iters + 1
        load_check <-
          suppressMessages(
            tryCatch({
              # This element is only true once the page is loaded
              remDr$findElement("xpath", '//*[@data-triggered = "true"]')
              TRUE
            }, error = function(e) {
              # If the key element isn't loaded, wait one second then try again
              Sys.sleep(1)
              FALSE
            }
            ))
      }
    },
    error = function(e){
      return(NA)
    })
  
  tryCatch(
    expr = {
      host_pic <- remDr$findElement(using = 'class', "_9ofhsl")
      
      photo_link <- 
        host_pic$getElementAttribute("src")
      
      Sys.sleep(1)
      
      return(photo_link)
      
    },
    error = function(e){
      return(NA)
    }
  )
}


result <- vector("character", length(host_ID))

time_mean <- 0

for (i in seq_along(host$host_ID)) {
  
  beg <- Sys.time()
  NAs$url[i] <- photo_fun(host$host_ID[i])
  end <- Sys.time()
  
  time_mean <- ((time_mean*(i-1)+(end-beg))/i)
  
  print(paste0(scales::percent(i/length(host$host_ID), accuracy = 0.1), ". This request took ", round(end-beg, digit = 1),
               " seconds. Average request time of ", round(time_mean, digit = 1),
               " seconds. ETC: ", Sys.time()+(time_mean)*(length(host$host_ID)-i)))
}


host_photos <- tibble(host_ID = host_ID, url = unlist(result))


# Save results ------------------------------------------------------------

qsave(host_photos, file = "output/host_photos.qs")
