### SOURCE ###############################################

library(tidyverse)
library(sf)
library(lubridate)
library(parallel)


if (Sys.info()["sysname"] != "Windows") plan(multicore, 
                                             workers = availableCores())