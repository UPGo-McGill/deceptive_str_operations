### SOURCE ###############################################

library(tidyverse)
library(sf)
library(lubridate)
library(parallel)


if (Sys.info()["sysname"] != "Windows") plan(multicore, 
                                             workers = availableCores())

if (memory.limit() < 20000) memory.limit(size = memory.limit()*3)
