source("R/01_source.R")


# Load data ---------------------------------------------------------------

property <- qread("output/property.qs", nthreads = 8)
daily <- qread("output/daily.qs", nthreads = 8)
property_nm <- qread("output/property_nm.qs", nthreads = 8)
daily_nm <- qread("output/daily_nm.qs", nthreads = 8)

FREH_nm <- qread("output/FREH_nm.qs", nthreads = 8)
