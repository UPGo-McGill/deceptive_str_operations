### SOURCE ###############################################

library(tidyverse)
library(sf)
library(lubridate)
library(parallel)
library(qs)
library(patchwork)


if (Sys.info()["sysname"] != "Windows") plan(multicore, 
                                             workers = availableCores())

if (memory.limit() < 20000) memory.limit(size = memory.limit()*3)

# color_palette <- c("#f7c9cc", "#9bae98", "#f9d4af", "#74131c", "#274006", "#193b53")
color_palette <- c("#ffb56b", "#800020",
                   "#6BB5FF", "#a05aa3",
                   "#bc365d", "#1f005c")

scales::show_col(c(color_palette))

gp_gradient1 <- color_palette[c(3,4,2)]
gp_gradient2 <- color_palette[c(1,5,6)]
gp_duo1 <- color_palette[c(3,1)]
gp_duo2 <- color_palette[c(1,4)]
gp_duo3 <- color_palette[c(3,4)]
gp_duo4 <- color_palette[c(1,5)]

scales::show_col(gp_duo4)

colfunc <- colorRampPalette(gp_gradient2)
plot(rep(1,150),col=colfunc(150),pch=19,cex=3,type="p")

