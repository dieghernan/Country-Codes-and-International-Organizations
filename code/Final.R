#0. Cleaning and set-up R----
rm(list = ls())
XXXXX = "dieghernan"
library(pacman)
p_load(dplyr,
       rvest,
       jsonlite)

#A. Scrapping data----
#a1.Wikipedia
url <- "https://en.wikipedia.org/wiki/ISO_3166-1"
download.file(url, "files/ISOWiki.html")