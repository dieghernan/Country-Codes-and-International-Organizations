#0. Cleaning and set-up R----
rm(list = ls())
source("./code/pass.R")
XXXXX = GEONAMES_USER
library(pacman)
p_load(dplyr,
       rvest,
       jsonlite,
       stringr,
       fuzzyjoin)

#A. Scrapping data----
# Could be done by importing but prefers to have it downloaded as backup
#a1. Wikipedia ISO----
url <- "https://en.wikipedia.org/wiki/ISO_3166-1"
download.file(url, "files/ISOWiki.html")

#a2, Wikipedia NUTS and Euroarea----
url = "https://es.wikipedia.org/wiki/Nomenclatura_de_las_Unidades_Territoriales_Estad%C3%ADsticas"
download.file(url, "files/wikinuts.html")


#a3. UN----
download.file("https://unstats.un.org/unsd/methodology/m49/overview/",
              "files/UN.html")

#a4. geonames----
url =  paste(
  "http://api.geonames.org/countryInfoJSON?formatted=false&username=",
  XXXXX,
  "&style=full",
  sep = ""
)
download.file(url, "files/geocountries.json")
url = paste(url, "&lang=es", sep = "")
download.file(url, "files/geocountries_esp.json")
url = paste(
  "http://api.geonames.org/searchJSON?formatted=true&username=",
  XXXXX,
  "&style=full&fcode=ZN"
)
download.file(url, "files/geoorgs.json")

#a5.REST Countries----
download.file("https://restcountries.eu/rest/v2/all", "files/rest.json")

#a6. CIA Factbook----
download.file(
  "https://www.cia.gov/-library/publications/the-world-factbook/fields/317.html",
  "files/fact.html"
)

download.file(
  "https://www.cia.gov/library/publications/resources/the-world-factbook/appendix/appendix-d.html",
  "files/fact_codes.html"
  )

url="https://www.cia.gov/-library/publications/the-world-factbook/appendix/appendix-b.html"
download.file(url,
              "files/FactAppB.html")

#a7. Unicode Languages----
download.file(
  "https://raw.githubusercontent.com/unicode-cldr/cldr-localenames-modern/master/main/en/territories.json",
  "files/en_unicode.json"
)
download.file(
  "https://raw.githubusercontent.com/unicode-cldr/cldr-localenames-modern/master/main/es/territories.json",
  "files/es_unicode.json"
)
#a8. Statoids----
url = "http://www.statoids.com/wab.html"
download.file(url,
              "files/statoids.html")
rm(list = ls())
