#A. Cleaning and set-up R----
rm(list = ls())
dev.off()

XXXXX="dieghernan"
library(pacman)
p_load(dplyr,
       rvest,
       jsonlite)

#Start scrapping data
#a1. lukes githubs----
# https://github.com/lukes/ISO-3166-Countries-with-Regional-Codes/blob
# TODO: Self scrapping the same info

UN = read.csv(
  "https://raw.githubusercontent.com/lukes/ISO-3166-Countries-with-Regional-Codes/master/all/all.csv"
,stringsAsFactors = F,na.strings = "")


#a2. geonames----
url=  paste(
  "http://api.geonames.org/countryInfoJSON?formatted=false&username=",
  XXXXX,
  "&style=full",
  sep = ""
)

download.file(url,
  "files/geocount.json"
)
geonames=fromJSON("files/geocount.json")
geonames=data.frame(geonames[["geonames"]])

#a3 REST Countries
download.file("https://restcountries.eu/rest/v2/all","files/rest.json")
RESTCountries=fromJSON("files/rest.json")
c=RESTCountries$regionalBlocs[[1]]

RESTCountries=ifelse(RESTCountries$regionalBlocs=="List()",NA,RESTCountries$regionalBlocs)
cRESTCountries[15,]
f3=lapply(1:14, function(x) RESTCountries[x,"regionalBlocs"][[1]][["acronym"]])

#b1. Organizations geonames----
url=paste("http://api.geonames.org/searchJSON?formatted=true&username=",XXXXX,"&style=full&fcode=ZN")
download.file(url,
              "files/geocount_org.json"
)
geonames_org=fromJSON("files/geocount_org.json")
geonames_org=data.frame(geonames_org[["geonames"]])
geonames_org=geonames_org[!is.na(geonames_org$cc2),]


