#A. Cleaning and set-up R----
rm(list = ls())


XXXXX = "dieghernan"
library(pacman)
p_load(dplyr,
       rvest,
       jsonlite)

#Start scrapping data
#a0. ISO Wiki----
url <- "https://en.wikipedia.org/wiki/ISO_3166-1"
download.file(url,
              "files/ISOWiki.html")
ISOwiki <-  read_html("files/ISOWiki.html") %>%
  html_nodes(xpath = '//*[@id="mw-content-text"]/div/table[2]') %>%
  html_table() %>%
  as.data.frame(stringsAsFactors = F, fix.empty.names = F)
ISOwiki$Alpha.2.code = ifelse(ISOwiki$Alpha.3.code == "NAM", "NA", ISOwiki$Alpha.2.code)

ISOwiki[ISOwiki == ""] <- NA
#a0 Statoids----
url = "http://www.statoids.com/wab.html"
download.file(url,
              "files/statoids.html")
statoids =  read_html("files/statoids.html")  %>%
  html_nodes(xpath = '//*[@id="yui-main"]/div/div/table[1]') %>%
  html_table() %>%
  as.data.frame(stringsAsFactors = F, fix.empty.names = F)
statoids$A.2 = ifelse(statoids$A.3 == "NAM", "NA", statoids$A.2)
statoids = statoids[nchar(statoids$A.2) == 2,]
statoids[statoids == ""] <- NA

#a0 UN Methdology----
download.file("https://unstats.un.org/unsd/methodology/m49/overview/",
              "files/UN.html")
UN =  read_html("files/UN.html")  %>%
  html_nodes(xpath = '//*[@id="downloadTableEN"]') %>%
  html_table() %>%
  as.data.frame(stringsAsFactors = F, fix.empty.names = T)
Names <- as.character(UN[1,])
names(UN) = Names
UN = UN[-1,]
UN[UN == ""] <- NA
rm(Names)

#a0. lukes githubs----
# https://github.com/lukes/ISO-3166-Countries-with-Regional-Codes/blob
# TODO: Self scrapping the same info

Luk = read.csv(
  "https://raw.githubusercontent.com/lukes/ISO-3166-Countries-with-Regional-Codes/master/all/all.csv"
  ,
  stringsAsFactors = F,
  na.strings = ""
)
Luk[Luk == ""] <- NA
#a0. geonames----
url =  paste(
  "http://api.geonames.org/countryInfoJSON?formatted=false&username=",
  XXXXX,
  "&style=full",
  sep = ""
)

download.file(url, "files/geocount.json")
geonames = fromJSON("files/geocount.json")
geonames = data.frame(geonames[["geonames"]])
geonames[geonames == ""] <- NA
#a0 REST Countries----
download.file("https://restcountries.eu/rest/v2/all", "files/rest.json")
RESTCountries = fromJSON("files/rest.json")
RESTCountries[RESTCountries == ""] <- NA
#Fix Kosovo
RESTCountries$alpha3Code = ifelse(RESTCountries$alpha3Code == "KOS",
                                  "XKX",
                                  RESTCountries$alpha3Code)


#a0. NUTS----
url = "https://es.wikipedia.org/wiki/Nomenclatura_de_las_Unidades_Territoriales_Estad%C3%ADsticas"
download.file(url, "files/wikinuts.html")
NUTS_code = read_html("files/wikinuts.html") %>%
  html_nodes(xpath = '//*[@id="mw-content-text"]/div/table[2]') %>%
  html_table(fill = TRUE) %>%
  as.data.frame(stringsAsFactors = F, fix.empty.names = T) %>%
  select(NUTS = Estado.1 , Pais = Estado)

NUTS_code = NUTS_code[nchar(NUTS_code$NUTS) == 2,]

#a_fin. Get Codes----
Base <- ISOwiki %>% select(
  ISO1 = Numeric.code,
  ISO2 = Alpha.2.code,
  ISO3 = Alpha.3.code,
  Name_wiki = English.short.name..using.title.case.
)
#M49
Base = full_join(Base, UN[c("ISO-alpha3 Code", "M49 Code", "Country or Area")], by =
                   c("ISO3" = "ISO-alpha3 Code"))
Base$Name_UN = Base$`Country or Area`
Base = Base[, -which(names(Base) == "Country or Area")]
names(geonames)
nam = c("geonameId",
        "fipsCode",
        "isoAlpha3",
        "isoNumeric",
        "countryCode",
        "countryName")
Base = full_join(Base, geonames[nam], by = c("ISO3" = "isoAlpha3"))
Base$Name_geoname = Base$countryName
Base$ISO1 = coalesce(Base$ISO1, as.integer(Base$isoNumeric))
Base$ISO2 = coalesce(Base$ISO2, Base$countryCode)
Base = Base[, -which(names(Base) %in% c("countryCode", "isoNumeric", "countryName"))] %>% arrange(ISO2)

NUTS_code$ISO2 = ifelse(NUTS_code$NUTS == "EL",
                        "GR",
                        ifelse(NUTS_code$NUTS == "UK", "GB", NUTS_code$NUTS))
Base = full_join(Base, NUTS_code, by = "ISO2")
Base$ISO_official = ifelse(Base$ISO3 %in% ISOwiki$Alpha.3.code, TRUE, FALSE)
names(Base)


#Clean and sort
Base_codes = Base %>% select(
  ISO_3166_1 = ISO1,
  ISO_3166_2 = ISO2,
  ISO_3166_3 = ISO3,
  ISO_official,
  M49 = `M49 Code`,
  FIPS = fipsCode,
  NUTS,
  geonameId
)
Base_codes$NAME = coalesce(Base$Name_wiki, Base$Name_UN, Base$Name_geoname, Base$Pais)
Base_codes = Base_codes %>%  filter(!is.na(ISO_3166_3)) %>% arrange(ISO_3166_3)
Base_codes[Base_codes == ""] <- NA

#a1. Info regional----
#Capitals and Cont
Cont <- geonames %>%
  select(
    ISO_3166_3 = isoAlpha3,
    FX = currencyCode,
    gn.ContinentCode = continent,
    l.en.Capital.gn = capital,
    ContinentCode.gn = continent,
    l.en.Continent.gn = continentName,
    area_km2.gn = areaInSqKm,
    population.gn = population
  )
Base_codes = left_join(Base_codes, Cont, by = "ISO_3166_3")
rm(Cont)
url =  paste(
  "http://api.geonames.org/countryInfoJSON?formatted=false&username=",
  XXXXX,
  "&style=full&lang=es",
  sep = ""
)
#Info es
download.file(url, "files/geocount_es.json")
geonames.es = fromJSON("files/geocount_es.json") %>% as.data.frame()
Capitals = geonames.es %>%
  select(
    ISO_3166_3 = geonames.isoAlpha3,
    l.es.Capital.gn = geonames.capital,
    l.es.Continent.gl = geonames.continent
  )

Base_codes = left_join(Base_codes, Capitals)


#a1. UN ----
UNReg = UN %>%
  select(
    ISO_3166_3 = `ISO-alpha3 Code`,
    RegionCode.un = `Region Code`,
    InterRegionCode.un = `Intermediate Region Code`,
    SubRegionCode.un = `Sub-region Code`,
    l.en.Region.un = `Region Name`,
    l.en.InterRegion.un = `Intermediate Region Name`,
    l.en.SubRegion.un = `Sub-region Name`
  )


Base_codes = left_join(Base_codes, UNReg, by = "ISO_3166_3")

UN_ES =  read_html("files/UN.html")  %>%
  html_nodes(xpath = '//*[@id="downloadTableES"]') %>%
  html_table() %>%
  as.data.frame(stringsAsFactors = F, fix.empty.names = T)

names <- as.character(UN_ES[1,])
names(UN_ES) = names
UN_ES = UN_ES[-1,]

UNReg = UN_ES %>%
  select(
    ISO_3166_3 = `ISO-alpha3 Code`,
    l.es.Region.un = `Region Name`,
    l.es.InterRegion.un = `Intermediate Region Name`,
    l.es.SubRegion.un = `Sub-region Name`
  )
Base_codes = left_join(Base_codes, UNReg, by = "ISO_3166_3")
Base_codes[Base_codes == ""] <- NA

#Real names for countries
#Unicode ----
download.file(
  "https://raw.githubusercontent.com/unicode-cldr/cldr-localenames-modern/master/main/en/territories.json",
  "files/en_unicode.json"
)

download.file(
  "https://raw.githubusercontent.com/unicode-cldr/cldr-localenames-modern/master/main/es/territories.json",
  "files/es_unicode.json"
)
enunicode = fromJSON("files/en_unicode.json", flatten = T)
l = enunicode[["main"]][["en"]][["localeDisplayNames"]][["territories"]]
enunicode = unlist(l) %>% as.data.frame(stringsAsFactors = F)
names(enunicode) = "en"
enunicode$key = row.names(enunicode)
esunicode = fromJSON("files/es_unicode.json", flatten = T)
esunicode = unlist(esunicode[["main"]][["es"]][["localeDisplayNames"]][["territories"]]) %>% as.data.frame(stringsAsFactors = F)
names(esunicode) = "es"
esunicode$key = row.names(esunicode)
unicode = full_join(enunicode, esunicode)




library(sf)

a=st_read("~/R/mapslib/EUROSTAT/CNTR_RG_10M_2016_3857.shp")
b=a[!a$ISO3_CODE %in% Base_codes$ISO_3166_3,]

#-----

library(rnaturalearth)
maptest=ne_countries(50, returnclass = "sf")
maptest=maptest[,"iso_a3"]
maptest = left_join(maptest,Base_codes, by=c("iso_a3"="ISO_3166_3"))


plot(maptest["un.InterRegionCode"], key.pos = NULL)
###
a=maptest[maptest$un.RegionCode=="150",]

a <- Base_codes %>% count(gn.ContinentCode,un.RegionCode,aa.x)

graticules <- ne_download(type = "graticules_15", category = "physical",returnclass = "sf") %>% 
  st_transform(crs = "+proj=robin") %>% 
  st_geometry()

url = paste(
  "http://api.geonames.org/searchJSON?formatted=true&username=",
  XXXXX,
  "&style=full&fcode=CONT"
)
download.file(url,
              "files/geocont_org.json")
geonames_cont = fromJSON("files/geocont_org.json") %>% as.data.frame()

geonames_cont$namesp=lapply(1:7, function(x) geonames_cont$geonames.alternateNames[[x]][["name"]][nam[[x]][["lang"]]=="es"])
geonames_cont$namen=lapply(1:7, function(x) geonames_cont$geonames.alternateNames[[x]][["name"]][nam[[x]][["lang"]]=="en"])





Cont <- geonames %>%
  select(
    ISO_3166_3 = isoAlpha3,
    gn.Continent = continentName,
    gn.ContinentCode = continent
  )
Cont$

Base_codes = left_join(Base_codes, Cont, by = "ISO_3166_3")





#b1. Organizations geonames----
url = paste(
  "http://api.geonames.org/searchJSON?formatted=true&username=",
  XXXXX,
  "&style=full&fcode=ZN"
)
url
download.file(url,
              "files/geocount_org.json")
geonames_org = fromJSON("files/geocount_org.json")
geonames_org = data.frame(geonames_org[["geonames"]], stringsAsFactors = F)
geonames_org = geonames_org[!is.na(geonames_org$cc2),]
OrgGeo = data.frame(
  Org = gsub(" ", "", geonames_org$asciiName),
  geonames_org$cc2,
  stringsAsFactors = F
)

for (i in 1:nrow(OrgGeo)) {
  n = gsub(',', "|", OrgGeo[i, 2])
  org = strsplit(n, "[|]")
  p = data.frame(Org = OrgGeo[i, 1],
                 ISO = unlist(org),
                 stringsAsFactors = F)
  if (i == 1) {
    OrgGeoFin = p
  } else {
    OrgGeoFin = rbind(OrgGeoFin, p)
  }
  rm(org, n, p)
}

rm(OrgGeo)
#b1. factbook----
download.file(
  "https://www.cia.gov/-library/publications/the-world-factbook/fields/317.html",
  "files/fact.html"
)
Fact =  read_html("files/fact.html")  %>%
  html_nodes(xpath = '//*[@id="fieldListing"]') %>%
  html_table() %>% as.data.frame(stringsAsFactors = F, fix.empty.names =
                                   T)

for (i in 1:nrow(Fact)) {
  org = strsplit(Fact[i, 2], ", ")
  n = data.frame(name = Fact[i, 1],
                 org = unlist(org),
                 stringsAsFactors = F)
  n$org = lapply(1:nrow(n), function(x)
    strsplit(n[x, 2], "[()]")[[1]][[1]])
  
  if (i == 1) {
    Orgs = n
  } else {
    Orgs = rbind(Orgs, n)
  }
  rm(org, n)
}
rm(i)
Orgs$org = gsub(" ", "", as.character(Orgs$org))
Org_Index <-
  Orgs %>%  count(org) %>% filter(n > 3) %>% arrange(desc(n))

#-----



#Labesl continent
url =  paste(
  "http://api.geonames.org/countryInfoJSON?formatted=false&username=",
  XXXXX,
  "&style=full&lang=es",
  sep = ""
)

download.file(url, "files/geocount_es.json")
geonames.es = fromJSON("files/geocount_es.json") %>% as.data.frame()
Capitals= geonames.es %>% 
  select(isoAlpha3,
         currencyCode,
         l.en.gn.Capital=geonames.capital
  )


url = paste(
  "http://api.geonames.org/searchJSON?formatted=true&username=",
  XXXXX,
  "&style=full&fcode=CONT"
)
download.file(url,
              "files/geocont_org.json")

geonames_cont = fromJSON("files/geocont_org.json") %>% as.data.frame()
geonames_cont$l.es.gn.ContinentName = lapply(1:nrow(geonames_cont), function(x)
  geonames_cont$geonames.alternateNames[[x]][["name"]][geonames_cont$geonames.alternateNames[[x]][["lang"]] ==
                                                         "es"])
geonames_cont$l.en.gn.ContinentName = lapply(1:nrow(geonames_cont), function(x)
  geonames_cont$geonames.alternateNames[[x]][["name"]][geonames_cont$geonames.alternateNames[[x]][["lang"]] ==
                                                         "en"])
Base_codes=left_join(Base_codes,
                     geonames_cont[,c("geonames.continentCode","l.es.gn.ContinentName","l.en.gn.ContinentName")],
                     by=c("gn.ContinentCode"="geonames.continentCode")
)
#Capitals----
geonames.es$geonames.capital
Capitals <- geonames.es %>% select(
  isoAlpha3= geonames.isoAlpha3,
  l.es.gn.Capital=capital
) %>% full_join(Capitals, by ="isoAlpha3")

names(geonames.es)
