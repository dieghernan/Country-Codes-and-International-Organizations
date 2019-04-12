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


#a1. NUTS----
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


1#Clean and sort
Base_codes = Base %>% select(
  ISO_3166_1 = ISO1,
  ISO_3166_2 = ISO2,
  ISO_3166_3 = ISO3,
  ISO_official,
  M49 = `M49 Code`,
  FIPS = fipsCode,
  NUTS,
  geonameId
) %>%
  arrange(ISO_3166_3)
Name = Base %>% select(f = coalesce(Name_wiki, Name_UN))
Base_codes$NAME = coalesce(Base$Name_wiki, Base$Name_UN, Base$Name_geoname, Base$Pais)
#Info regional

Cont <- geonames %>%
  select(
    ISO_3166_3 = isoAlpha3,
    Continent_geonames = continentName,
    ContinentCode_geonames = continent
  )

Base_codes = left_join(Base_codes, Cont, by = "ISO_3166_3")

Base_codes[Base_codes == ""] <- NA




#b1. Organizations geonames----
url = paste(
  "http://api.geonames.org/searchJSON?formatted=true&username=",
  XXXXX,
  "&style=full&fcode=ZN"
)
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
