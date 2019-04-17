#0. Cleaning and set-up R----
rm(list = ls())
library(pacman)
p_load(dplyr,
       rvest,
       jsonlite,
       stringr,
       fuzzyjoin)

#A. Preprocess----
#Create and add codifications
Base_code <-  read_html("files/ISOWiki.html") %>%
  html_nodes(xpath = '//*[@id="mw-content-text"]/div/table[2]') %>%
  html_table() %>%
  as.data.frame(stringsAsFactors = F, fix.empty.names = F)
Base_code$Alpha.2.code = ifelse(Base_code$Alpha.3.code == "NAM", "NA", Base_code$Alpha.2.code)
Base_code$ISO_Official = T
Base_code <- Base_code %>%
  select(
    ISO_3166_1 = Numeric.code,
    ISO_3166_2 = Alpha.2.code,
    ISO_3166_3 = Alpha.3.code,
    ISO_Official,
    name.en.wiki = English.short.name..using.title.case.,
    Independent
  )

#Add UN data
UN =  read_html("files/UN.html")  %>%
  html_nodes(xpath = '//*[@id="downloadTableEN"]') %>%
  html_table() %>%
  as.data.frame(stringsAsFactors = F, fix.empty.names = T)
Names <- as.character(UN[1, ])
names(UN) = Names
UN = UN[-1, ]
UN[UN == ""] <- NA
rm(Names)
UN <- UN %>%
  select(
    ISO_3166_3 = `ISO-alpha3 Code`,
    M49 = `M49 Code`,
    name.en.un = `Country or Area`,
    regioncode.un = `Region Code`,
    regionname.en.un = `Region Name`,
    interregioncode.un = `Intermediate Region Code`,
    interregionname.en.un = `Intermediate Region Name`,
    subregioncode.un = `Sub-region Code`,
    subregionname.en.un = `Sub-region Name`,
    Developed = `Developed / Developing Countries`
  )
UN_ES =  read_html("files/UN.html")  %>%
  html_nodes(xpath = '//*[@id="downloadTableES"]') %>%
  html_table() %>%
  as.data.frame(stringsAsFactors = F, fix.empty.names = T)
Names <- as.character(UN_ES[1, ])
names(UN_ES) = Names
UN_ES = UN_ES[-1, ]
rm(Names)
UN_end = left_join(
  UN,
  UN_ES %>%
    select(
      ISO_3166_3 = `ISO-alpha3 Code`,
      name.es.un = `Country or Area`,
      regionname.es.un = `Region Name`,
      interregionname.es.un = `Intermediate Region Name`,
      subregionname.es.un = `Sub-region Name`
    ),
  by = "ISO_3166_3"
)
rm(UN, UN_ES)
Base_code = full_join(Base_code, UN_end, by = "ISO_3166_3")
Base_code = Base_code[!is.na(Base_code$ISO_3166_3),]
Base_code[Base_code == ""] <- NA

#Add NUTS
NUTS_code = read_html("files/wikinuts.html") %>%
  html_nodes(xpath = '//*[@id="mw-content-text"]/div/table[2]') %>%
  html_table(fill = TRUE) %>%
  as.data.frame(stringsAsFactors = F, fix.empty.names = T) %>%
  select(NUTS = Estado.1 , Pais = Estado)

NUTS_code = NUTS_code[nchar(NUTS_code$NUTS) == 2,]
NUTS_code$ISO_3166_2 = ifelse(NUTS_code$NUTS == "EL",
                              "GR",
                              ifelse(NUTS_code$NUTS == "UK", "GB", NUTS_code$NUTS))
NUTS_code = NUTS_code[,-2]
Base_code = full_join(Base_code, NUTS_code, by = "ISO_3166_2")

#Add geonames
geonames = fromJSON("files/geocountries.json")
geonames = data.frame(geonames[["geonames"]])
geonames[geonames == ""] <- NA
exclude = c("languages", "south", "north", "east", "west")
geonames = geonames[!colnames(geonames) %in% exclude]
rm(exclude)
names(geonames)
names(geonames) = c(
  "continentcode.gn",
  "capital.en.gn",
  "geonameId",
  "ISO_3166_3",
  "FIPS",
  "population.gn",
  "ISO1",
  "area_km2",
  "ISO2",
  "name.en.gn",
  "continentname.en.gn",
  "currencycode"
)
Base_code = full_join(Base_code, geonames, by = "ISO_3166_3")
Base_code$ISO_3166_1 = coalesce(Base_code$ISO_3166_1, as.integer(Base_code$ISO1))
Base_code$ISO_3166_2 = coalesce(Base_code$ISO_3166_2, Base_code$ISO2)
Base_code = Base_code[, !names(Base_code) %in% c("ISO1", "ISO2")]
#Spanish
geonames_es = fromJSON("files/geocountries_esp.json") %>% as.data.frame()
names(geonames_es) = gsub("geonames.", "", names(geonames_es))
include = c("capital", "isoAlpha3", "countryName", "continentName")
geonames_es = geonames_es[, colnames(geonames_es) %in% include]
Base_code = left_join(
  Base_code,
  geonames_es %>%
    select(
      ISO_3166_3 = isoAlpha3,
      capital.es.gn = capital,
      name.es.gn = countryName,
      continentname.es.gn = continentName
    ),
  by = "ISO_3166_3"
)
#Get dependencies
statoids =  read_html("files/statoids.html")  %>%
  html_nodes(xpath = '//*[@id="yui-main"]/div/div/table[1]') %>%
  html_table() %>%
  as.data.frame(stringsAsFactors = F, fix.empty.names = F)
statoids$A.2 = ifelse(statoids$A.3 == "NAM", "NA", statoids$A.2)
statoids = statoids[nchar(statoids$A.2) == 2, ]
statoids[statoids == ""] <- NA
Depend = filter(Base_code, Base_code$Independent == "No")
df = left_join(Depend, statoids, c("ISO_3166_3" = "A.3"))
df = df[, c(3, ncol(df))]
df$ISO2_Sov = str_sub(df$Independent.y,-2)
df = left_join(
  df %>%
    select(ISO_3166_3,
           sovstatus = Independent.y,
           ISO2_Sov),
  Base_code %>%
    select(ISO_3166_2,
           ISO_3166_3.sov = ISO_3166_3)
  ,
  by = c("ISO2_Sov" = "ISO_3166_2")
)
Base_code = left_join(Base_code,
                      df %>%
                        select(ISO_3166_3,
                               ISO_3166_3.sov,
                               sovstatus),
                      by = "ISO_3166_3")

#Unicode names (preferred)
enunicode = fromJSON("files/en_unicode.json", flatten = T)
l = enunicode[["main"]][["en"]][["localeDisplayNames"]][["territories"]]
enunicode = unlist(l) %>% as.data.frame(stringsAsFactors = F)
colnames(enunicode) = "en.unicode"
enunicode$cod = row.names(enunicode)

esunicode = fromJSON("files/es_unicode.json", flatten = T)
l = esunicode[["main"]][["es"]][["localeDisplayNames"]][["territories"]]
esunicode = unlist(l) %>% as.data.frame(stringsAsFactors = F)
colnames(esunicode) = "es.unicode"
esunicode$cod = row.names(esunicode)
unicode = full_join(enunicode, esunicode, by = "cod")
rm(esunicode, enunicode, l)
Base_code = left_join(
  Base_code,
  unicode %>%
    select(
      regioncode.un = cod,
      regionname.en.uc = en.unicode,
      regionname.es.uc = es.unicode
    ),
  by = "regioncode.un"
)

Base_code = left_join(
  Base_code,
  unicode %>%
    select(
      interregioncode.un = cod,
      interregionname.en.uc = en.unicode,
      interregionname.es.uc = es.unicode
    ),
  by = "interregioncode.un"
)

Base_code = left_join(
  Base_code,
  unicode %>%
    select(
      subregioncode.un = cod,
      subregionname.en.uc = en.unicode,
      subregionname.es.uc = es.unicode
    ),
  by = "subregioncode.un"
)

Base_code = left_join(
  Base_code,
  unicode %>%
    select(
      ISO_3166_2 = cod,
      name.en.uc = en.unicode,
      name.es.uc = es.unicode
    ),
  by = "ISO_3166_2"
)

#B. Complete and clean----
#First block: codes
#Second part: regional codes , currency , dependency status and sov
#Third part, names,
#Fourth part: demographics
names(Base_code)
#Blocks 1 and 2
Clean <- Base_code %>%
  select(
    ISO_3166_1,
    ISO_3166_2,
    ISO_3166_3,
    ISO_Official,
    FIPS,
    M49,
    NUTS,
    geonameId,
    continentcode = continentcode.gn,
    regioncode = regioncode.un,
    interregioncode = interregioncode.un,
    subregioncode = subregioncode.un,
    currency = currencycode,
    independent = Independent,
    sovstatus,
    ISO_3166_3.sov
  )
Clean[Clean == ""] <- NA
Clean$ISO_Official = ifelse(is.na(Clean$ISO_Official), FALSE, TRUE)
Clean$independent = ifelse(Clean$independent == "Yes",
                           TRUE,
                           ifelse(is.na(Clean$independent), FALSE, FALSE))
#Block3- names:
Clean$NAME.EN = coalesce(
  Base_code$name.en.uc,
  Base_code$name.en.gn,
  Base_code$name.en.wiki,
  Base_code$name.en.un
)
Clean$CONTINENT.EN = Base_code$continentname.en.gn
Clean$REGION.EN = coalesce(Base_code$regionname.en.uc,
                           Base_code$regionname.en.un)
Clean$INTERREGION.EN = coalesce(Base_code$interregionname.en.uc,
                                Base_code$interregionname.en.un)
Clean$SUBREGION.EN = coalesce(Base_code$subregionname.en.uc,
                              Base_code$subregionname.en.un)
Clean$CAPITAL.EN = Base_code$capital.en.gn
#Spanish
Clean$NAME.ES = coalesce(Base_code$name.es.uc,
                         Base_code$name.es.gn,
                         Base_code$name.es.un)
Clean$CONTINENT.ES = Base_code$continentname.en.gn
Clean$REGION.ES = coalesce(Base_code$regionname.es.uc,
                           Base_code$regionname.es.un)
Clean$INTERREGION.ES = coalesce(Base_code$interregionname.es.uc,
                                Base_code$interregionname.es.un)
Clean$SUBREGION.ES = coalesce(Base_code$subregionname.es.uc,
                              Base_code$subregionname.es.un)
Clean$CAPITAL.ES = Base_code$capital.es.gn
Clean[Clean == ""] <- NA

#Block 4
names(Base_code)
Clean$pop = Base_code$population.gn
Clean$area_km2 = Base_code$area_km2
Clean$Developed = Base_code$Developed
rm(df)
write.csv(Clean,"outputs/Countrycodes.csv",row.names=FALSE)
write.csv(Clean,paste("outputs/bk/Countrycodes_",Sys.Date(),".csv",sep = ""),row.names=FALSE)
#rm(list = ls())

