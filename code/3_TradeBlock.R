#0. Cleaning and set-up R----
rm(list = ls())
library(pacman)
p_load(dplyr,
       rvest,
       jsonlite,
       stringr,
       fuzzyjoin)
#Import ISO relationship
ISOfull = read.csv("outputs/CountryCodes.csv",
                   stringsAsFactors = F)
#Fix Namibia
ISOfull$ISO_3166_2=ifelse(ISOfull$ISO_3166_3=="NAM",
                          "NA",
                          ISOfull$ISO_3166_2)

#Preprocess: Orgs and Abb----
OrgWeb <-  read_html("files/FactAppB.html") %>% 
  html_nodes(xpath = '//*[@id="wfb-text-holder"]/div[2]/section/div[4]') 

iter=length(xml_children(OrgWeb[[1]]))
raworg=lapply(1:iter, function (x)
  xml_contents(xml_child(OrgWeb[[1]], x))[2] %>% html_text()
) %>% unlist() %>% as.data.frame(stringsAsFactors = F) 
names(raworg)="raworg"

raworg$clean =
  str_squish(gsub(
    "\\(",
    "/",
    gsub("\\)",
         "",
         raworg$raworg)
  ))

raworg$fullname=word(raworg$clean, 1, sep = fixed('/'))
raworg$org_name=coalesce(word(raworg$clean, 2, sep = fixed('/')),raworg$fullname) %>%
  str_squish() 

#Final clean
raworg$org_name= gsub(" -", "-", gsub("- ", "-",raworg$org_name))
raworg$org_name=gsub(" ", "_", raworg$org_name)
raworg$org_name = toupper(raworg$org_name)
finorg= raworg %>% select(
  fullname,org_name)

#Dups -first occurence by org name
finorg = finorg %>%
  group_by(org_name) %>%
  arrange(org_name, desc(fullname)) %>%
  filter(row_number() == 1) %>% as.data.frame()

#A. CIA Factbook----
#Load
Fact = read.csv("outputs/bk/FactOrgs.csv",
                stringsAsFactors = F)
#Fix Namibia
Fact$ISO_3166_2[is.na(Fact$ISO_3166_2)] <- "NA"



Fact$Orgs =
  str_squish(gsub(
    "\\(",
    "/",
    gsub("\\)",
         "",
         Fact$Orgs)
  ))

for (i in 1:nrow(Fact)) {
  org = strsplit(Fact[i, 6], ", ")
  n = data.frame(
    name = Fact[i, 1],
    ISO_3166_2 = Fact[i, 2],
    ISO_3166_3 = Fact[i, 3],
    org = unlist(org),
    stringsAsFactors = F
  )
  n$org_name = word(n$org, 1, sep = fixed('/'))
  n$org_member = word(n$org, 2, sep = fixed('/'))
  if (i == 1) {
    Orgs = n
  } else {
    Orgs = rbind(Orgs, n)
  }
  rm(org, n)
}
rm(i, Fact)
Orgs = Orgs[,-c(1, 4)]

Orgs$org_name = str_squish(Orgs$org_name)
Orgs$org_name = gsub(" -", "-", gsub("- ", "-", Orgs$org_name))
Orgs$org_name = gsub(" ", "_", Orgs$org_name)

Orgs$org_member = ifelse(is.na(Orgs$org_member), "member", Orgs$org_member)
Orgs[Orgs == ""] <- NA
Orgs$org_name = ifelse(Orgs$org_name == "Australian_Group",
                       "Australia_Group",
                       Orgs$org_name)
Orgs[Orgs == ""] <- NA
Orgs[Orgs == "Australia_Group"] <- "AG"

Orgs$source = "CIAFactbook"
Orgs = Orgs %>% filter(!org_name == "Commonwealth_of_Nations")
Orgs$org_name = toupper(Orgs$org_name)
#Dups -first occurence by country org
Orgs = Orgs %>%
  group_by(ISO_3166_3, org_name) %>%
  arrange(ISO_3166_3, org_name, org_member) %>%
  filter(row_number() == 1) %>% as.data.frame()

Orgsnames= Orgs %>% distinct(org_name) %>% 
  left_join(finorg)

trytocapblank=Orgsnames %>% distinct(fullname,org_name) %>% filter(is.na(fullname)) %>% select(org_name)
remain=finorg %>% filter(!org_name %in% Orgsnames$org_name )
trytocapblank$try2=substr(trytocapblank$org_name,1,5)
remain$org_name=substr(remain$org_name,1,5)
try2=left_join(trytocapblank,remain,by=c("try2"="org_name"))
Orgsnames=left_join(Orgsnames,try2,by="org_name")
Orgsnames$fullname = coalesce(Orgsnames$fullname.x,Orgsnames$fullname.y)
Orgsnames = Orgsnames %>% select(
  org_name,
  fullname
)

Orgs= left_join(Orgs,
                Orgsnames)
rm(Orgsnames,trytocapblank,remain,try2)


#B. RESTCountries----
RESTCountries = fromJSON("files/rest.json")
RESTCountries = RESTCountries[c("alpha2Code", "alpha3Code", "regionalBlocs")]
#Fix Kosovo
RESTCountries[RESTCountries == "KOS"] <- "XKX"


RESTCountries$abb = lapply(1:nrow(RESTCountries), function(x)
  RESTCountries$regionalBlocs[[x]][["acronym"]])
RESTCountries$abbname=lapply(1:nrow(RESTCountries), function(x)
  RESTCountries$regionalBlocs[[x]][["name"]])

RESTCountries = RESTCountries[, -3]
RESTCountries = filter(RESTCountries, !RESTCountries$abb == "NULL")
for (i in 1:nrow(RESTCountries)) {
  f = data.frame(RESTCountries[i, 1:2],
                 orgs = RESTCountries[i, 3],
                 stringsAsFactors = F)
  names(f) = c("ISO_3166_2", "ISO_3166_3", "org_name")
  if (i == 1) {
    TBREST = f
  }
  else  {
    TBREST = rbind(TBREST, f)
  }
  rm(f)
}
nn= RESTCountries %>% select(abb,abbname)
f=unlist(nn[,1])
d=unlist(nn[,2])
namesorg=cbind(unlist(nn[,1]),d=unlist(nn[,2]))%>%
              as.data.frame(stringsAsFactors = F) %>% 
              distinct()
rm(i, RESTCountries)
TBREST$org_member = "member"
TBREST$source = "RESTCountries"
TBREST[TBREST == ""] <- NA
TBREST$org_name = toupper(TBREST$org_name)
Alinfacts = sort(unique(Orgs$org_name))
TBREST = filter(TBREST,!TBREST$org_name %in% Alinfacts)
TBREST=left_join(TBREST,namesorg,by=c("org_name" = "V1"))
names(TBREST)=names(Orgs)

#C. geonames----
#not needed---
# geonames_org = fromJSON("files/geoorgs.json") %>% as.data.frame(stringsAsFactors = F)
# geonames_org = geonames_org[c("geonames.toponymName",
#                               "geonames.alternateNames",
#                               "geonames.cc2")]
# names(geonames_org) = c("name", "list", "mem")
# geonames_org = filter(geonames_org, !is.na(geonames_org$mem))
# geonames_org$abb = lapply(1:nrow(geonames_org), function (x)
#   unlist(as.character(geonames_org[x, 2][[1]][["name"]][1])))
# geonames_org$fin = ifelse(nchar(geonames_org$abb) > 10,
#                           geonames_org$name,
#                           geonames_org$abb)
# geonames_org$fin = gsub(" ", "_", geonames_org$fin)
# 
# geonames_org = geonames_org[c("fin", "mem","name")]
# 
# 
# 
# for (i in 1:nrow(geonames_org)) {
#   df = data.frame(geonames_org[i, 3],
#                   geonames_org[i, 1],
#                   strsplit(geonames_org[i, 2], ","),
#                   stringsAsFactors = F)
#   names(df) = c("fullname","org_name", "ISO_3166_2")
#   if (i == 1) {
#     GeoOrg = df
#   }
#   else  {
#     GeoOrg = rbind(GeoOrg, df)
#   }
#   rm(df)
# }
# 
# GeoOrg$org_member = "member"
# GeoOrg$source = "geonames"
# GeoOrg[GeoOrg == ""] <- NA
# rm(i, geonames_org)
# GeoOrg$org_name = toupper(GeoOrg$org_name)
# Alinfactrest = sort(unique(append(Alinfacts, TBREST$org_name)))
# GeoOrg = filter(GeoOrg,!GeoOrg$org_name %in% Alinfacts)
# GeoOrg = left_join(GeoOrg,
#                    ISOfull %>%
#                      select(ISO_3166_2,
#                             ISO_3166_3))
# 
# rm(Alinfactrest, Alinfacts)
# 
#D. Custom----

#Select EZone
Custom <- ISOfull %>%
  filter(currency == "EUR")  %>% select(ISO_3166_2,
                                        ISO_3166_3)

EU = Orgs %>% filter(org_name == "EU") %>% select(ISO_3166_3)
Custom = filter(Custom, Custom$ISO_3166_3 %in% EU$ISO_3166_3)
Custom$org_name = "EuroArea"
Custom$fullname="Euro Area"
Custom$org_member = "member"
Custom$source = "custom"
rm(EU)

#E. CSV----
allOrgs = do.call("rbind", list(Orgs, TBREST,  Custom)) #GeoOrg
del = allOrgs %>% count(org_name) %>% filter(n < 2)
allOrgs = allOrgs %>% filter(!org_name == "NONE") %>% filter(!org_name %in% del$org_name) %>% arrange(org_name, ISO_3166_2)
#Last cleanup - man
aa = allOrgs %>% 
  distinct(org_name,
           fullname) %>% 
  filter(is.na(fullname)) %>%
  select(org_name)

aa$full=c("African, Caribbean, and Pacific Group of States ",
    "Eurasian Economic Community ",
    "United Nations Stabilization Mission In Haiti",
    "UN Security Council",
    "United Nations–African Union Mission in Darfur")
allOrgs=left_join(allOrgs,aa)
allOrgs$fullname=coalesce(allOrgs$fullname,allOrgs$full)
allOrgs=allOrgs[,-ncol(allOrgs)]

allOrgs = left_join(allOrgs,
                    ISOfull %>%
                      select(ISO_3166_2, ISO_3166_3, NAME.EN))
#Arrange
allOrgs = allOrgs %>% select(ISO_3166_2,
                             ISO_3166_3,
                             NAME.EN,
                             source,
                             org_name=fullname,
                             org_id=org_name,
                             org_member)


allOrgs[allOrgs == ""] <- NA

write.csv(allOrgs, "outputs/CountrycodesOrgs.csv", row.names = FALSE)
write.csv(
  allOrgs,
  paste("outputs/bk/CountrycodesOrgs_", Sys.Date(), ".csv", sep = ""),
  row.names = FALSE
)
rm(list = ls())

#F. JSON----
#Import ISO relationship
ISOfull = read.csv("outputs/CountryCodes.csv",
                   stringsAsFactors = F)
#Fix Namibia
ISOfull$ISO_3166_2=ifelse(ISOfull$ISO_3166_3=="NAM",
                          "NA",
                          ISOfull$ISO_3166_2)

ISOs = ISOfull %>% select(ISO_3166_2, ISO_3166_3) %>% arrange(ISO_3166_2) %>% 
  filter(!is.na(ISO_3166_2))

allOrgs=read.csv("outputs/CountrycodesOrgs.csv",
                 stringsAsFactors = F)
OrgFull = allOrgs %>% distinct(org_id) %>% arrange(org_id)

alld = do.call("rbind", lapply(1:nrow(ISOs), function(x)
  cbind(
    ISOs[x, ],
    OrgFull,
    stringsAsFactors = F,
    row.names = NULL
  )))
alld = left_join(alld, allOrgs)

#Create vector of orgs and memberships
ISOfull$org_id = list(OrgFull$org_id)
ISOfull$org_member = lapply(1:nrow(ISOfull),
                             function(x)
                               unlist(
                                 list(alld[alld$ISO_3166_2 == ISOfull[x, "ISO_3166_2"],"org_member"])))


#Export to json
initenc = getOption("encoding")
options(encoding = "utf8")
a = toJSON(ISOfull, pretty = T)
write(a, "outputs/Countrycodesfull.json")
write(a,
      paste("outputs/bk/Countrycodesfull_",
            Sys.Date(), ".json", sep = ""))
options(encoding = initenc)
getOption("encoding")

rm(list = ls())


#G. Complementary function----
ISO_memcol = function(df, orgtosearch) {
  ind = match(orgtosearch, unlist(df[1, "org_id"]))
  or = lapply(1:nrow(df), function(x)
    unlist(df[x, "org_member"])[ind])
  or = data.frame(matrix(unlist(or)), stringsAsFactors = F)
  names(or) = orgtosearch
  df2 = as.data.frame(cbind(df, or, stringsAsFactors = F))
  return(df2)
}

#ISOfull = fromJSON("outputs/Countrycodesfull.json")
#ISO_extract = ISO_memcol(ISOfull, "EU")
