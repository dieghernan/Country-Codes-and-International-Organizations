#0. Cleaning and set-up R----
rm(list = ls())
library(pacman)
p_load(dplyr,
       rvest,
       jsonlite,
       stringr,
       fuzzyjoin)


#NOT RUN Prev: ISO to Factbook----
# Fact =  read_html("files/fact.html")  %>%
#    html_nodes(xpath = '//*[@id="fieldListing"]') %>%
#    html_table() %>% as.data.frame(stringsAsFactors = F, fix.empty.names =T) %>% select(Country)
# 
# #Wiki
# Wiki <-  read_html("files/ISOWiki.html") %>%
#   html_nodes(xpath = '//*[@id="mw-content-text"]/div/table[2]') %>%
#   html_table() %>%
#   as.data.frame(stringsAsFactors = F, fix.empty.names = F)
# Wiki$Alpha.2.code = ifelse(Wiki$Alpha.3.code == "NAM", "NA", Wiki$Alpha.2.code) 
# nam1=Wiki %>% select(ISO_3166_2=Alpha.2.code,
#                      ISO_3166_3=Alpha.3.code,
#                      Country=English.short.name..using.title.case.)
# Try1=inner_join(Fact,nam1)
# Left1=anti_join(Fact,nam1)
# LeftISO=nam1[!nam1$ISO_3166_3 %in% Try1$ISO_3166_3,]
# Try2=stringdist_inner_join(Left1,LeftISO,distance_col = "dist",max_dist=1)
# Try2=Try2[,1:3]
# names(Try2) =names(Try1)
# Try2=rbind(Try1,Try2)
# Try3=anti_join(Fact,Try2)
# 
# #Statoids
# statoids =  read_html("files/statoids.html")  %>%
#   html_nodes(xpath = '//*[@id="yui-main"]/div/div/table[1]') %>%
#   html_table() %>%
#   as.data.frame(stringsAsFactors = F, fix.empty.names = F)
# st= statoids %>% filter(!A.3 %in% Try2$ISO_3166_3)%>% select(ISO_3166_2=A.2,
#                         ISO_3166_3=A.3,
#                         Country=X249.countries)
# Try4=inner_join(Try3,st)
# Keep4=rbind(Try2,Try4)
# Left4=anti_join(Fact,Keep4)
# 
# #Geonames
# geonames = fromJSON("files/geocountries.json")
# geonames = data.frame(geonames[["geonames"]])
# geonames = geonames %>% filter(!isoAlpha3 %in% Keep4$ISO_3166_3) %>% 
#   select(ISO_3166_2=countryCode,
#          ISO_3166_3=isoAlpha3,
#          Country=countryName)
# 
# Try5=inner_join(Left4,geonames)
# Keep5=rbind(Keep4,Try5)
# Left5=anti_join(Fact,Keep5)
# Left5$search=substr(Left5$Country,1,4)
# Nofind=statoids[!statoids$A.3 %in% Keep5$ISO_3166_3,] %>% select(
#   ISO_3166_2=A.2,
# ISO_3166_3=A.3,
# Country=X249.countries)
# Left5$ISO_3166_3=lapply(1:nrow(Left5), function(x) Nofind[grep(Left5[x,2],Nofind$Country,),2])
# Keep6=Left5[lengths(Left5$ISO_3166_3)==1,]
# Keep6$ISO_3166_3=as.character(Keep6$ISO_3166_3)
# 
# Keep6=left_join(Keep6,Nofind %>% select(
#   ISO_3166_2,ISO_3166_3
# ))
# Keep6=Keep6[,names(Keep6) %in% names(Keep5)]
# Keep6=rbind(Keep5,Keep6)
# #For the rest manual
# Man=Left5[lengths(Left5$ISO_3166_3)!=1,]
# Man$ISO_3166_3=c("MMR","COG",NA,"ATF","VAT","SGS")
# Man=left_join(Man,geonames,by="ISO_3166_3")
# Man=Man[,c(1,3,4)]
# names(Man)=c("Country","ISO_3166_3","ISO_3166_2")
# FactsISO=rbind(Keep6,Man) %>% filter(!is.na(ISO_3166_3)) %>% arrange(ISO_3166_3)
# 
# rm(list=setdiff(ls(),"FactsISO"))
# write.csv(FactsISO,"outputs/bk/CIAFactbookISO.csv",row.names = FALSE)
# 

#A. CIA Factbook----
#Load
Fact = read.csv(
  "outputs/bk/CIAFactbookISO.csv",
  stringsAsFactors = F
)
Orgs =  read_html("files/fact.html")  %>%
      html_nodes(xpath = '//*[@id="fieldListing"]') %>%
      html_table() %>% as.data.frame(stringsAsFactors = F, fix.empty.names =T) 
  
Fact=inner_join(Fact,Orgs)
rm(Orgs)

Fact$International.organization.participation = str_squish(gsub(
  "\\(",
  "/",
  gsub("\\)", "", Fact$International.organization.participation)
))

for (i in 1:nrow(Fact)) {
  org = strsplit(Fact[i, 4], ", ")
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
rm(i,Fact)
Orgs=Orgs[,-c(1,4)]

Orgs$org_name = str_squish(Orgs$org_name)
Orgs$org_name = gsub(" -", "-", gsub("- ", "-", Orgs$org_name))
Orgs$org_name = gsub(" ", "_", Orgs$org_name)

Orgs$org_member=ifelse(is.na(Orgs$org_member),"member",Orgs$org_member)
Orgs[Orgs == ""] <- NA
Orgs$org_name=ifelse(Orgs$org_name=="Australian_Group","Australia_Group",Orgs$org_name)
Orgs$source="CIAFactbook"
Orgs = Orgs %>% filter(!org_name=="Commonwealth_of_Nations")
Orgs$org_name=toupper(Orgs$org_name)

#B. RESTCountries----
RESTCountries = fromJSON("files/rest.json")
RESTCountries = RESTCountries[c("alpha2Code", "alpha3Code", "regionalBlocs")]
RESTCountries$abb = lapply(1:nrow(RESTCountries), function(x)
  RESTCountries$regionalBlocs[[x]][["acronym"]])
RESTCountries = RESTCountries[, -3]
RESTCountries = filter(RESTCountries, !RESTCountries$abb == "NULL")
for (i in 1:nrow(RESTCountries)) {
  f = data.frame(RESTCountries[i, 1:2], orgs = RESTCountries[i, 3],stringsAsFactors = F)
  names(f) = c("ISO_3166_2", "ISO_3166_3", "org_name")
  if (i == 1) {
    TBREST = f
  }
  else  {
    TBREST = rbind(TBREST, f)
  }
  rm(f)
}
rm(i, RESTCountries)
TBREST$org_member="member"
TBREST$source="RESTCountries"
TBREST[TBREST == ""] <- NA
TBREST$org_name=toupper(TBREST$org_name)
Alinfacts=sort(unique(Orgs$org_name))
TBREST=filter(TBREST,!TBREST$org_name %in% Alinfacts)


#C. geonames----
geonames_org = fromJSON("files/geoorgs.json") %>% as.data.frame(stringsAsFactors = F)
geonames_org = geonames_org[c("geonames.toponymName",
                              "geonames.alternateNames",
                              "geonames.cc2")]
names(geonames_org) = c("name", "list", "mem")
geonames_org = filter(geonames_org, !is.na(geonames_org$mem))
geonames_org$abb = lapply(1:nrow(geonames_org), function (x)
  unlist(as.character(geonames_org[x, 2][[1]][["name"]][1])))
geonames_org$fin = ifelse(nchar(geonames_org$abb) > 10,
                          geonames_org$name,
                          geonames_org$abb)
geonames_org$fin = gsub(" ", "_", geonames_org$fin)
geonames_org = geonames_org[c("fin", "mem")]
names(geonames_org)
for (i in 1:nrow(geonames_org)) {
  df = data.frame(geonames_org[i, 1], strsplit(geonames_org[i, 2], ","),stringsAsFactors = F)
  names(df) = c("org_name", "ISO_3166_2")
  if (i == 1) {
    GeoOrg = df
  }
  else  {
    GeoOrg = rbind(GeoOrg, df)
  }
  rm(df)
}
GeoOrg$org_member="member"
GeoOrg$source="geonames"
GeoOrg[GeoOrg == ""] <- NA
rm(i,geonames_org)
GeoOrg$org_name=toupper(GeoOrg$org_name)
Alinfactrest=sort(unique(append(Alinfacts,TBREST$org_name)))
GeoOrg=filter(GeoOrg,!GeoOrg$org_name %in% Alinfacts)
rm(Alinfactrest,Alinfacts)
#D. Custom----
ISOs<-read.csv("outputs/CountryCodes.csv",stringsAsFactors = F) %>% select(
  ISO_3166_2,
  ISO_3166_3,
  currency
)

#Select EZone
Custom <- ISOs %>% 
  filter(currency=="EUR")  %>% select(
    ISO_3166_2,
    ISO_3166_3)
EU=Orgs %>% filter(org_name=="EU") %>% select(ISO_3166_3)
Custom =filter(Custom,Custom$ISO_3166_3 %in% EU$ISO_3166_3)
Custom$org_name="EuroArea"
Custom$org_member="member"
Custom$source="custom"


#E. Joins----
ISOs=ISOs %>% select(ISO_3166_2,ISO_3166_3)
OrgName=sort(unique(Orgs$org_name))
OrgName=sort(unique(append(OrgName,TBREST$org_name)))
OrgName=sort(unique(append(OrgName,Custom$org_name)))
OrgName=as.data.frame(sort(unique(append(OrgName,GeoOrg$org_name))),stringsAsFactors=F)
#Custom
names(OrgName)="org_name"
for (i in 1:nrow(ISOs)){
  a=cbind(ISOs[i,1:2],OrgName,stringsAsFactors=F)
  if (i==1){
    IsoOrgs=a
  } else {
    IsoOrgs=rbind(IsoOrgs,a,stringsAsFactors=F)
  }
  rm(a)
}
rm(OrgName)

#CIAFact
factfin= IsoOrgs %>% filter(org_name %in% Orgs$org_name) %>% left_join(Orgs)
RESTfin= IsoOrgs %>% filter(org_name %in% TBREST$org_name) %>% left_join(TBREST)
Geofin=  IsoOrgs %>% filter(org_name %in% GeoOrg$org_name) %>% left_join(GeoOrg)
CustomFin=IsoOrgs %>% filter(org_name %in% Custom$org_name) %>% left_join(Custom)
sf=rbind(rbind(rbind(factfin,RESTfin),Geofin),CustomFin)
#Clean dup - manual
f=sf %>% count(ISO_3166_2,org_name) %>% filter(n>1) %>% left_join(sf)
f=f[1,]
sf=left_join(sf,f) %>% filter(is.na(n)) 
sf=sf[,-ncol(sf)]
sf$org_member=ifelse(is.na(sf$org_member),"no",sf$org_member)
sf = sf %>% arrange(ISO_3166_3,org_name)

sf %>% count(org_name,source) %>% filter(!is.na(source)) %>% nrow()
length(unique(sf$org_name))


rm(list=setdiff(ls(),"sf"))

# F, Put in vector in the main db----
db=read.csv("outputs/CountryCodes.csv",stringsAsFactors = F)
db$orgs=lapply(1:nrow(db), function(x) as.character(unlist(list(sf[sf$ISO_3166_3 ==db[x,"ISO_3166_3"],"org_name"]))))
db$membership=lapply(1:nrow(db), function(x) as.character(list(sf[sf$ISO_3166_3 ==db[x,"ISO_3166_3"],"org_member"])))


