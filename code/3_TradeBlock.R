#0. Cleaning and set-up R----
rm(list = ls())
library(pacman)
p_load(dplyr,
       rvest,
       jsonlite,
       stringr,
       fuzzyjoin)




#A. CIA Factbook----
#Pretty manual when catching ISO3 codes - commmented by now
# Factbook
# Fact =  read_html("files/fact.html")  %>%
#   html_nodes(xpath = '//*[@id="fieldListing"]') %>%
#   html_table() %>% as.data.frame(stringsAsFactors = F, fix.empty.names =T)
#
# ISOFACT=left_join(Fact,Clean[c("ISO_3166_3","NAME.EN")],by=c("Country"="NAME.EN"))
# FactKeep=ISOFACT[!is.na(ISOFACT$ISO_3166_3),]
# ISOleft1=ISOFACT[is.na(ISOFACT$ISO_3166_3),-3]
# ISOSearch=Base_code[!Base_code$ISO_3166_3 %in% FactKeep$ISO_3166_3,]
# ISOSearch2=stringdist_left_join(ISOleft1,
#                                 ISOSearch %>% select(
#                                   name.en.wiki,
#                                   ISO_3166_3
#                                 )
#                                 ,by=c("Country"="name.en.wiki"),distance_col = "aa",max_dist=2)
# FactKeep=rbind(FactKeep,ISOSearch2[!is.na(ISOSearch2$ISO_3166_3),c(1,2,4)])
# ISOleft2=ISOSearch2[is.na(ISOSearch2$ISO_3166_3),c(1,2)]
# ISOleft2$new=substr(ISOleft2$Country,1,4)
# ISOSearch2=Base_code[!Base_code$ISO_3166_3 %in% FactKeep$ISO_3166_3,]
# ISOleft2$cand=lapply(1:nrow(ISOleft2), function(x) unlist(list(ISOSearch2[grep(ISOleft2[x,3],ISOSearch2$name.en.uc),"ISO_3166_3"])))
# ISOleft2$cand2=lapply(1:nrow(ISOleft2), function(x) unlist(list(ISOSearch2[grep(ISOleft2[x,3],ISOSearch2$name.es.uc),"ISO_3166_3"])))
# ISOleft2=stringdist_left_join(ISOleft2,
#                                ISOSearch2 %>% select(
#                                  ISO_3166_3,
#                                  name.en.gn,
#                                  name.en.uc,
#                                  name.en.un,
#                                  name.en.wiki
#                                ),
#                                by=c("Country"="name.en.uc"),
#                                max_dist=4
#                                )
#
# ISOleft2[ISOleft2=="character(0)"]<- ""
# ISOleft2$candfin1=as.character(ifelse(!nchar(ISOleft2$cand)==3,"",ISOleft2$cand))
# ISOleft2$candfin2=as.character(ifelse(!nchar(ISOleft2$cand2)==3,"",ISOleft2$cand2))
# ISOleft2[ISOleft2==""]<- NA
# ISOleft2$final=coalesce(ISOleft2$candfin1,ISOleft2$candfin2,ISOleft2$ISO_3166_3)
# ISOkeep2=ISOleft2[!is.na(ISOleft2$final),c(1,2,ncol(ISOleft2))]
# names(ISOkeep2)=names(FactKeep)
# ISOleft3=ISOleft2[is.na(ISOleft2$final),]
# rm(ISOleft1,ISOleft2,ISOSearch,ISOSearch2)
# ISOSearch3=ISOSearch2=Base_code[!Base_code$ISO_3166_3 %in% FactKeep$ISO_3166_3,]
# ISOleft3=ISOleft3[,1:2]
# ISOleft3$ISO_3166_3=c("","ATF","VAT","PRK","KOR")
# Fact=rbind(FactKeep,ISOkeep2)
# Fact=rbind(Fact,ISOleft3)
# Fact[Fact==""]<- NA
# Fact[Fact=="none"]<- NA
# rm(FactKeep,ISOFACT,ISOkeep2,ISOleft3,ISOSearch2,ISOSearch3)
# #Write csv
# rownames(Fact)=1:nrow(Fact)
#write.csv(Fact,"outputs/CIAFactbookOrgISO.csv")
#Load
Fact = read.csv(
  "outputs/CIAFactbookOrgISO.csv",
  row.names = "X",
  stringsAsFactors = F
)
Fact$International.organization.participation = str_squish(gsub(
  "\\(",
  "/",
  gsub("\\)", "", Fact$International.organization.participation)
))
for (i in 1:nrow(Fact)) {
  org = strsplit(Fact[i, 2], ", ")
  n = data.frame(
    name = Fact[i, 1],
    ISO_3166_3 = Fact[i, 3],
    org = unlist(org),
    stringsAsFactors = F
  )
  n$org_name = word(n$org, 1, sep = fixed('/'))
  n$org_member = word(n$org, 2, sep = fixed('/'))
  n = n[,-3]
  if (i == 1) {
    Orgs = n
  } else {
    Orgs = rbind(Orgs, n)
  }
  rm(org, n, Fact)
}
rm(i)
Orgs$org_name = str_squish(Orgs$org_name)
Orgs$org_name = gsub(" -", "-", gsub("- ", "-", Orgs$org_name))
Orgs$org_name = gsub(" ", "_", Orgs$org_name)
Orgs=Orgs[,-1]
Orgs$source="CIAFactbook"

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
TBREST$source="RESTCountries"
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
GeoOrg$source="geonames"
rm(i,geonames_org)


#----


b=a$list[["name"]][1]
b[["name"]][1]
#Select organisms----
WikiTrade <-  read_html("files/wikitrade.html") %>%
  html_nodes(xpath = '//*[@id="mw-content-text"]/div/table[3]') %>%
  html_table(fill=T) %>%
  as.data.frame(stringsAsFactors = F, fix.empty.names = F)

ToSearch=data.frame(Orgs=sort(unique(append(WikiTrade[,1],WikiTrade[,2]))))
ToSearch$Orgs=gsub(" ","_",ToSearch$Orgs)


Others=fromJSON("files/rest.json")
a=Others[20,]
Others$abb=lapply(1:nrow(Others), function (x) as.character(Others$regionalBlocs[[x]][["acronym"]]))
summary(Others)
str_squish(pp[18,1])
max(lengths(Others$abb))
f=n$org2
s=f[5]
s[[2]]
k <- strsplit(j,"[()]")
j=n$org
j
wri

#Search tradeblocks
geonames_org = fromJSON("files/geoorgs.json")
geonames_org = data.frame(geonames_org[["geonames"]], stringsAsFactors = F)
c=geonames_org[,"alternateNames"]
s=lapply(1:nrow(geonames_org),function(x) c[[x]][["name"]][1])
names(s)=geonames_org$geonameId
df=data.frame(unlist(s),stringsAsFactors = F)
names(df)="TB"
df$key=as.integer(row.names(df))
df=left_join(df, geonames_org %>%
               select(
                 key=geonameId,
                 name=asciiName,
                 members=cc2
               ),
             by="key")

df=df[!is.na(df$members),]
df$TB=ifelse(nchar(df$TB)>10,gsub(" ","",df$name),df$TB)
df$n=lapply(1:nrow(df), function(x) nchar(df[x,"members"]))
df=filter(df,df$n>2)  
df$members_list=strsplit(df[,4],",")
org=data.frame(ORG=df$TB,stringsAsFactors = F)
org$members=df$members_list
rm(geonames_org,df,c,s)

