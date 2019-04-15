#0. Cleaning and set-up R----
rm(list = ls())
XXXXX = "dieghernan"
library(pacman)
p_load(dplyr,
       rvest,
       jsonlite,
       stringr,
       fuzzyjoin)




#D. Trade Blocks----
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
  n = n[, -3]
  if (i == 1) {
    Orgs = n
  } else {
    Orgs = rbind(Orgs, n)
  }
  rm(org, n)
}
rm(i)
Orgs$org_name=str_squish(Orgs$org_name)
Orgs$org_name=gsub(" -","-",gsub("- ","-",Orgs$org_name))
Orgs$org_name=gsub(" ","_",Orgs$org_name)

#Select organisms
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

