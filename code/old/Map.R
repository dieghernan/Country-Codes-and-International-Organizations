#load----
rm(list = ls())
library(pacman)

p_load(sf,
       rnaturalearth,
       dplyr,lwgeom)

f=paste(tempdir(),"a.zip",sep="/")
        
        
download.file("https://ec.europa.eu/eurostat/cache/GISCO/distribution/v2/countries/shp/CNTR_RG_60M_2016_4326.shp.zip",
              f)
unzip(f,exdir=tempdir())
map=st_read(paste(tempdir(),"CNTR_RG_60M_2016_4326.shp",sep="/"))
rm(f)
#Orthomap----
#crs = "+proj=ortho +lon_0=-70 +lat_0=-15"
crs = "+proj=ortho +lon_0=-100 +lat_0=30"
sphere_ort <- st_graticule(
  lon = seq(-180, 180, 2),
  lat = seq(-89, 89, 2),
  ndiscr = 1000,
  margin = 10e-6
) %>%
  st_transform(crs = crs) %>%
  st_convex_hull() 
sphere = st_transform(sphere_ort,
                      4326) %>%
  st_buffer(3) %>%
  st_combine() %>%
  st_union() 
map=st_transform(map,4326)
sphere=st_crop(sphere,st_bbox(map))
map_dis=st_cast(map,"POLYGON")
map_crop=st_intersection(map_dis,sphere)
grat=st_geometry(st_graticule(lon=seq(-180,180,30),lat=seq(-90,90,30))) 


#Transform
bg=sphere_ort %>% st_buffer(100) %>% st_combine() %>% st_union()
map_ortho=st_transform(map_crop,crs=crs)
valid = st_is_valid(map_ortho)
map_ortho=st_buffer(map_ortho[!is.na(valid),], 0.0)
#map_ortho=st_make_valid(map_ortho)
grat_ortho = grat %>% st_transform(crs = crs)

par(mar = c(0, 0, 0, 0))
plot(st_geometry(bg),border = NA,col="#FDFDFD")
plot(st_geometry(map_ortho),add=T,col="#B9B9B9",border="#FFFFFF",lwd=0.1)
plot(st_geometry(bg),border = "#AAAAAA",col=NA,lwd=1.5,add=T)
#plot(st_geometry(map_ortho[map_ortho$continent=="South America",]),col="#346733",border="#B9B9B9",add=T,lwd=0.3)
grat_ortho=st_make_valid(grat_ortho) %>% st_cast()
plot(grat_ortho,col="#c4c4c4",add=T,lwd=0.3)
plot(f[[16]],add=T)
lengths(grat_ortho)
grat_ortho[]
f=grat_ortho[lengths(grat_ortho)>2]
a=class(grat_ortho[[14]])
f[[16]]
a=st_coordinates(f[[16]]) %>% as.data.frame()
!is.na(ifelse(a$X<0 & lag(a$X,1)>0,1,NA)*1:nrow(a))
max=max(a$X)
a$aa=a$X>lag(a$X,1)

#----
st_bbox(map)
st_bbox(sphere)
st_bbox(st_graticule(
  ndiscr = 1000, margin = 10e-6))
map_ort = st_transform(map, crs = crs)

sphere_ort <- st_graticule(
    lon = seq(-180, 180, 2),
   lat = seq(-89, 89, 2),
     ndiscr = 1000, margin = 10e-6) %>%
  st_transform(crs = crs) %>%
  st_convex_hull() 
sphere = st_transform(sphere_ort, st_crs(map))
plot(st_geometry(sphere_ort))
plot(st_geometry(sphere))
st_bbox(sphere)
sphere=sphere %>% st_buffer(2)  %>% st_combine() %>% st_union()
plot(sphere,axes=T)
a=st_crop(sphere,st_bbox(map))
plot(a)
plot(st_geometry(map),add=T)
b=st_crop(st_geometry(map),st_geometry(a))
map_dis=st_cast(map,"POLYGON")
c=st_intersection(map_dis,a)
b=st_crop(c,a)
plot(st_geometry(b))

plot(st_geometry(st_transform(b,crs=crs)))

plot(st_geometry(c),add=T)
plot(b)
plot(b)
mapcrop=st_crop(map,sphere)
plot(st_transform(mapcrop,crs=crs))

plot(st_geometry(mapcrop))

plot(st_geometry(map))
plot(sphere,add=T)

plot(sphere_ort[0])
plot(st_geometry(map))
plot(sphere, add = T)
plot(sphere)

a <- st_graticule(ndiscr = 1000,
                  margin = 10e-6) %>%
  st_transform(crs = crs) %>%
  st_convex_hull() %>% 
  t_buffer(100) %>% 
  st_union()

  
  
  
  
  st_graticule(
#  lon = seq(-180, 180, 5),
#  lat = seq(-90, 90, 5),
  ndiscr = 1000,
  margin = 10e-6
) %>%
  st_transform(crs = crs)%>%
  st_convex_hull() %>% st_buffer(100) %>% st_union()

c=st_transform(a,4326)
plot(st_geometry(map))
plot(c,add=T)
plot(st_geometry(map))
plot(st_transform(a,4326),add=T)


plot(a[0],axes=T)
plot(sphere_ort)
cent=st_centroid(st_as_sfc(st_bbox(sphere_ort),crs=crs))
cent=st_buffer(cent,min(abs(st_bbox(sphere_ort))))
plot(cent)
cent_un=st_transform(cent,3857)
plot(st_geometry(map))
plot(cent_un,add=T)

plot(cent,add=T,border="green")
min(abs(st_bbox(sphere_ort)))
plot(st_geometry(map))
plot(st_transform(sphere_ort,4326),add=T)
mapcrop=st_crop(map,st_transform(sphere_ort,4326))
plot(mapcrop[0])
plot(sphere_ort)
plot(st_transform(mapcrop,crs=crs),add=T)

grat=st_geometry(st_graticule(lon=seq(-180,180,30),lat=seq(-90,89,30)))
plot(st_geometry(map))
st_crs(map)

crs= "+proj=laea +lon_0=20 +lat_0=0 "
sphere <- st_graticule(ndiscr = 100, margin = 10e-6) %>%
  st_transform(crs = crs) %>%
  st_convex_hull() %>% st_buffer(3) %>% st_union()
plot(sphere)
map_proj=map %>% st_transform(crs)
grat_proj=grat %>% st_transform(crs)

plot(st_geometry(map_proj),add=T)
a=st_as_sfc(st_bbox(map_proj)*1.2)
plot(a)
plot(st_geometry(map_proj),add=T,col="blue")

plot(grat_proj,add=T)

a=st_bbox(map)-c(-120,0,60,0)
a=st_as_sfc(a)
st_crs(a)=4326
plot(a)
plot(st_geometry(map),add=T)
map2_crop=st_crop(map,a)



plot(sphere)
c=st_centroid(sphere)
a=st_bbox(sphere)
a
plot(st_geometry(sphere),border=NA,col="red")
plot(st_geometry(map2),add=T,col="green")
sf2=st_transform(sphere,4326)
plot(st_geometry(sf2),axes=T)
st_bbox(sf2)
st_crs(sphere)
st_crs(map2)

sphere <- st_graticule(st_transform(map, crs = crs)) %>%
  st_convex_hull() %>%
  summarise(geometry = st_union(geometry))
plot(st_geometry(sphere),col="blue")
map2=st_transform(map,crs = crs)

cutsbbox=5
st_crs(map)
bbox=cbind(-180,seq(-90,90,cutsbbox))
bbox=rbind(bbox,cbind(seq(-180,180,cutsbbox),90))
bbox=rbind(bbox,cbind(180,seq(90,-90,-cutsbbox)))
bbox=rbind(bbox,cbind(seq(180,-180,-cutsbbox),-90))
bbox=st_sfc(st_polygon(list(bbox)),crs=st_crs(map))
grat=st_geometry(st_graticule(lon=seq(-180,180,30),lat=seq(-90,89,30)))


#Plot wiki plain
proj="+proj=robin"
map_proj=st_transform(map,crs=proj)
grat_proj=st_transform(grat,crs=proj)
bbox_proj=st_transform(bbox,crs=proj)
par(mar = c(0, 0, 0, 0))
plot(bbox_proj,col="#F3F3F3",border=NA)
plot(st_geometry(map_proj),col="#B2B2B2",border=NA,add=T)
plot(grat_proj,col="#999999",add=T)
plot(bbox_proj,col=NA,border ="#999999", lwd=1.1,add=T )
plot(st_geometry(map_proj[map_proj$continent=="Asia",]),col="#346733",border ="#B2B2B2",add=T )

#Plot wiki sfere
proj = "+proj=ortho +lon_0=30 +lat_0=40"
map_proj=st_transform(map,crs=proj)
grat_proj=st_transform(grat,crs=proj)
bbox_proj=st_as_sfc(st_bbox(map_proj),proj)
bbox_proj=st_buffer(st_centroid(bbox_proj),max(abs(st_bbox(map_proj))))
st_bbox(grat_proj)
plot(map_proj[0])
par(mar = c(0, 0, 0, 0))
plot(bbox_proj,col="#F3F3F3",border=NA)
plot(st_geometry(map_proj),col="#B2B2B2",border=NA,add=T)
plot(grat_proj,col="#999999",add=T)

plot(st_geometry(map_proj[map_proj$continent=="Africa",]),col="#346733",border ="#B2B2B2",add=T )


library(ggplot2)
library(sf)
library(rnaturalearth)
library(dplyr)
st_crs(ctrys50m)
crs <- "+proj=ortho +lat_0=0 +lon_0=20"
ctrys50m <- ne_countries(scale = 50, type = "countries", returnclass = "sf") %>%
  select(iso_a3, iso_n3, admin)


map=st_transform(ctrys50m,crs) %>% st_buffer(100)
sphere <- st_graticule(ndiscr = 10000, margin = 10e-6) %>%
  st_transform(crs = crs) %>%
  st_convex_hull() %>% st_buffer(100) %>% st_union()
plot(sphere)
mapnew=st_crop(map,sphere) 
plot(sphere)
plot(st_geometry(mapnew),add=T)


%>%
  summarise(geometry = st_union(geometry)) 

c=st_cast(sphere)
c$a=as.numeric(st_area(c))
c2=filter(c,c$a>0) %>% st_buffer(100) %>% st_union() 
st_crs(c2)
plot(c2)
plot(st_geometry(map),add=T)
st_crs(ctrys50m)
c3=st_transform(c2,4326)
plot(st_geometry(c3))
plot(st_geometry(ctrys50m,add=T))
