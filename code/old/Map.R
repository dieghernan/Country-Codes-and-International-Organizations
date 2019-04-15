rm(list = ls())
library(pacman)
p_load(sf,
       rnaturalearth,
       dplyr)
map=ne_countries(50,type="countries",returnclass = "sf")
cutsbbox=5
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
proj = "+proj=laea +lat_0=-10 +lon_0=35 +x_0=0 +y_0=0 +ellps=WGS84 +units=m +no_defs "
map_proj=st_transform(map,crs=proj)
grat_proj=st_transform(grat,crs=proj)
bbox_proj=st_as_sfc(st_bbox(grat_proj),proj)
bbox_proj=st_buffer(st_centroid(bbox_proj),max(abs(st_bbox(grat_proj))))


par(mar = c(0, 0, 0, 0))
plot(bbox_proj,col="#F3F3F3",border=NA)
plot(st_geometry(map_proj),col="#B2B2B2",border=NA,add=T)
plot(grat_proj,col="#999999",add=T)
plot(bbox_proj,col=NA,border ="#999999", lwd=1.1,add=T )
plot(st_geometry(map_proj[map_proj$continent=="Africa",]),col="#346733",border ="#B2B2B2",add=T )

