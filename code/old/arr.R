#load----
rm(list = ls())
library(pacman)

p_load(sf,
       rnaturalearth,
       dplyr,
       raster,
       png,
       rsvg
       )


crs="+proj=ortho"
#Border
orthoborder=cbind(6378137 * sin(seq(0, 2 * pi, length.out = 1000)),
                  6378137 * cos(seq(0, 2 * pi, length.out = 1000))) %>% 
  st_linestring() %>% st_sfc() %>% st_cast("POLYGON")
st_crs(orthoborder)=crs
#Plot
par(mar = c(0, 0, 0, 0))
plot(orthoborder,col=NA,border="red")
#Grad
grad=as.raster(
  rsvg("https://upload.wikimedia.org/wikipedia/commons/8/8d/Orthographic_gradient.svg")
)
lims=st_bbox(orthoborder)
rasterImage(grad,lims[1],lims[2],lims[3],lims[4])


