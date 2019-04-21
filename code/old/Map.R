#load----
rm(list = ls())
library(pacman)

p_load(sf,
       rnaturalearth,
       dplyr)

#Inputs
st_orthoproj <- function(sf, xinit, yinit) {
  oldw <- getOption("warn")
  options(warn = -1)
  #Arrange map
  sfinit = sf
  clon = xinit
  #This minimize an error when cropping
  if (yinit == 0) {
    clat = 0.001
  } else {
    clat = yinit
  }
  crs.out = paste("+proj=ortho +lon_0=", clon, " +lat_0=", clat, sep = "")
  
  #Border Projected map
  R = 6378137 #Earth radius as per the internal maths
  x = R * sin(seq(0, 2 * pi, length.out = 1000))
  y = R * cos(seq(0, 2 * pi, length.out = 1000))
  border = cbind(x, y) %>%
    st_multipoint() %>%
    st_sfc(crs = crs.out) %>%
    st_cast("POINT") %>%
    st_sf(ind_ring = 1:length(x))
  
  #Prepare cut
  border_un = st_transform(border, 4326) %>% st_geometry()
  coord = st_coordinates(border_un) %>% as.data.frame() %>% arrange(X, Y)
  bbox = st_bbox(border_un)
  #Complete polygon
  if (clat > 0) {
    a1 = bbox["ymax"]
    a2 = 90
    
  } else {
    a1 = bbox["ymin"]
    a2 = -90
  }
  newdots = data.frame(X = bbox["xmax"], Y = a1)
  newdots = rbind(newdots, c(bbox["xmax"], a2))
  newdots = rbind(newdots, c(bbox["xmin"], a2))
  newdots = rbind(newdots, c(bbox["xmin"], a1))
  coord2 = rbind(coord, newdots)
  pol = as.matrix(coord2) %>% st_linestring() %>% st_cast("POLYGON") %>% st_sfc()
  st_crs(pol) = 4326
    sfcrop = st_intersection(sfinit, pol)
  plot(pol,col="red")
  plot(st_geometry(sfcrop),add=T)
  #Orthogonal projection
  sfproj = sfcrop %>% st_transform(crs.out)
  #Fix errors
  ptr = st_buffer(sfproj[!is.na(st_is_valid(sfproj)),], 0.0)
  options(warn = oldw)
  return(ptr)
}
cntry = ne_countries(110, "countries", returnclass = "sf")
a2=st_orthoproj(cntry,-90,50)

#Border
orthoborder=cbind(6378137 * sin(seq(0, 2 * pi, length.out = 1000)),
         6378137 * cos(seq(0, 2 * pi, length.out = 1000))) %>% 
  st_linestring() %>% st_sfc() %>% st_cast("POLYGON")
st_crs(orthoborder)=st_crs(a2)

#Grid
grid=st_graticule(lon=seq(-180,180,30),lat=seq(-90,90,30),ndiscr=1000, margin = 10e-9) %>% 
  st_transform(st_crs(a2)) 
grid=grid[as.integer(st_length(grid))>0,]
plot(st_geometry(grid))
gride=filter(grid,grid$type=="E")
gridn=filter(grid,grid$type=="N")
for (i in 1:nrow(gridn)){
  a=gridn[i,]
  c=st_coordinates(a) %>% 
    as.data.frame() %>% 
    arrange(X) %>% 
    as.matrix()%>% 
    st_linestring() %>% 
    st_sfc()
  fin=st_sf(st_drop_geometry(a),geometry=c)
  if (i==1){
    gridn_arr=fin
  } else {
    gridn_arr=rbind(gridn_arr,fin)
  }
}
plot(st_geometry(gridn_arr))
st_crs(gridn_arr)=st_crs(gride)
grid_arr=rbind(gride,gridn_arr)
plot(st_geometry(grid_arr))






plot(st_geometry(gride))
st_gr
plot(grid[1:16,0])
grid=st_sf(n=1:length(grid),grid)
grid$l=as.integer(st_length(grid))
a=grid[16,] %>% st_coordinates() %>% as.data.frame()
ap=a %>% arrange(X)
a2=st_linestring(as.matrix(a))
ap2=st_linestring(as.matrix(a))
plot(a2)

st_length(grid)
length(grid)
plot(fin)
plot(grid)
c=grid %>% st_cast("POLYGON") %>% st_combine()
c2=st_orthoproj(c,-90,50)
plot(c)

st_is_valid(c)
plot(grid,add=T)
plot(st_geometry(a2),add=T)
a=st_transform(grid,4326)
plot(a)



plot(c)
a3=st_transform(a2,"+proj=ortho")

plot(st_geometry(a3))


borderplot=st_coordinates(border) %>% st_linestring() %>% st_sfc()
st_crs(borderplot)=ortho
plot(borderplot,axes=T)
plot(st_geometry(ptr),add=T)






st_crs(pol)
summary(pol2)
st_sfc
pol2=st_sfc(pol,4326)


st_crs(pol)=4326
crop=st_intersection()



clon=0
clat=0
ortho = paste("+proj=ortho +lon_0=",clon," +lat_0=",clat,sep = "")
cntry=ne_countries(110,"countries",returnclass = "sf")
plot(st_geometry(st_transform(cntry,ortho)))


testpoint=rbind(c(10,30),c(160,-70),c(0,0),c(-20,-10))
testpoint=st_multipoint(testpoint) %>% st_sfc(crs=4326) %>% st_cast("POINT")
t2=st_transform(testpoint,ortho)
plot(t2,add=T,cex=5,col="blue")

coord=st_coordinates(testpoint) %>% as.data.frame()
coord2=st_coordinates(t2) %>% as.data.frame()
coord=cbind(coord,coord2)
names(coord)=c("alpha","beta","x","y")

#Maths
d2r     <- pi / 180
R=6378137
coord$newx=R*(cos(coord$beta*d2r) * sin(coord$alpha*d2r - clon*d2r))
coord$x/coord$newx
coord$newy=R*(cos(clat*d2r) * sin(coord$beta*d2r) - sin(clat*d2r) * cos(coord$beta*d2r) * cos(coord$alpha*d2r - clon*d2r))
coord$front=sin(clat*d2r)* sin(coord$beta*d2r)+cos(clat*d2r)* cos(coord$beta*d2r)* cos(coord$alpha*d2r - clon*d2r)
cos(asin(sqrt(coord$x^2+coord$y^2)/R))

#Border Map
x=R*sin(seq(0,2*pi,length.out = 1000))
y=R*cos(seq(0,2*pi,length.out = 1000))
length(x)
border=cbind(x,y) %>% st_multipoint() %>% st_sfc(crs=ortho) %>% st_cast("POINT") %>% st_sf(ind_ring=1:length(x)                                                                                         )
border.c=st_coordinates(border) %>% as.data.frame()
border.c$check=cos(asin(sqrt(border.c$X^2+border.c$Y^2)/R))

#Arrange map
clon=-5
clat=-1
ortho = paste("+proj=ortho +lon_0=",clon," +lat_0=",clat,sep = "")

#Border Map
x=R*sin(seq(0,2*pi,length.out = 1000))
y=R*cos(seq(0,2*pi,length.out = 1000))
border=cbind(x,y) %>% 
  st_multipoint() %>% 
  st_sfc(crs=ortho) %>% 
  st_cast("POINT") %>% 
  st_sf(ind_ring=1:length(x))                                   
border_un=st_transform(border,st_crs(sfinit)) %>% st_geometry()



cntry=ne_countries(110,"countries",returnclass = "sf")

sfinit=cntry
sfinit$ind=1:nrow(sfinit)
sfinit_dis=st_cast(sfinit,"POLYGON")
sfinit_dis$ind_dis=1:nrow(sfinit_dis)
sfinit_dis=sfinit_dis[,c("ind","ind_dis")]
sfinit_dis=st_cast(sfinit_dis,"POINT")
sfinit_dis$ipoint=1:nrow(sfinit_dis)
sfinit_dis=cbind(sfinit_dis,st_coordinates(sfinit_dis))
#Identify points on the back
d2r     <- pi / 180
R=6378137
sfinit_dis$front=sin(clat*d2r)* sin(sfinit_dis$Y*d2r)+cos(clat*d2r)* cos(sfinit_dis$Y*d2r)* cos(sfinit_dis$X*d2r - clon*d2r)
back=filter(sfinit_dis,sfinit_dis$front<0)
front=filter(sfinit_dis,sfinit_dis$front>=0)
plot(st_geometry(sfinit))
plot(st_geometry(back),add=T,col="red")
plot(st_geometry(front),add=T,col="green")
del=st_transform(border,st_crs(sfinit)) %>% st_geometry()
del

del2=del %>% st_coordinates() %>% st_linestring()
dd2=st_coordinates(del) %>% as.data.frame() %>% arrange(X,Y) %>% as.matrix()
is.matrix(dd2)
f=st_linestring(dd2) %>% st_sfc(crs=4326)
plot(f,add=T,col="pink")

a=st_intersection(sfinit,f)
plot(st_geometry(a))

  st_intersection(sfinit)
plot(f)
plot(del,add=T,col="orange")
st_bbox(del)
dis=st_cast(cntry,"POLYGON")
dis$index=1:nrow(dis)
disend=dis["index"]

sphere_g <- st_graticule(
  lon = seq(-180, 180, 2),
  lat = seq(-89, 89, 2),
  ndiscr = 1000, margin = 10e-6) %>%
  st_transform(crs = ortho) %>%
  st_convex_hull() %>% st_buffer(100) %>% st_combine() %>%st_union() %>% st_geometry()

plot(sphere_g)
cnt2=st_cast(cntry,"POLYGON")
cnt=st_transform(cnt2,ortho) %>% st_geometry() %>% st_buffer(0)
plot(cnt)
c2=st_intersection(cnt,sphere_g)
plot(c2)


c3=st_transform(c2,4326)
plot(c3)
disendt=st_transform(disend,ortho)
plot(st_geometry(disendt),add=T)

Rad=6370000
x=sin(seq(0,pi/180*365,0.05))*Rad
y=cos(seq(0,pi/180*365,0.05))*Rad
mpoint=st_multipoint(cbind(x,y)) %>% st_sfc(crs=ortho)
munproj=st_transform(mpoint,4326)
st_coordinates(mpoint)
plot(st_geometry(cntry))
plot(munproj,add=T)
plot(munproj)
mline=st_cast(munproj,"MULTILINESTRING") 
mpol=st_cast(mline,"POLYGON")
plot(st_geometry(cntry))
c=st_cast(mline,"LINESTRING")
plot(mline,add=T)
st_cast()

plot(mpol)
c=st_convex_hull(mpol)
plot(c,col="red")
c2=st_intersection(cntry,c)


plot(st_geometry(c2))
c3=st_transform(c2,ortho)



plot(st_geometry(c3))
dis=st_cast(cntry,"POLYGON")
dis2=st_transform(dis,ortho) %>% st_geometry()
plot(dis2)

plot(st_geometry(crop))
plot(st_)
sin(90*pi/180)
pi/180*360

clon=-20
clat=10
lon=10
lat=20
d2r     <- pi / 180
crs = paste("+proj=ortho +lon_0=",clon," +lat_0=",clat,sep = "")
checkpoint=st_point(c(lon,lat)) %>% st_sfc(crs=4326) %>% st_sf(Test=1) %>% st_transform(crs=crs)
cd=st_coordinates(checkpoint) %>% as.data.frame()
cd$newx=cos(lat*d2r) * sin(lon*d2r - clon*d2r)
Rad=cd$X/cd$newx
cd$newy=Rad*(cos(clat*d2r) * sin(lat*d2r) - sin(clat*d2r) * cos(lat*d2r) * cos(lon*d2r - clon*d2r))
cd$angle=atan2(cd$Y,cd$X)*180/pi
cd$angle

center=c(clon,clat) %>% st_point() %>% st_sfc(crs=4326) %>% st_sf(Test=0) %>% st_transform(crs)
dfin=rbind(center,checkpoint)
  plot(dfin[0],axes=T)
plot(checkpoint,axes=T)

xnew=Rad*sin(cd$angle)
ynew=Rad*cos(cd$angle)



atan(4/3)+180


atan(4/3)*180/pi 


atan2(4,3)*180/pi
4/3
pi/2

dot = x1*x2 + y1*y2      # dot product
det = x1*y2 - y1*x2      # determinant
angle = atan2(det, dot)  # atan2(y, x) or atan2(sin, cos)

cd$newx=cos(lat*d2r) * sin(lon*d2r - clon*d2r)


cntry=ne_countries(110,"countries",returnclass = "sf")




plot(st_geometry(st_transform(cntry,crs)))
dd=cntry %>% st_cast("POLYGON") %>% st_transform(crs) %>% st_buffer(0)
dd2=st_intersection(dd,sphere_g)
plot(st_geometry(dd))
plot(sphere_g,add=T)
plot(st_geometry(dd2))

crop=st_intersection(cnntry_a,sphere_g)
plot(st_geometry(crop))
sphere_g <- st_graticule(
  lon = seq(-180, 180, 2),
  lat = seq(-89, 89, 2),
  ndiscr = 1000, margin = 10e-6) %>%
  st_transform(crs = crs) %>%
  st_convex_hull() %>% st_buffer(0) %>% st_combine() %>%st_union()

a=st_coordinates(sphere_g) %>% as.data.frame()
b=st_transform(sphere_g,4326) %>% st_buffer(0)
plot(st_geometry(cntry))
plot(b,add=T)
c=st_intersection(cntry,b)
plot(st_geometry(c))
c2=st_transform(c,crs)
plot(st_geometry(c2))
st_bbox(sphere_g)
plot(sphere_g)
sphere_unproj=sphere_g %>% st_transform(4326) %>% st_buffer(1) %>% st_combine() %>% st_union()
plot(sphere_unproj)
st_bbox(sphere_unproj)
rad=6378137
x=rad*sin(seq(0,361,0.5))
y=rad*cos(seq(0,361,0.5))
f=cbind(x,y)
c=st_sf(a=1,st_sfc(st_multipoint(f)))
c2=st_cast(c,"LINESTRING") 
plot(c2,axes=T)
%>% st_buffer(0) %>% st_union() 
st_crs(c2)=crs
st_crs(c2)
plot(c2    ,axes=T)1
c2=st_transform(c2,4326)
plot(c2)
st_bbox(c)
f=st_multilinestring(list(f))
st_as_sf(st_multipoint(f))
f2=st_sf(1,f)
f3=st_polygon(f)
st_bbox(f)
plot(f)
f2=st_cast(f,"LINESTRING")
s=st_multipolygon(f2)
plot(f2)
sphere_ort <- st_graticule(
  lon = seq(-180, 180, 2),
  lat = seq(-89, 89, 2),
  ndiscr = 1000, margin = 10e-6) %>%
  st_transform(crs = crs) %>%
  st_convex_hull() 
cntry=ne_countries(110,"countries",returnclass = "sf")
cntry_dis=st_cast(cntry,"POLYGON")

a=sphere_ort %>% st_buffer(100) %>% st_union() %>% st_geometry()

map2=st_transform(cntry_dis,crs=crs)
plot(st_geometry(map2))
b=st_make_valid(map2)
plot(st_geometry(b))
c=st_intersection(b,a)
plot(st_geometry(c))
plot(a,add=T,lwd=3)
st_sn
dd=st_snap(c,a,tol=30000)
plot(st_geometry(dd))
bb=st_nearest_points(c,a)
plot(bb)
cntry2=st_cast(cntry,"POLYGON")
a=st_coordinates(cntry2[1,])
a
plot(a)
b=st_polygon(a)
cntry3=st_cast(cntry2,"MULTILINESTRING",group_or_split=T)
st_geometry(cntry2)
coord=st_coordinates(st_geometry(cntry2)) %>% as.data.frame()




st_bbox(cntry)
point=st_sfc(st_point(c(-60,-40)))
st_crs(point)=4326
plot(st_geometry(cntry))
plot(point,col="red",cex=7,add=T)

p2=st_transform(point,crs="+proj=ortho +lon_0=0 +lat_0=0")

Rad=6378137
d2r     <- pi / 180
cenlong=0*d2r
cenlat=0*d2r
long=-60*d2r
lat=-40*d2r

x     <- Rad*(cos(lat) * sin(long - cenlong))
y     <- Rad*(cos(cenlat) * sin(lat) - sin(cenlat) * cos(lat) * cos(long - cenlong))
front <- sin(cenlat) * sin(lat) + cos(cenlat) * cos(lat) * cos(long-cenlong) > 0

c(x,y)
st_coordinates(p2)
#back
rho=sqrt(x^2+y^2)
c=asin(rho/Rad)

latreg=asin(cos(c)*sin(cenlat)+y*sin(c)*cos(cenlat)/rho)/d2r
longreg=(cenlong+atan(x*sin(c)/(rho*cos(c)*cos(cenlat)-y*sin(c)*sin(cenlat))))/d2r

map=ne_countries(50,"countries",returnclass = "sf")
#Orthomap----
#crs = "+proj=ortho +lon_0=-70 +lat_0=-15"
crs = "+proj=ortho +lon_0=0 +lat_0=0"
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
plot(st_geometry(map))
plot(sphere,add=T)
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
