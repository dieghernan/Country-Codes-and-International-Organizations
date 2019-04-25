
library(maps)
library(mapdata)


## start plot & extract coordinates from orthographic map
o <- c(-5,155,0) #orientation
xy <- map("world",proj="orthographic",orientation=o)
## draw a circle around the points for coloring the ocean 
polygon(sin(seq(0,2*pi,length.out=100)),cos(seq(0,2*pi,length.out=100)),col=rgb(0.6,0.6,0.9),border=rgb(1,1,1,0.5),lwd=2)
## overlay world map
map("worldHires",proj="orthographic",orientation=o,fill=TRUE,col=rgb(0.5,0.8,0.5),resolution=0,add=TRUE)
aa=sin(seq(0,2*pi,length.out=100)

#Get contiguous country coordinates
contigcoord <- function (database = "world", regions = ".", exact = FALSE, boundary = TRUE, interior = TRUE, fill = FALSE, xlim = NULL, ylim = NULL){
  if (is.character(database))
    as.polygon = fill
  else as.polygon = TRUE
  coord <- maps:::map.poly(database, regions, exact, xlim, ylim, boundary, 
                           interior, fill, as.polygon)
  return(coord)
}
mapproj <- function(lat,long,cenlat,cenlong){
  d2r=pi/180; lat=lat*d2r; long=long*d2r; cenlat=cenlat*d2r; cenlong=cenlong*d2r
  x=cos(lat)*sin(long-cenlong)
  y=cos(cenlat)*sin(lat)-sin(cenlat)*cos(lat)*cos(long-cenlong)
  front=sin(cenlat)*sin(lat)+cos(cenlat)*cos(lat)*cos(long-cenlong) > 0
  return(list(x=x,y=y,front=front))
}
xy <- contigcoord("world",fill=TRUE)
coord <- cbind(xy$x,xy$y)
coordtr <- mapproj(coord[,2],coord[,1],cenlat,cenlong)
coord <- cbind(coord,coordtr$x,coordtr$y,coordtr$front)

naloc <- (1:nrow(coord))[!complete.cases(coord)]
naloc <- c(0,naloc)
for(i in 2:length(naloc)){
  thispoly <- coord[(naloc[i-1]+1):(naloc[i]-1),3:5,drop=F]
  thispoly <- rbind(thispoly,thispoly[1,])
  unq <- unique(thispoly[,3])
  if(length(unq) == 1){ 
    if(unq == 1){ #Polygon is fully on front side
      polygon(thispoly[,1],thispoly[,2],col=rgb(0.5,0.8,0.5),border=NA)
    }
  } else { #front and back present
    ind <- thispoly[,3] == 0
    #project points "outside" the globe
    temdist <- pmax(sqrt(rowSums(as.matrix(thispoly[ind,1:2]^2))),1e-5)
    thispoly[ind,1:2] <- thispoly[ind,1:2]*(2-temdist)/temdist
    polygon(thispoly[,1],thispoly[,2],col=rgb(0.5,0.8,0.5),border=NA)
  }
}
