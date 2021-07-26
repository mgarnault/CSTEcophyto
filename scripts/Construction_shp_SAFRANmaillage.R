# HEADER #
# Clear command history
write("",file=".blank")
loadhistory(".blank")
unlink(".blank")

# Clear plots
if(!is.null(dev.list())) dev.off()

# Clear R-objects
rm(list=ls(all.names=TRUE))
gc()

# Clear R-session and R-system memory
# .rs.restartR() # this command must be run independently otherwise Rstudio IDE may freeze

# Clear console
cat("\014")


# Packages
# install.packages("ggplot2")
library("ggplot2")

# install.packages("sf")
library("sf")

# install.packages("raster")
library("raster")

# install.packages("parallel")
library("parallel")


# Working directory
path=dirname(rstudioapi::getSourceEditorContext()$path)
path=substr(path,1,(as.numeric(gregexpr("/scripts",dirname(rstudioapi::getSourceEditorContext()$path)))-1))
setwd(path)


# Grid estimation method
handmade=TRUE
if(handmade){name=paste0(name,"_handmade")}else{name=paste0(name,"_points2grid")} # store the chosen characteristics for future data saving  






# DATA HANDELING #
# Importation
df=read.table(paste0(path,"/data/mailles_safran/raw/mailles_safran_drias-20200206.csv"),
              sep=",",header=TRUE,stringsAsFactors=FALSE,encoding="UTF-8",quote="\"") # ZIP content downloaded from https://siclima.intranet.inrae.fr/siclima/help/extraction/creation/sel_mailles.html#choisir-les-mailles-avec-leurs-attributs
df2=df


# Projection of centroids from WGS84 to NTF (Paris) Lambert Zone II
coordinates(df2)=~longitude+latitude
proj4string(df2)=CRS("+init=epsg:4326")
df2=spTransform(df2,CRS("+init=epsg:27572"))


# Computation of the regular grid
if(handmade){
  e=as(extent(0,1240000,1605000,2685000),"SpatialPolygons")
  grid=as.data.frame(makegrid(e,"regular",cellsize=c(8000,8000)))
  grid$x2=grid$x2-1000
  names(grid)=c("longitude","latitude")
  coordinates(grid)=c("longitude","latitude")
  proj4string(grid)=CRS("+init=epsg:27572")
}else{
  grid=points2grid(df2,tolerance=0.00587692)
  grid=as.data.frame(SpatialGrid(grid))
  coordinates(grid)=~longitude+latitude
}


# Plot to compare location of true-centroids (df) and estimated-centroids (grid)
# zoom=list(All=list(longitude=c(0,1200000),latitude=c(1600000,2700000)),
#           Max1=list(longitude=c(1100000,1200000),latitude=c(1650000,1750000)),
#           Max2=list(longitude=c(500000,600000),latitude=c(2100000,2200000)),
#           Max3=list(longitude=c(200000,300000),latitude=c(2400000,2500000)),
#           SouthWest=list(longitude=c(0,400000),latitude=c(1600000,1950000)),
#           West=list(longitude=c(0,400000),latitude=c(1950000,2300000)),
#           NorthWest=list(longitude=c(0,400000),latitude=c(2300000,2700000)),
#           South=list(longitude=c(400000,800000),latitude=c(1600000,1950000)),
#           Mid=list(longitude=c(400000,800000),latitude=c(1950000,2300000)),
#           North=list(longitude=c(400000,800000),latitude=c(2300000,2700000)),
#           SouthEast=list(longitude=c(800000,1200000),latitude=c(1600000,1950000)),
#           East=list(longitude=c(800000,1200000),latitude=c(1950000,2300000)),
#           NorthEast=list(longitude=c(800000,1200000),latitude=c(2300000,2700000)))
# 
# for(lims in 1:length(zoom)){
#   x11()
#   p=ggplot()
#   p=p+geom_point(aes(x=coordinates(df2)[,1],y=coordinates(df2)[,2]),pch=3,size=3,col="black")
#   p=p+geom_point(aes(x=coordinates(grid)[,1],y=coordinates(grid)[,2]),pch=4,size=1,col="red")
#   p=p+ggtitle(names(zoom)[lims])
#   lims=zoom[[lims]]
#   p=p+xlim(lims$longitude[1],lims$longitude[2])+ylim(lims$latitude[1],lims$latitude[2])
#   print(p)
# }


# Computation of a raster layer from regular grid
grid=rasterFromXYZ(grid)


# Computation of polygons from raster layer
grid=rasterToPolygons(grid)


# Projection of the regular grid polygons into the targeted coordinates system (WGS84)
grid=st_as_sf(grid)
st_crs(grid)=27572
grid=st_transform(grid,crs="+init=epsg:4326")


# Sortening of grid cells that actually contain centroids
unlink(paste0(path,"/parallel/consoleOutputs/*"),recursive=TRUE)

cl=makeCluster(spec=(detectCores()-1))
# cl=makeCluster(spec=(detectCores()))

clusterExport(cl,c("path","grid","df"))

clusterEvalQ(cl,{
  library("sf")
  sink(paste0(path,"/parallel/consoleOutputs/jobPID",Sys.getpid(),".txt"))
})

df$gridLoc=parApply(cl,df,1,function(row){
  pnt=st_sfc(st_point(c(as.numeric(row["longitude"]),as.numeric(row["latitude"]))),crs=4326)
  id=row["ID"]
  print(id)
  return(as.numeric(st_intersects(pnt,grid)))
})

stopCluster(cl)

grid=grid[df$gridLoc,]


# Reallocation of centroid infos to the corresponding cell within the grid
grid=cbind(grid,df)
grid=grid[,-which(colnames(grid)%in%c("layer","gridLoc"))]


# Computation of the distance between true-centroids (df) and estimated-centroids (grid) 
# estimatedCentroids=st_centroid(grid)
# 
# df2=df
# coordinates(df2)=~longitude+latitude
# proj4string(df2)=CRS("+init=epsg:4326")
# trueCentroids=st_centroid(st_as_sf(df2))
# 
# centroidsOffset=st_distance(estimatedCentroids,trueCentroids,by_element=TRUE)
# x11()
# hist(centroidsOffset,nclass=100)






# PLOT TEST #
x11()
ggplot()+
  geom_sf(aes(fill=maille_safran),col="black",data=grid)+
  geom_point(aes(x=longitude,y=latitude),pch=21,fill="white",col="black",size=.8,data=df)+
  scale_fill_gradient2(low="#47fe44",mid="#f8fe44",high="#fe4444",midpoint=5000)
  # +
  # xlim(-5,0)+ylim(42,45)
  # xlim(-5,0)+ylim(45,48)
  # xlim(-5,0)+ylim(48,51)
  # xlim(0,5)+ylim(42,45)
  # xlim(0,5)+ylim(45,48)
  # xlim(0,5)+ylim(48,51)
  # xlim(5,10)+ylim(41,45)
  # xlim(5,10)+ylim(45,48)
  # xlim(5,10)+ylim(48,51)






# SAVE #
if(!file.exists(paste0(path,"/data/mailles_safran/output/SAFRANgrid",name))){
  dir.create(paste0(path,"/data/mailles_safran/output/SAFRANgrid",name))
}else{
  unlink(paste0(path,"/data/mailles_safran/output/SAFRANgrid",name),recursive=TRUE)
  dir.create(paste0(path,"/data/mailles_safran/output/SAFRANgrid",name))
}
st_write(grid,paste0(path,"/data/mailles_safran/output/SAFRANgrid",name,"/SAFRANgrid",name,".shp"),layer_options="ENCODING=UTF-8")