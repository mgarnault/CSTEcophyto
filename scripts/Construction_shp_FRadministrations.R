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

# install.packages("rmapshaper")
library("rmapshaper") # ms_simplify(..., sys=TRUE) needs the mapshaper node: download from https://nodejs.org/en/ and execute "npm install -g mapshaper" in a command prompt

# install.packages("sf")
library("sf")

# install.packages("dplyr")
library("dplyr")

# install.packages("raster")
library("raster")

# install.packages("parallel")
library("parallel")


# Working directory
path=dirname(rstudioapi::getSourceEditorContext()$path)
path=substr(path,1,(as.numeric(gregexpr("/scripts",dirname(rstudioapi::getSourceEditorContext()$path)))-1))
setwd(path)


# Global parameters
lite=TRUE # should polygon be simplified? [TRUE,FALSE]
retained=0.1 # which fraction (%) of the polygon points should be retained? [0,1]

metropolis=TRUE # should polygons of DROM be removed? [TRUE,FALSE]
metropolisLimits=st_sfc(st_polygon(list(cbind(c(-7,10,10,-7,-7),
                                             c(40,40,55,55,40))))) 
metropolisLimits=st_sf(loc="Métropole",metropolisLimits,crs=4326) # crs=4326 corresponds to the international WGS84 coordinates system

name="" # store the chosen characteristics for future data saving  
if(metropolis){name=paste0(name,"_metropole")}else{name=paste0(name,"_metropole&DROM")}
if(lite){name=paste0(name,"_lite")}else{name=paste0(name,"_complet")}

# shape="communes" # Shapefile: https://www.data.gouv.fr/fr/datasets/decoupage-administratif-communal-francais-issu-d-openstreetmap/
# shape="departements" # Shapefile: https://www.data.gouv.fr/fr/datasets/contours-des-departements-francais-issus-d-openstreetmap/
shape="regions" # Shapefile: https://www.data.gouv.fr/fr/datasets/contours-des-regions-francaises-sur-openstreetmap/
# shape="france" # based on st_union(regions)





# COMMUNAL POLYGONS #
if(shape=="france"){
  polygons=st_read(paste0(path,"/data/shapes_administration/raw/regions"),options="ENCODING=UTF-8")
}else{
  polygons=st_read(paste0(path,"/data/shapes_administration/raw/",shape),options="ENCODING=UTF-8")
}
object.size(polygons)


# Filtering metropolitan polygons if metropolis==TRUE
if(metropolis){polygons=filter(polygons,st_intersects(polygons,metropolisLimits,sparse=FALSE))}
object.size(polygons)
names=polygons$nom
wikipedia=polygons$wikipedia


# Simplifying polygons if lite==TRUE
if(lite){polygons=ms_simplify(polygons,keep=retained,keep_shapes=TRUE,sys=TRUE)}
polygons$nom=names
polygons$wikipedia=wikipedia
if(shape=="france"){
  polygons=st_union(polygons)
  polygons=st_as_sf(polygons)
  polygons=cbind(nom="France",polygons)
  colnames(polygons)[2]="geometry"
  st_geometry(polygons)="geometry"
}
object.size(polygons)


# Saving data
if(!file.exists(paste0(path,"/data/shapes_administration/output/",shape,name))){
  dir.create(paste0(path,"/data/shapes_administration/output/",shape,name))
}else{
  unlink(paste0(path,"/data/shapes_administration/output/",shape,name),recursive=TRUE)
  dir.create(paste0(path,"/data/shapes_administration/output/",shape,name))
}

dir.create(paste0(path,"/data/shapes_administration/output/",shape,name,"/shp"))
st_write(polygons,paste0(path,"/data/shapes_administration/output/",shape,name,"/shp/",shape,name,".shp"),layer_options="ENCODING=UTF-8")

colnames(polygons)=colnames(st_read(paste0(path,"/data/shapes_administration/output/",shape,name,"/shp"),options="ENCODING=UTF-8",quiet=TRUE))
dir.create(paste0(path,"/data/shapes_administration/output/",shape,name,"/rds"))
saveRDS(polygons,paste0(path,"/data/shapes_administration/output/",shape,name,"/rds/",shape,name,".rds"))


# Plot test polygons and SAFRAN mesh
polygons=readRDS(paste0(path,"/data/shapes_administration/output/",shape,name,"/rds/",shape,name,".rds"))
grid=readRDS(paste0(path,"/data/mailles_safran/output/SAFRANgrid_handmade/rds/SAFRANgrid_handmade.rds"))

polygons$color=sample(1:1000,nrow(polygons),replace=TRUE)
x11()
ggplot()+
  geom_sf(aes(fill=color),col=NA,data=polygons)+
  geom_sf(fill=NA,col="black",data=grid)+
  # xlim(-5,1)+ylim(47,49)+
  theme(panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        panel.background=element_blank())