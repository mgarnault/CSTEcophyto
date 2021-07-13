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
.rs.restartR() # this command must be run independently otherwise Rstudio IDE may freeze

# Clear console
cat("\014")


# Packages
# install.packages("ggplot2")
library("ggplot2")

# install.packages("rmapshaper")
# library("devtools") # need Rtools first: download from https://cran.r-project.org/bin/windows/Rtools/
# install_github("ateucher/rmapshaper") # avoid encoding error from ms_simplify(), see: https://github.com/ateucher/rmapshaper/issues/67
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
path="C:/Users/mgarnault/Documents/CSTEcophyto"


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

shape="communes" # Shapefile: https://www.data.gouv.fr/fr/datasets/decoupage-administratif-communal-francais-issu-d-openstreetmap/
# shape="departements" # Shapefile: https://www.data.gouv.fr/fr/datasets/contours-des-departements-francais-issus-d-openstreetmap/
# shape="regions" # Shapefile: https://www.data.gouv.fr/fr/datasets/contours-des-regions-francaises-sur-openstreetmap/
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
  polygons$nom="France"
}
object.size(polygons)


# Saving data
if(!file.exists(paste0(path,"/data/shapes_administration/output/",shape,name))){
  dir.create(paste0(path,"/data/shapes_administration/output/",shape,name))
}else{
  unlink(paste0(path,"/data/shapes_administration/output/",shape,name),recursive=TRUE)
  dir.create(paste0(path,"/data/shapes_administration/output/",shape,name))
}
st_write(polygons,paste0(path,"/data/shapes_administration/output/",shape,name,"/",shape,name,".shp"),layer_options="ENCODING=UTF-8")
# polygons=st_read(paste0(path,"/data/shapes_administration/output/",shape,name),options="ENCODING=UTF-8")


# Plot test polygons and SAFRAN mesh
grid=st_read(paste0(path,"/data/mailles_safran/output/SAFRANgrid_handmade"),options="ENCODING=UTF-8")

polygons$color=sample(1:100,nrow(polygons),replace=TRUE)
x11()
ggplot()+
  geom_sf(aes(fill=color),col=NA,data=polygons)+
  geom_sf(fill=NA,col="black",data=grid)+
  # xlim(-5,1)+ylim(47,49)+
  theme(panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        panel.background=element_blank())