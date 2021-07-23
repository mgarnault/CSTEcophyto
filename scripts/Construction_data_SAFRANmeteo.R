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
# install.packages("sf")
library("sf")

# install.packages("ggplot2")
library("ggplot2")


# Working directory
path="C:/Users/mgarnault/Documents/CSTEcophyto"








# DATA LOAD, SPLIT AND SAVE #
weather=read.table(paste0(path,"/data/meteo_safran/output/2013/Total_2013.csv"),
                   sep=";",header=TRUE,stringsAsFactors=FALSE)

grid=st_read(paste0(path,"/data/mailles_safran/output/SAFRANgrid_handmade"),options="ENCODING=UTF-8")

variable="t_q"
grid$var=apply(grid,1,function(row){
  if(as.numeric(row["altitud"])<1500){
    return(weather[which(weather$Site==as.numeric(row["mll_sfr"])),variable])
  }else{
    return(NA)
  }
})


# shape="france"
shape="regions"
# shape="departements"
polygons=st_read(paste0(path,"/data/shapes_administration/output/",shape,"_metropole_lite"),options="ENCODING=UTF-8")
polygons=polygons[,which(colnames(polygons)%in%c("geometry","nom"))]

grid=grid[,which(colnames(grid)%in%c("geometry","var","mll_sfr"))]
intersections=st_intersection(grid,polygons)
intersections$area=as.numeric(st_area(intersections))
# sum(intersections$var*intersections$area)/sum(intersections$area) # pour 1 région

intersections2=intersections#[which(intersections$nom=="Nouvelle-Aquitaine"),]
x11()
ggplot()+
  geom_sf(aes(fill=var),col="black",data=intersections2)+
  scale_fill_gradient2(low="#4444fe",mid="#f8fe44",high="#fe4444",midpoint=13)









