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

# Working directory
path="C:/Users/mgarnault/Documents/CSTEcophyto/data/meteo_safran/"






# DATA LOAD, SPLIT AND SAVE #
weather=read.table(paste0(path,"raw/guillaumeHarel/siclima_extraction_401_20210413.csv"),
                   sep=";",header=TRUE,stringsAsFactors=FALSE)
table(weather$Month,weather$Year)

for(year in names(table(weather$Year))){
  if(!file.exists(paste0(path,"output/",year))){
    dir.create(paste0(path,"output/",year))
  }else{
    unlink(paste0(path,"output/",year),recursive=TRUE)
    dir.create(paste0(path,"output/",year))
  }
  
  for(month in names(table(weather$Month))){
    write.table(weather[which(weather$Year==year & 
                                weather$Month==month),],
                paste0(path,"output")
                sep=";",row.names=FALSE)
  }
}