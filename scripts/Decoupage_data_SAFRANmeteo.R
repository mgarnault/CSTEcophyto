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

years=unique(weather$Year)
months=unique(weather$Month)

for(year in years){
  if(!file.exists(paste0(path,"output/mailles/",year))){
    dir.create(paste0(path,"output/mailles/",year))
  }else{
    unlink(paste0(path,"output/mailles/",year),recursive=TRUE)
    dir.create(paste0(path,"output/mailles/",year))
  }
  
  df=weather[which(weather$Year==year),]
  write.table(df,
              paste0(path,"output/mailles/",year,"/Total_",year,".csv"),
              sep=";",row.names=FALSE)
  df=df[,-which(colnames(df)%in%c("Month","DOM","DOY"))]
  write.table(aggregate(.~Site,df,"mean"),
              paste0(path,"output/mailles/",year,"/Moyenne_",year,".csv"),
              sep=";",row.names=FALSE)
  
  for(month in months){
    dir.create(paste0(path,"output/mailles/",year,"/",month,"-",year))
    
    df=weather[which(weather$Year==year & weather$Month==month),]
    write.table(df,
                paste0(path,"output/mailles/",year,"/",month,"-",year,"/Total_",month,"-",year,".csv"),
                sep=";",row.names=FALSE)
    df=df[,-which(colnames(df)%in%c("DOM","DOY"))]
    write.table(aggregate(.~Site,df,"mean"),
                paste0(path,"output/mailles/",year,"/",month,"-",year,"/Moyenne_",month,"-",year,".csv"),
                sep=";",row.names=FALSE)
    
    days=unique(weather$DOM[which(weather$Year==year & weather$Month==month)])
    for(day in days){
      df=weather[which(weather$Year==year & weather$Month==month & weather$DOM==day),]
      write.table(df,
                  paste0(path,"output/mailles/",year,"/",month,"-",year,"/",day,"-",month,"-",year,".csv"),
                  sep=";",row.names=FALSE)
    }
  }
}