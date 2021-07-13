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
# install.packages("parallel")
library("parallel")


# Working directory
path="C:/Users/mgarnault/Documents/CSTEcophyto"






# DATA LOAD, SPLIT AND SAVE #
# Loading data
weather=read.table(paste0(path,"/data/meteo_safran/raw/guillaumeHarel/siclima_extraction_401_20210413.csv"),
                   sep=";",header=TRUE,stringsAsFactors=FALSE)
table(weather$Month,weather$Year)

# Saving years and months values
years=unique(weather$Year)
months=unique(weather$Month)

# Emptying parallelisaion folders
unlink(paste0(path,"/parallel/consoleOutputs/*"),recursive=TRUE)
unlink(paste0(path,"/parallel/tempData/*"),recursive=TRUE)

# Splitting the inter-annual data into mono-annual datas
sapply(years,function(year){
  saveRDS(weather[which(weather$Year==year),],
              paste0(path,"/parallel/tempData/",year,".rds"))
})
rm(list="weather") # remove the big weather dataset from the RAM
gc()

# Parallel computation
cl=makeCluster(spec=(detectCores()-1)) # custers creation
# cl=makeCluster(spec=(detectCores()))

clusterExport(cl,c("path","months")) # exportation of R objects in clusters

clusterEvalQ(cl,{
  sink(paste0(path,"/parallel/consoleOutputs/jobPID",Sys.getpid(),".txt")) # saving console outputs in .txt files
})

parSapply(cl,years,function(year){
  cat(paste0("==========\n",year,"\n")) # display the processed year

  if(!file.exists(paste0(path,"/data/meteo_safran/output/",year))){ # create a year folder
    dir.create(paste0(path,"/data/meteo_safran/output/",year))
  }else{
    unlink(paste0(path,"/data/meteo_safran/output/",year),recursive=TRUE)
    dir.create(paste0(path,"/data/meteo_safran/output/",year))
  }
  
  df=readRDS(paste0(path,"/parallel/tempData/",year,".rds"))
  write.table(df, # save the raw dataset containing 1 year
              paste0(path,"/data/meteo_safran/output/",year,"/Total_",year,".csv"),
              sep=";",row.names=FALSE)
  
  df2=df[,-which(colnames(df)%in%c("Month","DOM","DOY"))]
  write.table(aggregate(.~Site,df2,"mean"), # save the averaged dataset containing 1 year
              paste0(path,"/data/meteo_safran/output/",year,"/Moyenne_",year,".csv"),
              sep=";",row.names=FALSE)
  
  for(month in months){
    cat(paste0("----------\n",month,"-",year,"\n")) # display the processed month

    dir.create(paste0(path,"/data/meteo_safran/output/",year,"/",month,"-",year))
    
    df2=df[which(df$Year==year & df$Month==month),]
    write.table(df2, # save the raw dataset containing 1 month
                paste0(path,"/data/meteo_safran/output/",year,"/",month,"-",year,"/Total_",month,"-",year,".csv"),
                sep=";",row.names=FALSE)
    
    df2=df2[,-which(colnames(df)%in%c("DOM","DOY"))]
    write.table(aggregate(.~Site,df2,"mean"), # save the averaged dataset containing 1 month
                paste0(path,"/data/meteo_safran/output/",year,"/",month,"-",year,"/Moyenne_",month,"-",year,".csv"),
                sep=";",row.names=FALSE)
    
    days=unique(df$DOM[which(df$Year==year & df$Month==month)])
    for(day in days){
      cat(paste0("-\n",day,"-",month,"-",year,"\n")) # display the processed day

      df2=df[which(df$Year==year & df$Month==month & df$DOM==day),]
      write.table(df2, # save the raw dataset containing 1 day
                  paste0(path,"/data/meteo_safran/output/",year,"/",month,"-",year,"/",day,"-",month,"-",year,".csv"),
                  sep=";",row.names=FALSE)
      }
  }
})

stopCluster(cl)