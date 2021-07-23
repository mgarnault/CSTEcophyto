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
# install.packages("parallel")
library("parallel")


# Working directory
path="C:/Users/mgarnault/Documents/CSTEcophyto"






# DATA LOAD, SPLIT AND SAVE #
# Loading data
weather=read.table(paste0(path,"/data/meteo_safran/raw/guillaumeHarel/siclima_extraction_401_20210413.csv"),
                   sep=";",header=TRUE,stringsAsFactors=FALSE)
gc()

# Saving years and climatic variable values
years=unique(weather$Year)
climVars=colnames(weather)[6:ncol(weather)]

# Creating a Date column format %Y-%m-%d
weather$Date=as.Date(paste(weather$Year,weather$Month,weather$DOM,sep="-"))

# Emptying parallelization folders
unlink(paste0(path,"/parallel/consoleOutputs/*"),recursive=TRUE)
unlink(paste0(path,"/parallel/tempData/*"),recursive=TRUE)

# Splitting the inter-annual data into mono-annual datas
sapply(years,function(year){
  d=weather[which(weather$Year==year),]
  d=d[,which(!colnames(d)%in%c("Year","Month","DOM","DOY"))]
  saveRDS(d,paste0(path,"/parallel/tempData/",year,".rds"))
})
rm(list="weather") # remove the big weather dataset from RAM
gc()

# Parallel computation
cl=makeCluster(spec=(detectCores()-1)) # custers creation
# cl=makeCluster(spec=(detectCores()))

clusterExport(cl,c("path","climVars")) # exportation of R objects in clusters

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
  
  for(climVar in climVars){
    cat(paste0("----------\n",year,"-",climVar,"\n")) # display the processed climatic variable
    
    dir.create(paste0(path,"/data/meteo_safran/output/",year,"/",climVar))
    
    df2=df[,which(colnames(df)%in%c("Site","Date",climVar))]
    write.table(df2, # save the raw dataset containing 1 year and 1 climatic variable
                paste0(path,"/data/meteo_safran/output/",year,"/",climVar,"/Total.csv"),
                sep=";",row.names=FALSE)
    cat("Total data saved\n")
    
    df2$Date=as.numeric(format(df2$Date,format="%Y"))
    df2=aggregate(.~Site,df2,"mean")
    write.table(df2, # save the raw dataset containing 1 year and 1 climatic variable
                paste0(path,"/data/meteo_safran/output/",year,"/",climVar,"/Moyenne.csv"),
                sep=";",row.names=FALSE)
    cat("Averaged data saved\n")
  }
})

stopCluster(cl)
gc()