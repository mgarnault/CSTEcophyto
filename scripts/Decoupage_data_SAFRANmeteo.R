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

# install.packages("data.table")
library("data.table")

# Working directory
path=dirname(rstudioapi::getSourceEditorContext()$path)
path=substr(path,1,(as.numeric(gregexpr("/scripts",dirname(rstudioapi::getSourceEditorContext()$path)))-1))
# path="/home/mgarnault/CSTEcophyto"
setwd(path)






# DATA LOAD, SPLIT AND SAVE #
# Loading data
weather=fread(paste0(path,"/data/meteo_safran/raw/projet_d_extraction_538_20210713/siclima_extraction_538_20210713.csv"),
                   sep=";",data.table=FALSE)
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
  gc()
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
    
    # write.table(df2, # save the raw dataset containing 1 year and 1 climatic variable (csv)
    #             paste0(path,"/data/meteo_safran/output/",year,"/",climVar,"/",year,"_",climVar,".csv"),
    #             sep=";",row.names=FALSE)
    # cat("File .csv saved\n")
    
    saveRDS(df2, # save the raw dataset containing 1 year and 1 climatic variable (rds)
            paste0(path,"/data/meteo_safran/output/",year,"/",climVar,"/",year,"_",climVar,".rds"))
    cat("File .rds saved\n")
  }
})

stopCluster(cl)
gc()

# Emptying parallelization folders
unlink(paste0(path,"/parallel/consoleOutputs/*"),recursive=TRUE)
unlink(paste0(path,"/parallel/tempData/*"),recursive=TRUE)