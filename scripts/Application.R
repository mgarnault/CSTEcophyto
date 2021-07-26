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

# install.packages("gridExtra")
library("gridExtra")

# install.packages("shinydashboard")
library("shinydashboard")

# install.packages("shiny")
library("shiny")

# install.packages("shinyjs")
library("shinyjs")

# install.packages("shinyWidgets")
library("shinyWidgets")

# install.packages("shinybusy")
library("shinybusy")

# install.packages("DT")
library("DT")

# Working directory
path=dirname(rstudioapi::getSourceEditorContext()$path)
path=substr(path,1,(as.numeric(gregexpr("/scripts",dirname(rstudioapi::getSourceEditorContext()$path)))-1))
setwd(path)


# Functions
makeLegend=function(palette,breaks,title){
  colors=as.character(palette)
  values=as.numeric(names(palette))
  
  p=ggplot()+
    scale_x_continuous(limits=c(min(values)-(length(c(min(values):max(values)))/20),
                                max(values)+(length(c(min(values):max(values)))/20)),
                       expand=c(0,0))+
    scale_y_continuous(limits=c(0,1),expand=c(0,0))+
    geom_point(aes(x=values,y=0.5),shape=15,size=10,col=colors)+
    geom_segment(aes(x=breaks,xend=breaks,y=0.43,yend=0.27),size=.8)+
    geom_text(aes(x=breaks,y=0.25,label=names(breaks)),hjust=0.5,vjust=1,size=5)+
    geom_text(aes(x=mean(breaks),y=0.75,label=title),
              hjust=0.5,vjust=0.5,size=6)+
    theme_void()
  
  return(p)
}


# Global variables
# weather=read.table(paste0(path,"/data/meteo_safran/raw/guillaumeHarel/siclima_extraction_401_20210413.csv"),sep=";",header=TRUE,stringsAsFactors=FALSE)

# quantile(weather$t_q,c(.05,.95))
colFunction=colorRampPalette(c("#ad4bff","#4e4bff","#4bfffc","#4bff59","#fcff3b","#ff3b3b","#b22727"))
paletteTemperature=colFunction(length(seq(-5,30,.1)))
names(paletteTemperature)=round(seq(-5,30,.1),1)
# hist(weather$t_q,nclass=100,xlim=c(-5,30))
# points(x=as.numeric(names(paletteTemperature)),
#        y=rep(300000,length(paletteTemperature)),
#        col=paletteTemperature,pch=15,cex=2)
breaksTemperature=c(-5,0,5,10,15,20,25,30)
names(breaksTemperature)=c("<-5","0","5","10","15","20","25",">30")

# quantile(weather$preliq_q,c(.05,.95))
colFunction=colorRampPalette(c("#ffffb1","#c9ff6a","#51ff43","#5cf8f6","#5bbfff","#5b6aff","#0017ff"))
paletteRain=colFunction(length(seq(0,10,.1)))
names(paletteRain)=round(seq(0,10,.1),1)
# hist(weather$preliq_q,nclass=1000,xlim=c(0,10))
# points(x=as.numeric(names(paletteRain)),
#        y=rep(5000000,length(paletteRain)),
#        col=paletteRain,pch=15,cex=2)
breaksRain=c(0,1,2,3,4,5,6,7,8,9,10)
names(breaksRain)=c("0","1","2","3","4","5","6","7","8","9",">10")

# quantile(weather$hu_q,c(.05,.95))
colFunction=colorRampPalette(c("#ff4343","#e7e7e7","#4a47ff"))
paletteMoisture=colFunction(length(seq(50,100,.1)))
names(paletteMoisture)=round(seq(50,100,.1),1)
# hist(weather$hu_q,nclass=100,xlim=c(50,100))
# points(x=as.numeric(names(paletteMoisture)),
#        y=rep(300000,length(paletteMoisture)),
#        col=paletteMoisture,pch=15,cex=2)
breaksMoisture=c(50,60,70,80,90,100)
names(breaksMoisture)=c("<50","60","70","80","90","100")

# quantile(weather$ff_q,c(.05,.95))
colFunction=colorRampPalette(c("#00ded7","#00de1b","#fffc00","#de0000","#895e00","#000000"))
paletteWind=colFunction(length(seq(0,8,.1)))
names(paletteWind)=round(seq(0,8,.1),1)
# hist(weather$ff_q,nclass=100,xlim=c(0,8))
# points(x=as.numeric(names(paletteWind)),
#        y=rep(600000,length(paletteWind)),
#        col=paletteWind,pch=15,cex=2)
breaksWind=c(0,1,2,3,4,5,6,7,8)
names(breaksWind)=c("0","1","2","3","4","5","6","7",">8")

# quantile(weather$dli_q,c(.05,.95))
colFunction=colorRampPalette(c("#00fff7","#00ff08","#f7ff00","#ff9300","#ff0000","#680000"))
paletteRadiation=colFunction(length(seq(2000,3500,1)))
names(paletteRadiation)=round(seq(2000,3500,1))
# hist(weather$dli_q,nclass=100,xlim=c(2000,3500))
# points(x=as.numeric(names(paletteRadiation)),
#        y=rep(300000,length(paletteRadiation)),
#        col=paletteRadiation,pch=15,cex=2)
breaksRadiation=c(2000,2250,2500,2750,3000,3250,3500)
names(breaksRadiation)=c("<2000","2250","2500","2750","3000","3250",">3500")

# quantile(weather$tsup_h_q,c(.05,.95))
colFunction=colorRampPalette(c("#fffb00","#ff0000","#760000"))
paletteTsup=colFunction(length(seq(5,40,.1)))
names(paletteTsup)=round(seq(5,40,.1),1)
# hist(weather$tsup_h_q,nclass=100,xlim=c(5,40))
# points(x=as.numeric(names(paletteTsup)),
#        y=rep(300000,length(paletteTsup)),
#        col=paletteTsup,pch=15,cex=2)
breaksTsup=c(5,10,15,20,25,30,35,40)
names(breaksTsup)=c("<5","10","15","20","25","30","35",">40")

# quantile(weather$tinf_h_q,c(.05,.95))
colFunction=colorRampPalette(c("#0017ff","#00f7ff","#00ff04"))
paletteTinf=colFunction(length(seq(-15,20,.1)))
names(paletteTinf)=round(seq(-15,20,.1),1)
# hist(weather$tinf_h_q,nclass=100,xlim=c(-15,20))
# points(x=as.numeric(names(paletteTinf)),
#        y=rep(300000,length(paletteTinf)),
#        col=paletteTinf,pch=15,cex=2)
breaksTinf=c(-15,-10,-5,0,5,10,15,20)
names(breaksTinf)=c("<-15","-10","-5","0","5","10","15",">20")

SAFRANvarList=list(
  # "Concentration de CO2 [ppm]"="co2",
  # "Contenu en eau gelée dans la couche de racinaire à 06 UTC [m³/m³]"="wgi_racine_q",
  # "Contenu en eau liquide dans la couche racinaire à 06 UTC [m³/m³]"="wg_racine_q",
  # "Drainage (cumul quotidien 06-06 UTC) [mm]"="drainc_q",
  # "Ecoulement à la base du manteau neigeux [mm]"="ecoulement_q",
  # "Epaisseur du manteau neigeux (moyenne quotidienne 06-06 UTC) [m]"="hteurneige_q",
  # "Epaisseur du manteau neigeux maximum au cours de la journée [m]"="hteurneigex_q",
  # "Epaisseur du manteau neigeux à 06 UTC [m]"="hteurneige6_q",
  # "Equivalent en eau du manteau neigeux (moyenne quotidienne 06-06 UTC) [mm]"="resr_neige_q",
  # "Equivalent en eau du manteau neigeux à 06 UTC [mm]"="resr_neige6_q",
  # "Evapotranspiration potentielle (formule de Penman-Monteith) [mm]"="etp_q",
  # "Evapotranspiration potentielle calculée par SICLIMA (formule de Penman-Monteith) [mm]"="etppm",
  # "Evapotranspiration réelle (cumul quotidien 06-06 UTC) [mm]"="evap_q",
  # "Fraction recouverte par la neige (moyenne quotidienne 06-06 UTC) [%]"="snow_frac_q",
  "Humidité relative (moyenne quotidienne) [%]"=list(name="hu_q",palette=paletteMoisture,breaks=breaksMoisture),
  # "Humidité spécifique (moyenne quotidienne) [g/kg]"="q_q",
  # "Indice d’humidité des sols (moyenne quotidienne 06-06 UTC) [%]"="swi_q",
  # "Pluies efficaces (cumul quotidien) [mm]"="pe_q",
  "Précipitations liquides (cumul quotidien 06-06 UTC) [mm]"=list(name="preliq_q",palette=paletteRain,breaks=breaksRain),
  # "Précipitations solides (cumul quotidien 06-06 UTC) [mm]"="prenei_q",
  "Rayonnement atmosphérique (cumul quotidien) [J/cm²]"=list(name="dli_q",palette=paletteRadiation,breaks=breaksRadiation),
  # "Rayonnement visible (cumul quotidien) [J/cm²]"="ssi_q",
  # "Ruissellement (cumul quotidien 06-06 UTC) [mm]"="runc_q",
  "Température (moyenne quotidienne) [°C]"=list(name="t_q",palette=paletteTemperature,breaks=breaksTemperature),
  "Température maximale des 24 températures horaires [°C]"=list(name="tsup_h_q",palette=paletteTsup,breaks=breaksTsup),
  "Température minimale des 24 températures horaires [°C]"=list(name="tinf_h_q",palette=paletteTinf,breaks=breaksTinf),
  "Vent à 10m (moyenne quotidienne) [m/s]"=list(name="ff_q",palette=paletteWind,breaks=breaksWind))

polygonsCom=st_read(paste0(path,"/data/shapes_administration/output/communes_metropole_lite"),options="ENCODING=UTF-8",quiet=TRUE)
polygonsDep=st_read(paste0(path,"/data/shapes_administration/output/departements_metropole_lite"),options="ENCODING=UTF-8",quiet=TRUE)
polygonsReg=st_read(paste0(path,"/data/shapes_administration/output/regions_metropole_lite"),options="ENCODING=UTF-8",quiet=TRUE)
polygonsFra=st_read(paste0(path,"/data/shapes_administration/output/france_metropole_lite"),options="ENCODING=UTF-8",quiet=TRUE)
polygonsSaf=st_read(paste0(path,"/data/mailles_safran/output/SAFRANgrid_handmade"),options="ENCODING=UTF-8",quiet=TRUE)
polygonsSaf=polygonsSaf[-which(is.na(polygonsSaf$dprtmnt)),]







# USER INTERFACE #
ui=dashboardPage(
  dashboardHeader(title="CST Ecophyto II+ Tool"),
  dashboardSidebar(width=350,
                   sidebarMenu(id="sidebarmenu",
                               menuItem("Météo SAFRAN",tabName="SAFRAN",icon=icon("temperature-high")),
                               conditionalPanel("input.sidebarmenu=='SAFRAN'",
                                                fluidRow(column(12,dateRangeInput("timeRangeSAFRAN","Plage temporelle"))),
                                                fluidRow(column(12,pickerInput("variableToPlotSAFRAN","Variable climatique représentée",
                                                                               choices=names(SAFRANvarList),
                                                                               multiple=FALSE))),
                                                fluidRow(column(12,pickerInput("scaleToPlotSAFRAN","Résolution spatiale",
                                                                               choices=c("Mailles SAFRAN","Départements","Régions","France entière"),
                                                                               multiple=FALSE))),
                                                fluidRow(column(12,pickerInput("bordersToPlotSAFRAN","Découpages spatiaux à afficher",
                                                                               choices=c("Mailles SAFRAN","Communes","Départements","Régions","France entière"),
                                                                               multiple=TRUE))),
                                                fluidRow(column(12,prettySwitch(inputId="showMeshCharac",label="Afficher les contraintes appliquées aux mailles",status="primary",slim=TRUE))),
                                                conditionalPanel("input.showMeshCharac==1",
                                                                 fluidRow(column(1),
                                                                          column(11,sliderInput("altitudeMeshSAFRAN","Altitude moyenne [m]",
                                                                                                0,3000,c(0,1000),ticks=FALSE))),
                                                                 fluidRow(column(1),
                                                                          column(11,sliderInput("meadowMeshSAFRAN","Prairies [%]",
                                                                                                0,100,c(0,80),ticks=FALSE))),
                                                                 fluidRow(column(1),
                                                                          column(11,sliderInput("fieldMeshSAFRAN","Systèmes culturaux et parcellaires complexes [%]",
                                                                                                0,100,c(0,100),ticks=FALSE))),
                                                                 fluidRow(column(1),
                                                                          column(11,sliderInput("fieldNoIrrigationMeshSAFRAN","Terres arables hors périmètres d’irrigation [%]",
                                                                                                0,100,c(0,100),ticks=FALSE))),
                                                                 fluidRow(column(1),
                                                                          column(11,sliderInput("orchardMeshSAFRAN","Vergers et petits fruits [%]",
                                                                                                0,100,c(0,100),ticks=FALSE))),
                                                                 fluidRow(column(1),
                                                                          column(11,sliderInput("vineyardMeshSAFRAN","Vignoble [%]",
                                                                                                0,100,c(0,100),ticks=FALSE))),
                                                                 fluidRow(column(1),
                                                                          column(11,sliderInput("hardwoodForestMeshSAFRAN","Forêts de feuillus [%]",
                                                                                                0,100,c(0,80),ticks=FALSE))),
                                                                 fluidRow(column(1),
                                                                          column(11,sliderInput("coniferousForestMeshSAFRAN","Forêts de conifères [%]",
                                                                                                0,100,c(0,80),ticks=FALSE))),
                                                                 fluidRow(column(1),
                                                                          column(11,sliderInput("mixedForestMeshSAFRAN","Forêts mélangées [%]",
                                                                                                0,100,c(0,80),ticks=FALSE)))),
                                                fluidRow(column(12,align="center",actionBttn("go","Go",icon=icon("sync-alt"))))),
                               hr(),
                               menuItem("Vente Phytos",tabName="BNVD",icon=icon("spray-can")))),
  dashboardBody(
    tabItems(tabItem(tabName="SAFRAN",
                     fluidRow(column(8,
                                     box(title=p("Cartographie des données SAFRAN",uiOutput("buttonSaveCartoSAFRAN",inline=TRUE)),
                                         width=12,solidHeader=TRUE,status="primary",plotOutput("cartographySAFRAN",width="100%",height=600))),
                              column(4,
                                     box(title=p("Tableau des moyennes",uiOutput("buttonSaveTableSAFRAN",inline=TRUE)),
                                         width=12,solidHeader=TRUE,status="primary",dataTableOutput("tableSAFRAN"))))
    ),
    tabItem(tabName="BNVD",h2("Graphique d'utilisation des PPP"))
    )
  )
)




# SERVER #
server=function(input,output,session){
  
  weatherSAFRAN=reactiveVal(NULL)
  cartographySAFRAN=reactiveVal(NULL)
  legendSAFRAN=reactiveVal(NULL)
  tableSAFRAN=reactiveVal(NULL)
  timeRangeSAFRAN=reactiveVal(NULL)
  variableToPlotSAFRAN=reactiveVal(NULL)
  scaleToPlotSAFRAN=reactiveVal(NULL)
  
  observeEvent(input$go,{
    gc()
    
    if(input$timeRangeSAFRAN[2]<input$timeRangeSAFRAN[1]){ # test if the first date of the time range asked by the user is actually before the second one
      sendSweetAlert(session, # display an error message if not
                     title="Erreur",
                     type="error",
                     text=paste0("La première date renseignée (",input$timeRangeSAFRAN[1],") 
                                 se situe après la seconde (",input$timeRangeSAFRAN[2],"). 
                                 Veuillez renseigner une plage temporelle où la première 
                                 date précède la seconde."))
      weatherSAFRAN(NULL)
      cartographySAFRAN(NULL)
      legendSAFRAN(NULL)
      tableSAFRAN(NULL)
      timeRangeSAFRAN(NULL)
      variableToPlotSAFRAN(NULL)
      scaleToPlotSAFRAN(NULL)
      return(NULL)
    }
    
    suppressWarnings({
      dataYears=as.numeric(list.files(paste0(path,"/data/meteo_safran/output"))) # folder names found in ~/CSTEcophyto/data/meteo_safran/output 
    })
    
    if(length(which(is.na(dataYears)))>0){ # test if folder names within ~/CSTEcophyto/data/meteo_safran/output are numeric values (i.e. years)
      errorFoldersDataYear=list.files(paste0(path,"/data/meteo_safran/output"))[
        which(is.na(dataYears))] # computes folders that are not numeric values (i.e. years)
      
      sendSweetAlert(session, # display an error message if not
                     title="Erreur",
                     type="error",
                     text=paste0("Le dossier ~/CSTEcophyto/data/meteo_safran/output
                     contient un ou des dossier(s) dont le nom ne peut être interprété
                     par une année : ",paste(errorFoldersDataYear,collapse = " et ")))
      weatherSAFRAN(NULL)
      cartographySAFRAN(NULL)
      legendSAFRAN(NULL)
      tableSAFRAN(NULL)
      timeRangeSAFRAN(NULL)
      variableToPlotSAFRAN(NULL)
      scaleToPlotSAFRAN(NULL)
      return(NULL)
    }
    
    timeRangeDays=seq(input$timeRangeSAFRAN[1],input$timeRangeSAFRAN[2],by="days") # determines sequence of days from the begin to the end of the time range asked by the user
    timeRangeYears=unique(format(timeRangeDays,format="%Y")) # keeps only years from the time range asked by the user
    
    if(!all(timeRangeYears%in%dataYears)){ # test if years asked by the user actually exist in the SAFRAN database
      timeRangesData=split(dataYears,cumsum(c(1,diff(dataYears)!=1))) # computes time ranges available in the SAFRAN database
      timeRangesData=lapply(timeRangesData,function(element){
        paste0(element[1]," à ",element[length(element)])
      })
      timeRangesData=do.call("paste",c(timeRangesData,sep=" et "))
      
      sendSweetAlert(session, # display an error message if not
                     title="Erreur",
                     type="error",
                     text=paste0("Le dossier ~/CSTEcophyto/data/meteo_safran/output 
                     ne contient pas des données sur toute ou partie de la plage 
                     temporelle demandée (du ",input$timeRangeSAFRAN[1]," au ",input$timeRangeSAFRAN[2],
                                 "). Les années disponibles dans la base données SAFRAN sont : ",
                                 timeRangesData))
      weatherSAFRAN(NULL)
      cartographySAFRAN(NULL)
      legendSAFRAN(NULL)
      tableSAFRAN(NULL)
      timeRangeSAFRAN(NULL)
      variableToPlotSAFRAN(NULL)
      scaleToPlotSAFRAN(NULL)
      return(NULL)
    }
    
    show_modal_spinner(spin="orbit",text="Importation des données",color="#08298A")
    variable=SAFRANvarList[[which(names(SAFRANvarList)==input$variableToPlotSAFRAN)]]$name
    
    importation=TRUE
    if(!is.null(variableToPlotSAFRAN()) & !is.null(timeRangeSAFRAN())){
      if(input$variableToPlotSAFRAN==variableToPlotSAFRAN() &
         all(input$timeRangeSAFRAN==timeRangeSAFRAN())){
        importation=FALSE
      }
    }
    
    if(importation){
      if(length(timeRangeYears)==1){ # the time range asked by the user span over only one year
        weather=read.table(paste0(path,"/data/meteo_safran/output/",timeRangeYears,"/",variable,"/Total.csv"),
                           sep=";",header=TRUE,stringsAsFactors=FALSE)
      }else if(length(timeRangeYears)==2){ # time range asked by the user span over only two years
        weather1=read.table(paste0(path,"/data/meteo_safran/output/",timeRangeYears[1],"/",variable,"/Total.csv"),
                            sep=";",header=TRUE,stringsAsFactors=FALSE)
        weather2=read.table(paste0(path,"/data/meteo_safran/output/",timeRangeYears[2],"/",variable,"/Total.csv"),
                            sep=";",header=TRUE,stringsAsFactors=FALSE)
        weather=rbind(weather1,weather2)
        rm(list=c("weather1","weather2"))
        gc()
      }else{ # time range asked by the user span over more than two years
        sendSweetAlert(session,
                       title="Erreur",
                       type="Error",
                       text="L'application ne prévoit pas de moyenner des données 
                       climatiques sur une plage temporelle suprérieure à deux années.")
        weatherSAFRAN(NULL)
        cartographySAFRAN(NULL)
        legendSAFRAN(NULL)
        tableSAFRAN(NULL)
        timeRangeSAFRAN(NULL)
        variableToPlotSAFRAN(NULL)
        scaleToPlotSAFRAN(NULL)
      }
      weather$Date=as.Date(weather$Date)
      weather=weather[which(weather$Date%in%timeRangeDays),]
      weather=weather[,which(colnames(weather)%in%c("Site",variable))]
      weather=aggregate(.~Site,weather,"mean")
    }else{
      weather=weatherSAFRAN()
    }
    
    remove_modal_spinner()
    show_modal_spinner(spin="orbit",text="Agrégation spatiale des données",color="#08298A")
    
    grid=polygonsSaf
    grid$variable=apply(grid,1,function(row){
      if(as.numeric(row["altitud"])>=input$altitudeMeshSAFRAN[1] &
         as.numeric(row["altitud"])<=input$altitudeMeshSAFRAN[2] &
         as.numeric(row["clc_231"])>=input$meadowMeshSAFRAN[1] &
         as.numeric(row["clc_231"])<=input$meadowMeshSAFRAN[2] &
         as.numeric(row["clc_242"])>=input$fieldMeshSAFRAN[1] &
         as.numeric(row["clc_242"])<=input$fieldMeshSAFRAN[2] &
         as.numeric(row["clc_211"])>=input$fieldNoIrrigationMeshSAFRAN[1] &
         as.numeric(row["clc_211"])<=input$fieldNoIrrigationMeshSAFRAN[2] &
         as.numeric(row["clc_222"])>=input$orchardMeshSAFRAN[1] &
         as.numeric(row["clc_222"])<=input$orchardMeshSAFRAN[2] &
         as.numeric(row["clc_221"])>=input$vineyardMeshSAFRAN[1] &
         as.numeric(row["clc_221"])<=input$vineyardMeshSAFRAN[2] &
         as.numeric(row["clc_311"])>=input$hardwoodForestMeshSAFRAN[1] &
         as.numeric(row["clc_311"])<=input$hardwoodForestMeshSAFRAN[2] &
         as.numeric(row["clc_312"])>=input$coniferousForestMeshSAFRAN[1] &
         as.numeric(row["clc_312"])<=input$coniferousForestMeshSAFRAN[2] &
         as.numeric(row["clc_313"])>=input$mixedForestMeshSAFRAN[1] &
         as.numeric(row["clc_313"])<=input$mixedForestMeshSAFRAN[2]){ # tile selection according to the characteristics asked by the user
        return(weather[which(weather$Site==as.numeric(row["mll_sfr"])),variable])
      }else{
        return(NA)
      }
    })
    grid=grid[,which(colnames(grid)%in%c("geometry","variable","mll_sfr"))]
    
    if(input$scaleToPlotSAFRAN=="Mailles SAFRAN"){
      polygons=grid
      colnames(polygons)[which(colnames(polygons)=="mll_sfr")]="nom"
      if(variable=="dli_q"){polygons$variable=round(polygons$variable)}else{polygons$variable=round(polygons$variable,1)}
    }else{
      if(input$scaleToPlotSAFRAN=="Départements"){
        polygons=polygonsDep
      }else if(input$scaleToPlotSAFRAN=="Régions"){
        polygons=polygonsReg
      }else if(input$scaleToPlotSAFRAN=="France entière"){
        polygons=polygonsFra
      }
      
      polygons=polygons[,which(colnames(polygons)%in%c("geometry","nom"))]
      
      suppressWarnings({
        intersections=st_intersection(grid,polygons)
      })
      intersections$area=as.numeric(st_area(intersections))
      
      polygons$variable=apply(polygons,1,function(row){
        intersects=intersections[which(intersections$nom==row["nom"] & !is.na(intersections$variable)),]
        result=sum(intersects$variable*intersects$area)/sum(intersects$area)
        if(variable=="dli_q"){result=round(result)}else{result=round(result,1)}
        return(result)
      })
    }
    
    remove_modal_spinner()
    show_modal_spinner(spin="orbit",text="Création du graphique",color="#08298A")
    
    palette=SAFRANvarList[[which(names(SAFRANvarList)==input$variableToPlotSAFRAN)]]$palette
    polygons$color=apply(polygons,1,function(row){
      if(is.na(row["variable"])){
        return("#898989")
      }else{
        return(palette[which(as.numeric(names(palette))==as.numeric(row["variable"]))])
      }
    })
    
    p=ggplot()+
      geom_sf(fill=polygons$color,col=NA,data=polygons)+
      theme(legend.position="none",
            panel.grid=element_blank(),
            panel.background=element_rect(fill="white"),
            axis.line=element_line(colour="black"),
            plot.title=element_text(size=16.5,hjust=0.5))+
      ggtitle(paste0(" Variable : ",input$variableToPlotSAFRAN,
                     "\n Plage temporelle : ",input$timeRangeSAFRAN[1]," au ",input$timeRangeSAFRAN[2]," (moyenne)",
                     "\n Résolution spatiale : ",input$scaleToPlotSAFRAN))
    
    for(border in input$bordersToPlotSAFRAN){
      if(border=="Mailles SAFRAN"){
        p=p+geom_sf(col="black",fill=NA,data=polygonsSaf)
      }
      if(border=="Communes"){
        p=p+geom_sf(col="black",fill=NA,data=polygonsCom)
      }
      if(border=="Départements"){
        p=p+geom_sf(col="black",fill=NA,data=polygonsDep)
      }
      if(border=="Régions"){
        p=p+geom_sf(col="black",fill=NA,data=polygonsReg)
      } 
      if(border=="France entière"){
        p=p+geom_sf(col="black",fill=NA,data=polygonsFra)
      } 
    }
    
    breaks=SAFRANvarList[[which(names(SAFRANvarList)==input$variableToPlotSAFRAN)]]$breaks
    legend=makeLegend(palette,breaks,input$variableToPlotSAFRAN)
    polygons=as.data.frame(polygons)
    polygons=polygons[,which(colnames(polygons)%in%c("nom","variable"))]
    colnames(polygons)[which(colnames(polygons)=="variable")]=variable
    
    weatherSAFRAN(weather)
    cartographySAFRAN(p)
    legendSAFRAN(legend)
    tableSAFRAN(polygons)
    timeRangeSAFRAN(input$timeRangeSAFRAN)
    variableToPlotSAFRAN(input$variableToPlotSAFRAN)
    scaleToPlotSAFRAN(input$scaleToPlotSAFRAN)
    
    remove_modal_spinner()
    return(NULL)
  })
  
  output$cartographySAFRAN=renderPlot({
    if(!is.null(cartographySAFRAN())){
      isolate({
        if(!is.null(legendSAFRAN())){
          return(grid.arrange(cartographySAFRAN(),legendSAFRAN(),ncol=1,heights=c(8,2)))
          print("renderplot")
        }else{
          return(NULL)
        }
      })
    }else{
      return(NULL)
    }
  })
  
  output$buttonSaveCartoSAFRAN=renderUI({
    if(!is.null(cartographySAFRAN()) & !is.null(legendSAFRAN())){
      return(actionBttn("saveCartoSAFRAN",label=NULL,style="simple",color="success",icon=icon("save")))
    }
  })
  
  observeEvent(input$saveCartoSAFRAN,{
    if(!is.null(cartographySAFRAN()) & !is.null(legendSAFRAN())){
      savePath=choose.dir()
      
      if(is.na(savePath)){
        return(NULL)
      }else{
        variable=substr(variableToPlotSAFRAN(),1,(as.numeric(gregexpr("\\[",variableToPlotSAFRAN()))-2))
        pdf(paste0(savePath,"/",variable,"_",
                   timeRangeSAFRAN()[1],"_",timeRangeSAFRAN()[2],"_",
                   scaleToPlotSAFRAN(),".pdf"))
        p=grid.arrange(cartographySAFRAN(),legendSAFRAN(),ncol=1,heights=c(8,2))
        print(p)
        dev.off()
      }
    }
  })
  
  output$tableSAFRAN=renderDataTable({
    if(!is.null(tableSAFRAN())){
      return(tableSAFRAN())
    }else{
      return(NULL)
    }
  },options=list(lengthMenu=list(c(10,100,1000,-1),c("10","100","1000","All")),
                 pageLength=10,dom="ftp"),rownames=FALSE)
  
  output$buttonSaveTableSAFRAN=renderUI({
    if(!is.null(tableSAFRAN())){
      return(actionBttn("saveTableSAFRAN",label=NULL,style="simple",color="success",icon=icon("save")))
    }
  })
  
  observeEvent(input$saveTableSAFRAN,{
    if(!is.null(tableSAFRAN())){
      savePath=choose.dir()
      
      if(is.na(savePath)){
        return(NULL)
      }else{
        variable=substr(variableToPlotSAFRAN(),1,(as.numeric(gregexpr("\\[",variableToPlotSAFRAN()))-2))
        write.table(tableSAFRAN(),paste0(savePath,"/",variable,"_",
                                         timeRangeSAFRAN()[1],"_",timeRangeSAFRAN()[2],"_",
                                         scaleToPlotSAFRAN(),".csv"),sep=";",row.names=FALSE)
      }
    }
  })
  
  
  # Shutdown the application when user quit the web browser page
  session$onSessionEnded(function(){
    stopApp()
  })
}





# RUN #
runApp(list(ui=ui,server=server),launch.browser=TRUE)