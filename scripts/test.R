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

# install.packages("Hmisc")
library("Hmisc")

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
    geom_tile(aes(x=values,y=0.5),height=0.2,width=(values[2]-values[1]),fill=colors,col=NA)+
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
paletteTemperature=colFunction(length(seq(0,25,.1)))
names(paletteTemperature)=round(seq(0,25,.1),1)
# paletteTemperature=colFunction(length(seq(-5,30,.1)))
# names(paletteTemperature)=round(seq(-5,30,.1),1)
# hist(weather$t_q,nclass=100,xlim=c(-5,30))
# points(x=as.numeric(names(paletteTemperature)),
#        y=rep(300000,length(paletteTemperature)),
#        col=paletteTemperature,pch=15,cex=2)
breaksTemperature=c(0,5,10,15,20,25)
names(breaksTemperature)=c("<0","5","10","15","20",">25")
# breaksTemperature=c(-5,0,5,10,15,20,25,30)
# names(breaksTemperature)=c("<-5","0","5","10","15","20","25",">30")

# quantile(weather$preliq_q,c(.05,.95))
colFunction=colorRampPalette(c("#ffffb1","#c9ff6a","#51ff43","#5cf8f6","#5bbfff","#5b6aff","#0017ff"))
paletteRain=colFunction(length(seq(0,8,.1)))
names(paletteRain)=round(seq(0,8,.1),1)
# paletteRain=colFunction(length(seq(0,10,.1)))
# names(paletteRain)=round(seq(0,10,.1),1)
# hist(weather$preliq_q,nclass=1000,xlim=c(0,10))
# points(x=as.numeric(names(paletteRain)),
#        y=rep(5000000,length(paletteRain)),
#        col=paletteRain,pch=15,cex=2)
breaksRain=c(0,1,2,3,4,5,6,7,8)
names(breaksRain)=c("0","1","2","3","4","5","6","7",">8")
# breaksRain=c(0,1,2,3,4,5,6,7,8,9,10)
# names(breaksRain)=c("0","1","2","3","4","5","6","7","8","9",">10")

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
paletteTsup=colFunction(length(seq(5,30,.1)))
names(paletteTsup)=round(seq(5,30,.1),1)
# paletteTsup=colFunction(length(seq(5,40,.1)))
# names(paletteTsup)=round(seq(5,40,.1),1)
# hist(weather$tsup_h_q,nclass=100,xlim=c(5,40))
# points(x=as.numeric(names(paletteTsup)),
#        y=rep(300000,length(paletteTsup)),
#        col=paletteTsup,pch=15,cex=2)
breaksTsup=c(5,10,15,20,25,30)
names(breaksTsup)=c("<5","10","15","20","25",">30")
# breaksTsup=c(5,10,15,20,25,30,35,40)
# names(breaksTsup)=c("<5","10","15","20","25","30","35",">40")

# quantile(weather$tinf_h_q,c(.05,.95))
colFunction=colorRampPalette(c("#0017ff","#00f7ff","#00ff04"))
paletteTinf=colFunction(length(seq(-5,20,.1)))
names(paletteTinf)=round(seq(-5,20,.1),1)
# paletteTinf=colFunction(length(seq(-15,20,.1)))
# names(paletteTinf)=round(seq(-15,20,.1),1)
# hist(weather$tinf_h_q,nclass=100,xlim=c(-15,20))
# points(x=as.numeric(names(paletteTinf)),
#        y=rep(300000,length(paletteTinf)),
#        col=paletteTinf,pch=15,cex=2)
breaksTinf=c(-5,0,5,10,15,20)
names(breaksTinf)=c("<-5","0","5","10","15",">20")
# breaksTinf=c(-15,-10,-5,0,5,10,15,20)
# names(breaksTinf)=c("<-15","-10","-5","0","5","10","15",">20")

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

dataPolygonsSaf=as.data.frame(polygonsSaf[,which(colnames(polygonsSaf)%in%
                                       c("clc_211","clc_221","clc_222","clc_231","clc_241","clc_242","clc_311","clc_312","clc_313"))])
dataPolygonsSaf=dataPolygonsSaf[,-ncol(dataPolygonsSaf)]
hist(rowSums(dataPolygonsSaf))

polygonsSaf$Forêts=rowSums(dataPolygonsSaf[,which(colnames(dataPolygonsSaf)%in%c("clc_311","clc_312","clc_313"))])
polygonsSaf$Champs=rowSums(dataPolygonsSaf[,which(colnames(dataPolygonsSaf)%in%c("clc_241","clc_242","clc_211"))])
polygonsSaf$Prairies=dataPolygonsSaf[,which(colnames(dataPolygonsSaf)%in%c("clc_231"))]
polygonsSaf$Vergers=dataPolygonsSaf[,which(colnames(dataPolygonsSaf)%in%c("clc_222"))]
polygonsSaf$Vignes=dataPolygonsSaf[,which(colnames(dataPolygonsSaf)%in%c("clc_221"))]







# USER INTERFACE #
ui=dashboardPage(
  dashboardHeader(title="CST Ecophyto II+ Tool"),
  dashboardSidebar(
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
                                                fluidRow(column(12,pickerInput("bordersToPlotSAFRAN","Frontières spatiales à afficher",
                                                                               choices=c("Mailles SAFRAN","Communes","Départements","Régions","France entière"),
                                                                               multiple=TRUE))),
                                                fluidRow(column(12,awesomeCheckbox(inputId="showMeshCharac",label="Affichage des contraintes appliquées aux mailles SAFRAN",status="primary"))),
                                                conditionalPanel("input.showMeshCharac==1",
                                                                 fluidRow(column(1),
                                                                          column(11,sliderInput("altitudeMeshSAFRAN","Altitude moyenne [m]",
                                                                                                0,3000,c(0,1000),ticks=FALSE))),
                                                                 # fluidRow(column(1),
                                                                 #          column(11,sliderInput("meadowMeshSAFRAN","Prairies [%]",
                                                                 #                                0,100,c(0,80),ticks=FALSE))),
                                                                 # fluidRow(column(1),
                                                                 #          column(11,sliderInput("fieldMeshSAFRAN","Systèmes culturaux et parcellaires complexes [%]",
                                                                 #                                0,100,c(0,100),ticks=FALSE))),
                                                                 # fluidRow(column(1),
                                                                 #          column(11,sliderInput("fieldNoIrrigationMeshSAFRAN","Terres arables hors périmètres d’irrigation [%]",
                                                                 #                                0,100,c(0,100),ticks=FALSE))),
                                                                 # fluidRow(column(1),
                                                                 #          column(11,sliderInput("orchardMeshSAFRAN","Vergers et petits fruits [%]",
                                                                 #                                0,100,c(0,100),ticks=FALSE))),
                                                                 # fluidRow(column(1),
                                                                 #          column(11,sliderInput("vineyardMeshSAFRAN","Vignoble [%]",
                                                                 #                                0,100,c(0,100),ticks=FALSE))),
                                                                 # fluidRow(column(1),
                                                                 #          column(11,sliderInput("hardwoodForestMeshSAFRAN","Forêts de feuillus [%]",
                                                                 #                                0,100,c(0,80),ticks=FALSE))),
                                                                 # fluidRow(column(1),
                                                                 #          column(11,sliderInput("coniferousForestMeshSAFRAN","Forêts de conifères [%]",
                                                                 #                                0,100,c(0,80),ticks=FALSE))),
                                                                 # fluidRow(column(1),
                                                                 #          column(11,sliderInput("mixedForestMeshSAFRAN","Forêts mélangées [%]",
                                                                 #                                0,100,c(0,80),ticks=FALSE)))
                                                                 fluidRow(column(1),
                                                                          column(11,sliderInput("forestMeshSAFRAN","Forêts [%]",
                                                                                                0,100,c(0,75),ticks=FALSE))),
                                                                 fluidRow(column(1),
                                                                          column(11,sliderInput("meadowMeshSAFRAN","Prairies [%]",
                                                                                                0,100,c(0,75),ticks=FALSE))),
                                                                 fluidRow(column(1),
                                                                          column(11,sliderInput("fieldMeshSAFRAN","Cultures annuelles [%]",
                                                                                                0,100,c(0,100),ticks=FALSE))),
                                                                 fluidRow(column(1),
                                                                          column(11,sliderInput("orchardMeshSAFRAN","Vergers [%]",
                                                                                                0,100,c(0,100),ticks=FALSE))),
                                                                 fluidRow(column(1),
                                                                          column(11,sliderInput("vineyardMeshSAFRAN","Vignobles [%]",
                                                                                                0,100,c(0,100),ticks=FALSE)))
                                                                 ),
                                                fluidRow(column(12,align="center",actionBttn("go","Go",icon=icon("sync-alt"))))),
                               hr(),
                               menuItem("Vente Phytos",tabName="BNVD",icon=icon("spray-can")))),
  dashboardBody(
    tabItems(tabItem(tabName="SAFRAN",
                     fluidRow(column(8,
                                     box(title=p("Cartographie",uiOutput("buttonSaveCartoSAFRAN",inline=TRUE)),
                                         width=12,solidHeader=TRUE,status="primary",plotOutput("cartographySAFRAN",width="100%",height=600))),
                              column(4,
                                     box(title=p("Tableau de données",uiOutput("buttonSaveTableSAFRAN",inline=TRUE)),
                                         width=12,solidHeader=TRUE,status="primary",div(dataTableOutput("tableSAFRAN",width="100%",height=600),style="font-size:80%"))))
    ),
    tabItem(tabName="BNVD",h2("Graphique d'utilisation des PPP"))
    )
  )
)




# SERVER #
server=function(input,output,session){
  
  timeRangeSAFRAN=reactiveVal(NULL)
  variableToPlotSAFRAN=reactiveVal(NULL)
  scaleToPlotSAFRAN=reactiveVal(NULL)
  
  weatherSAFRAN=reactiveVal(NULL)
  tableSAFRAN=reactiveVal(NULL)
  
  cartographySAFRAN=reactiveVal(NULL)
  legendSAFRAN=reactiveVal(NULL)
  
  paletteSAFRAN=reactiveVal(NULL)
  polygonsSAFRAN=reactiveVal(NULL)
  
  altitudeMeshSAFRAN=reactiveVal(NULL)
  # meadowMeshSAFRAN=reactiveVal(NULL)
  # fieldMeshSAFRAN=reactiveVal(NULL)
  # fieldNoIrrigationMeshSAFRAN=reactiveVal(NULL)
  # orchardMeshSAFRAN=reactiveVal(NULL)
  # vineyardMeshSAFRAN=reactiveVal(NULL)
  # hardwoodForestMeshSAFRAN=reactiveVal(NULL)
  # coniferousForestMeshSAFRAN=reactiveVal(NULL)
  # mixedForestMeshSAFRAN=reactiveVal(NULL)
  forestMeshSAFRAN=reactiveVal(NULL)
  meadowMeshSAFRAN=reactiveVal(NULL)
  fieldMeshSAFRAN=reactiveVal(NULL)
  orchardMeshSAFRAN=reactiveVal(NULL)
  vineyardMeshSAFRAN=reactiveVal(NULL)

  resetReactiveVal=function(){
    timeRangeSAFRAN(NULL)
    variableToPlotSAFRAN(NULL)
    scaleToPlotSAFRAN(NULL)
    
    weatherSAFRAN(NULL)
    legendSAFRAN(NULL)
    
    cartographySAFRAN(NULL)
    tableSAFRAN(NULL)

    paletteSAFRAN(NULL)
    polygonsSAFRAN(NULL)
    
    altitudeMeshSAFRAN(NULL)
    # meadowMeshSAFRAN(NULL)
    # fieldMeshSAFRAN(NULL)
    # fieldNoIrrigationMeshSAFRAN(NULL)
    # orchardMeshSAFRAN(NULL)
    # vineyardMeshSAFRAN(NULL)
    # hardwoodForestMeshSAFRAN(NULL)
    # coniferousForestMeshSAFRAN(NULL)
    # mixedForestMeshSAFRAN(NULL)
    forestMeshSAFRAN(NULL)
    meadowMeshSAFRAN(NULL)
    fieldMeshSAFRAN(NULL)
    orchardMeshSAFRAN(NULL)
    vineyardMeshSAFRAN(NULL)
  }
  
  observeEvent(input$go,{
    gc()
    
    if(input$timeRangeSAFRAN[2]<input$timeRangeSAFRAN[1]){ # test if the first date of the time range asked by the user actually precede the second
      sendSweetAlert(session, # display an error message if not
                     title="Erreur",
                     type="error",
                     text=paste0("La première date renseignée (",input$timeRangeSAFRAN[1],") 
                                 se situe après la seconde (",input$timeRangeSAFRAN[2],"). 
                                 Veuillez renseigner une plage temporelle où la première 
                                 date précède la seconde."))
      resetReactiveVal()
      return(NULL)
    }
    
    suppressWarnings({
      dataYears=as.numeric(list.files(paste0(path,"/data/meteo_safran/output"))) # folder names found in ~/CSTEcophyto/data/meteo_safran/output 
    })
    if(length(which(is.na(dataYears)))>0){ # test if every folder names within ~/CSTEcophyto/data/meteo_safran/output corresponds to a numeric value (i.e. a year)
      errorFoldersDataYear=list.files(paste0(path,"/data/meteo_safran/output"))[
        which(is.na(dataYears))] # computes folders that are not numeric values (i.e. years)
      
      sendSweetAlert(session, # display an error message if not
                     title="Erreur",
                     type="error",
                     text=paste0("Le dossier ~/CSTEcophyto/data/meteo_safran/output
                     contient un ou des dossier(s) dont le nom ne peut être interprété
                     par une année : ",paste(errorFoldersDataYear,collapse = " et ")))
      resetReactiveVal()
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
      resetReactiveVal()
      return(NULL)
    }
    
    importation=TRUE # test if the application actually needs to re-import weather data
    if(!is.null(variableToPlotSAFRAN()) & !is.null(timeRangeSAFRAN())){
      if(input$variableToPlotSAFRAN==variableToPlotSAFRAN() &
         all(input$timeRangeSAFRAN==timeRangeSAFRAN())){
        importation=FALSE
      }
    }
    
    variable=SAFRANvarList[[which(names(SAFRANvarList)==input$variableToPlotSAFRAN)]]$name
    if(importation){
      if(length(timeRangeYears)==1){ # the time range asked by the user span over only one year
        show_modal_spinner(spin="orbit",text="Importation des données",color="#08298A")
        
        weather=readRDS(paste0(path,"/data/meteo_safran/output/",timeRangeYears,"/",variable,"/",timeRangeYears,"_",variable,".rds"))
      }else if(length(timeRangeYears)==2){ # time range asked by the user span over only two years
        show_modal_spinner(spin="orbit",text="Importation des données",color="#08298A")
        
        weather1=readRDS(paste0(path,"/data/meteo_safran/output/",timeRangeYears[1],"/",variable,"/",timeRangeYears[1],"_",variable,".rds"))
        weather2=readRDS(paste0(path,"/data/meteo_safran/output/",timeRangeYears[2],"/",variable,"/",timeRangeYears[2],"_",variable,".rds"))
        
        weather=rbind(weather1,weather2)
        rm(list=c("weather1","weather2"))
        gc()
      }else{ # time range asked by the user span over more than two years
        sendSweetAlert(session, # display an error message if not
                       title="Erreur",
                       type="error",
                       text=paste0("L'application ne prévoit pas de moyenner des données 
                       climatiques sur une plage temporelle recouvrant plus de deux années."))
        resetReactiveVal()
        return(NULL)
      }
      
      weather=weather[which(weather$Date%in%timeRangeDays),]
      weather=weather[,which(colnames(weather)%in%c("Site",variable))]
      weather=aggregate(.~Site,weather,"mean")
      gc()
    }else{
      weather=weatherSAFRAN()
    }
    
    remove_modal_spinner()
    show_modal_spinner(spin="orbit",text="Agrégation spatiale des données",color="#08298A")
    
    aggregation=TRUE # test if the application actually needs to re-compute polygons values
    if(!is.null(scaleToPlotSAFRAN()) &
       !is.null(altitudeMeshSAFRAN()) &
       # !is.null(meadowMeshSAFRAN()) &
       # !is.null(fieldMeshSAFRAN()) &
       # !is.null(fieldNoIrrigationMeshSAFRAN()) &
       # !is.null(orchardMeshSAFRAN()) &
       # !is.null(vineyardMeshSAFRAN()) &
       # !is.null(hardwoodForestMeshSAFRAN()) &
       # !is.null(coniferousForestMeshSAFRAN()) &
       # !is.null(mixedForestMeshSAFRAN())
       !is.null(forestMeshSAFRAN()) &
       !is.null(meadowMeshSAFRAN()) &
       !is.null(fieldMeshSAFRAN()) &
       !is.null(orchardMeshSAFRAN()) &
       !is.null(vineyardMeshSAFRAN())
       ){
      if(!importation &
         input$scaleToPlotSAFRAN==scaleToPlotSAFRAN() &
         all(input$altitudeMeshSAFRAN==altitudeMeshSAFRAN()) &
         # all(input$meadowMeshSAFRAN==meadowMeshSAFRAN()) &
         # all(input$fieldMeshSAFRAN==fieldMeshSAFRAN()) &
         # all(input$fieldNoIrrigationMeshSAFRAN==fieldNoIrrigationMeshSAFRAN()) &
         # all(input$orchardMeshSAFRAN==orchardMeshSAFRAN()) &
         # all(input$vineyardMeshSAFRAN==vineyardMeshSAFRAN()) &
         # all(input$hardwoodForestMeshSAFRAN==hardwoodForestMeshSAFRAN()) &
         # all(input$coniferousForestMeshSAFRAN==coniferousForestMeshSAFRAN()) &
         # all(input$mixedForestMeshSAFRAN==mixedForestMeshSAFRAN())
         all(input$forestMeshSAFRAN==forestMeshSAFRAN()) &
         all(input$meadowMeshSAFRAN==meadowMeshSAFRAN()) &
         all(input$fieldMeshSAFRAN==fieldMeshSAFRAN()) &
         all(input$orchardMeshSAFRAN==orchardMeshSAFRAN()) &
         all(input$vineyardMeshSAFRAN==vineyardMeshSAFRAN()) 
         ){
        aggregation=FALSE
      }
    }
    
    if(aggregation){
      grid=polygonsSaf
      grid$variable=apply(grid,1,function(row){
        if(as.numeric(row["altitud"])>=input$altitudeMeshSAFRAN[1] & # tile selection according to the characteristics asked by the user
           as.numeric(row["altitud"])<=input$altitudeMeshSAFRAN[2] &
           # as.numeric(row["clc_231"])>=input$meadowMeshSAFRAN[1] &
           # as.numeric(row["clc_231"])<=input$meadowMeshSAFRAN[2] &
           # as.numeric(row["clc_242"])>=input$fieldMeshSAFRAN[1] &
           # as.numeric(row["clc_242"])<=input$fieldMeshSAFRAN[2] &
           # as.numeric(row["clc_211"])>=input$fieldNoIrrigationMeshSAFRAN[1] &
           # as.numeric(row["clc_211"])<=input$fieldNoIrrigationMeshSAFRAN[2] &
           # as.numeric(row["clc_222"])>=input$orchardMeshSAFRAN[1] &
           # as.numeric(row["clc_222"])<=input$orchardMeshSAFRAN[2] &
           # as.numeric(row["clc_221"])>=input$vineyardMeshSAFRAN[1] &
           # as.numeric(row["clc_221"])<=input$vineyardMeshSAFRAN[2] &
           # as.numeric(row["clc_311"])>=input$hardwoodForestMeshSAFRAN[1] &
           # as.numeric(row["clc_311"])<=input$hardwoodForestMeshSAFRAN[2] &
           # as.numeric(row["clc_312"])>=input$coniferousForestMeshSAFRAN[1] &
           # as.numeric(row["clc_312"])<=input$coniferousForestMeshSAFRAN[2] &
           # as.numeric(row["clc_313"])>=input$mixedForestMeshSAFRAN[1] &
           # as.numeric(row["clc_313"])<=input$mixedForestMeshSAFRAN[2]
           as.numeric(row["Forêts"])>=input$forestMeshSAFRAN[1] &
           as.numeric(row["Forêts"])<=input$forestMeshSAFRAN[2] &
           as.numeric(row["Prairies"])>=input$meadowMeshSAFRAN[1] &
           as.numeric(row["Prairies"])<=input$meadowMeshSAFRAN[2] &
           as.numeric(row["Champs"])>=input$fieldMeshSAFRAN[1] &
           as.numeric(row["Champs"])<=input$fieldMeshSAFRAN[2] &
           as.numeric(row["Vergers"])>=input$orchardMeshSAFRAN[1] &
           as.numeric(row["Vergers"])<=input$orchardMeshSAFRAN[2] &
           as.numeric(row["Vignes"])>=input$vineyardMeshSAFRAN[1] &
           as.numeric(row["Vignes"])<=input$vineyardMeshSAFRAN[2]
           ){
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
          result=wtd.mean(intersects$variable,(intersects$area/sum(intersects$area)))
          if(variable=="dli_q"){result=round(result)}else{result=round(result,1)}
          return(result)
        })
        
        polygons$variable_sd=apply(polygons,1,function(row){
          intersects=intersections[which(intersections$nom==row["nom"] & !is.na(intersections$variable)),]
          result=sqrt(wtd.var(intersects$variable,(intersects$area/sum(intersects$area)),normwt=TRUE))
          result=round(result,2)
          return(result)
        })
      }
      
      palette=SAFRANvarList[[which(names(SAFRANvarList)==input$variableToPlotSAFRAN)]]$palette
      polygons$color=apply(polygons,1,function(row){
        if(is.na(row["variable"])){
          return("#898989")
        }else if(as.numeric(row["variable"])<min(as.numeric(names(palette)))){
          return(as.character(palette[1]))
        }else if(as.numeric(row["variable"])>max(as.numeric(names(palette)))){
          return(as.character(palette[length(palette)]))
        }else{
          return(as.character(palette[which(as.numeric(names(palette))==as.numeric(row["variable"]))]))
        }
      })
    }else{
      palette=paletteSAFRAN()
      polygons=polygonsSAFRAN()
    }
    
    remove_modal_spinner()
    show_modal_spinner(spin="orbit",text="Création du graphique",color="#08298A")

    p=ggplot()+
      geom_sf(fill=polygons$color,col=NA,data=polygons)+
      theme(legend.position="none",
            panel.grid=element_blank(),
            panel.background=element_rect(fill="white"),
            axis.line=element_line(colour="black"),
            plot.title=element_text(size=16,hjust=0.5))+
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
    
    table=as.data.frame(polygons)
    table=table[,which(colnames(table)%in%c("nom","variable","variable_sd"))]
    colnames(table)[which(colnames(table)=="nom")]="Zone"
    colnames(table)[which(colnames(table)=="variable")]=paste0("Mean.",variable)
    colnames(table)[which(colnames(table)=="variable_sd")]=paste0("Sd.",variable)
    
    weatherSAFRAN(weather) # stores in reactive values all important objects
    cartographySAFRAN(p)
    legendSAFRAN(legend)
    tableSAFRAN(table)
    timeRangeSAFRAN(input$timeRangeSAFRAN)
    variableToPlotSAFRAN(input$variableToPlotSAFRAN)
    scaleToPlotSAFRAN(input$scaleToPlotSAFRAN)
    paletteSAFRAN(palette)
    polygonsSAFRAN(polygons)
    altitudeMeshSAFRAN(input$altitudeMeshSAFRAN)
    # meadowMeshSAFRAN(input$meadowMeshSAFRAN)
    # fieldMeshSAFRAN(input$fieldMeshSAFRAN)
    # fieldNoIrrigationMeshSAFRAN(input$fieldNoIrrigationMeshSAFRAN)
    # orchardMeshSAFRAN(input$orchardMeshSAFRAN)
    # vineyardMeshSAFRAN(input$vineyardMeshSAFRAN)
    # hardwoodForestMeshSAFRAN(input$hardwoodForestMeshSAFRAN)
    # coniferousForestMeshSAFRAN(input$coniferousForestMeshSAFRAN)
    # mixedForestMeshSAFRAN(input$mixedForestMeshSAFRAN)
    forestMeshSAFRAN(input$forestMeshSAFRAN)
    meadowMeshSAFRAN(input$meadowMeshSAFRAN)
    fieldMeshSAFRAN(input$fieldMeshSAFRAN)
    orchardMeshSAFRAN(input$orchardMeshSAFRAN)
    vineyardMeshSAFRAN(input$vineyardMeshSAFRAN)
    
    remove_modal_spinner()
    return(NULL)
  })

  output$cartographySAFRAN=renderPlot({
    if(!is.null(cartographySAFRAN())){
      isolate({
        if(!is.null(legendSAFRAN())){
          return(grid.arrange(cartographySAFRAN(),legendSAFRAN(),ncol=1,heights=c(8,2)))
        }else{
          return(NULL)
        }
      })
    }else{
      return(NULL)
    }
  })
  
  output$buttonSaveCartoSAFRAN=renderUI({
    if(!is.null(cartographySAFRAN())){
      isolate({
        if(!is.null(legendSAFRAN())){
          return(actionBttn("saveCartoSAFRAN",label=NULL,style="simple",color="success",icon=icon("save")))
        }else{
          return(NULL)
        }
      })
    }else{
      return(NULL)
    }
  })
  
  observeEvent(input$saveCartoSAFRAN,{
    if(!is.null(cartographySAFRAN()) & !is.null(legendSAFRAN())){
      savePath=choose.dir()

      if(is.na(savePath)){
        return(NULL)
      }else{
        variable=substr(variableToPlotSAFRAN(),1,(as.numeric(gregexpr("\\[",variableToPlotSAFRAN()))-2))
        filename=paste0(savePath,"/",variable,"_",timeRangeSAFRAN()[1],"_",timeRangeSAFRAN()[2],"_",scaleToPlotSAFRAN(),".pdf")
        
        if(file.exists(filename)){
          sendSweetAlert(session,
                         title="Erreur",
                         type="error",
                         text=paste0("L'enregistrement à échoué car le fichier ",gsub("\\\\","/",filename)," existe déjà."))
          return(NULL)
        }else{
          pdf(filename)
          p=grid.arrange(cartographySAFRAN(),legendSAFRAN(),ncol=1,heights=c(8,2))
          print(p)
          dev.off()
        }
      }
    }
  })
  
  output$tableSAFRAN=renderDataTable({
    if(!is.null(tableSAFRAN())){
      return(tableSAFRAN())
    }else{
      return(NULL)
    }
  },options=list(pageLength=10,dom="ftp",columnDefs=list(list(className='dt-center',width='60px',targets="_all"))),rownames=FALSE)
  
  output$buttonSaveTableSAFRAN=renderUI({
    if(!is.null(tableSAFRAN())){
      return(actionBttn("saveTableSAFRAN",label=NULL,style="simple",color="success",icon=icon("save")))
    }else{
      return(NULL)
    }
  })

  observeEvent(input$saveTableSAFRAN,{
    if(!is.null(tableSAFRAN())){
      savePath=choose.dir()
      
      if(is.na(savePath)){
        return(NULL)
      }else{
        variable=substr(variableToPlotSAFRAN(),1,(as.numeric(gregexpr("\\[",variableToPlotSAFRAN()))-2))
        filename=paste0(savePath,"/",variable,"_",timeRangeSAFRAN()[1],"_",timeRangeSAFRAN()[2],"_",scaleToPlotSAFRAN(),".csv")
        if(file.exists(filename)){
          sendSweetAlert(session,
                         title="Erreur",
                         type="error",
                         text=paste0("L'enregistrement à échoué car le fichier ",gsub("\\\\","/",filename)," existe déjà."))
          return(NULL)
        }else{
          write.table(tableSAFRAN(),filename,sep=";",row.names=FALSE)
        }
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