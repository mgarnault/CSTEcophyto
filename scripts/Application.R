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
if(!"markdown"%in%rownames(installed.packages())){
  install.packages("markdown")
}
library("markdown")

if(!"htmltools"%in%rownames(installed.packages())){
  install.packages("htmltools")
}
library("htmltools")

if(!"sf"%in%rownames(installed.packages())){
  install.packages("sf")
}
library("sf")

if(!"ggplot2"%in%rownames(installed.packages())){
  install.packages("ggplot2")
}
library("ggplot2")

if(!"gridExtra"%in%rownames(installed.packages())){
  install.packages("gridExtra")
}
library("gridExtra")

if(!"shinydashboard"%in%rownames(installed.packages())){
  install.packages("shinydashboard")
}
library("shinydashboard")

if(!"shiny"%in%rownames(installed.packages())){
  install.packages("shiny")
}
library("shiny")

if(!"shinyjs"%in%rownames(installed.packages())){
  install.packages("shinyjs")
}
library("shinyjs")

if(!"shinyWidgets"%in%rownames(installed.packages())){
  install.packages("shinyWidgets")
}
library("shinyWidgets")

if(!"shinybusy"%in%rownames(installed.packages())){
  install.packages("shinybusy")
}
library("shinybusy")

if(!"shinyFiles"%in%rownames(installed.packages())){
  install.packages("shinyFiles")
}
library("shinyFiles")

if(!"shinyjqui"%in%rownames(installed.packages())){
  install.packages("shinyjqui")
}
library("shinyjqui")

if(!"DT"%in%rownames(installed.packages())){
  install.packages("DT")
}
library("DT")

if(!"Hmisc"%in%rownames(installed.packages())){
  install.packages("Hmisc")
}
library("Hmisc")

if(!"plyr"%in%rownames(installed.packages())){
  install.packages("plyr")
}
library("plyr")

if(!"parallel"%in%rownames(installed.packages())){
  install.packages("parallel")
}
library("parallel")



# Working directory
path=dirname(rstudioapi::getSourceEditorContext()$path)
path=substr(path,1,(as.numeric(gregexpr("/scripts",path))-1))
setwd(path)


# Global variables
SAFRANvarList=list(
  "Humidité relative (moyenne quotidienne) [%]"=list(name="hu_q",palette=c("#ff4343","#e7e7e7","#4a47ff")),
  "Jour de gel (température minimale inf. à 0°C) [j]"=list(name="jgel_q",palette=c("#e7e7e7","#45ffff","#2023ff","#9320ff")),
  "Précipitations liquides (cumul quotidien 06-06 UTC) [mm]"=list(name="preliq_q",palette=c("#ffffb1","#c9ff6a","#51ff43","#5cf8f6","#5bbfff","#5b6aff","#0017ff")),
  "Rayonnement visible (cumul quotidien) [J/cm²]"=list(name="ssi_q",palette=c("#00fff7","#00ff08","#f7ff00","#ff9300","#ff0000","#680000")),
  "Température (moyenne quotidienne) [°C]"=list(name="t_q",palette=c("#ad4bff","#4e4bff","#4bfffc","#4bff59","#fcff3b","#ff3b3b","#b22727")),
  "Température maximale des 24 températures horaires [°C]"=list(name="tsup_h_q",palette=c("#4bff59","#fcff3b","#ff3b3b","#b22727")),
  "Température minimale des 24 températures horaires [°C]"=list(name="tinf_h_q",palette=c("#ad4bff","#4e4bff","#4bfffc","#4bff59")),
  "Vent à 10m (moyenne quotidienne) [m/s]"=list(name="ff_q",palette=c("#00ded7","#00de1b","#fffc00","#de0000","#895e00","#000000")))
# colFunction=colorRampPalette(SAFRANvarList[["Jour de gel (température minimale <0°C) [j]"]]$palette)
# plot(x=c(1:100),y=rep(1,100),col=colFunction(100),pch=15,xaxt="n",yaxt="n",ann=FALSE,frame.plot=FALSE)

polygonsCom=readRDS(paste0(path,"/data/shapes_administration/output/communes_metropole_lite/rds/communes_metropole_lite.rds"))
polygonsDep=readRDS(paste0(path,"/data/shapes_administration/output/departements_metropole_lite/rds/departements_metropole_lite.rds"))
polygonsReg=readRDS(paste0(path,"/data/shapes_administration/output/regions_metropole_lite/rds/regions_metropole_lite.rds"))
polygonsFra=readRDS(paste0(path,"/data/shapes_administration/output/france_metropole_lite/rds/france_metropole_lite.rds"))

polygonsSaf=readRDS(paste0(path,"/data/mailles_safran/output/SAFRANgrid_handmade/rds/SAFRANgrid_handmade.rds"))
polygonsSaf=polygonsSaf[-which(polygonsSaf$dprtmnt==""),]
dataPolygonsSaf=as.data.frame(polygonsSaf[,which(colnames(polygonsSaf)%in%
                                                   c("clc_211","clc_221","clc_222","clc_231","clc_241","clc_242","clc_311","clc_312","clc_313"))])
dataPolygonsSaf=dataPolygonsSaf[,-ncol(dataPolygonsSaf)]
polygonsSaf$Vegetal=rowSums(dataPolygonsSaf)
polygonsSaf$Forets=rowSums(dataPolygonsSaf[,which(colnames(dataPolygonsSaf)%in%c("clc_311","clc_312","clc_313"))])
polygonsSaf$Champs=rowSums(dataPolygonsSaf[,which(colnames(dataPolygonsSaf)%in%c("clc_241","clc_242","clc_211"))])
polygonsSaf$Prairies=dataPolygonsSaf[,which(colnames(dataPolygonsSaf)%in%c("clc_231"))]
polygonsSaf$Vergers=dataPolygonsSaf[,which(colnames(dataPolygonsSaf)%in%c("clc_222"))]
polygonsSaf$Vignes=dataPolygonsSaf[,which(colnames(dataPolygonsSaf)%in%c("clc_221"))]
# hist(polygonsSaf$Vegetal)






# USER INTERFACE #
ui=dashboardPage(
  dashboardHeader(title="CST Ecophyto II+ Tool",titleWidth=240),
  dashboardSidebar(width=240,
                   sidebarMenu(id="sidebarmenu",
                               menuItem(HTML("<span style='font-size:17px; position: relative; top: 1px'>Accueil</span>"),tabName="HOME",icon=icon("home")),
                               
                               hr(style="border-top: 1px solid #000000;"),
                               
                               menuItem(HTML("<span style='font-size:17px; position: relative; top: 1px'>Météorologie SAFRAN</span>"),tabName="SAFRAN",icon=icon("temperature-high")),
                               conditionalPanel("input.sidebarmenu=='SAFRAN'",
                                                fluidRow(column(12,dateRangeInput("timeRangeSAFRAN","Plage temporelle"))),
                                                
                                                fluidRow(column(12,pickerInput("variableToPlotSAFRAN","Variable climatique",
                                                                               choices=names(SAFRANvarList),
                                                                               multiple=FALSE))),
                                                
                                                fluidRow(column(12,pickerInput("methodToApplySAFRAN","Méthode d'agrégation temporelle",
                                                                               choices=c("Moyenne","Minimum","Maximum","Somme","Quantile 5%","Quantile 25%","Quantile 75%","Quantile 95%"),
                                                                               multiple=FALSE))),
                                                
                                                fluidRow(column(12,pickerInput("scaleToPlotSAFRAN","Échelle d'agrégation spatiale",
                                                                               choices=c("Mailles SAFRAN","Départements","Régions","France entière"),
                                                                               multiple=FALSE))),
                                                
                                                fluidRow(column(12,pickerInput("bordersToPlotSAFRAN","Frontières spatiales à afficher",
                                                                               choices=c("Mailles SAFRAN","Communes","Départements","Régions","France entière"),
                                                                               multiple=TRUE))),
                                                
                                                fluidRow(column(12,awesomeCheckbox(inputId="showMeshCharac",
                                                                                   label="Affichage des contraintes appliquées aux mailles SAFRAN",status="primary"))),
                                                conditionalPanel("input.showMeshCharac==1",
                                                                 fluidRow(column(2),
                                                                          column(10,sliderInput("altitudeMeshSAFRAN","Altitude moyenne [m]",
                                                                                                0,3000,c(0,800),ticks=FALSE))),
                                                                 fluidRow(column(2),
                                                                          column(10,sliderInput("vegetalMeshSAFRAN","Couverture végétale [%]",
                                                                                                0,100,c(20,100),ticks=FALSE))),
                                                                 fluidRow(column(2),
                                                                          column(10,sliderInput("forestMeshSAFRAN","Forêts [%]",
                                                                                                0,100,c(0,80),ticks=FALSE))),
                                                                 fluidRow(column(2),
                                                                          column(10,sliderInput("meadowMeshSAFRAN","Prairies [%]",
                                                                                                0,100,c(0,80),ticks=FALSE))),
                                                                 fluidRow(column(2),
                                                                          column(10,sliderInput("fieldMeshSAFRAN","Cultures annuelles [%]",
                                                                                                0,100,c(0,100),ticks=FALSE))),
                                                                 fluidRow(column(2),
                                                                          column(10,sliderInput("orchardMeshSAFRAN","Vergers [%]",
                                                                                                0,100,c(0,100),ticks=FALSE))),
                                                                 fluidRow(column(2),
                                                                          column(10,sliderInput("vineyardMeshSAFRAN","Vignobles [%]",
                                                                                                0,100,c(0,100),ticks=FALSE)))),
                                                
                                                fluidRow(column(12,align="center",actionBttn("go","Go",icon=icon("sync-alt"))))),
                               
                               hr(style="border-top: 1px solid #000000;"),
                               
                               menuItem(HTML("<span style='font-size:17px; position: relative; top: 1px'>Vente pesticides BNV-D</span>"),tabName="BNVD",icon=icon("spray-can")),
                               
                               hr(style="border-top: 1px solid #000000;"),
                               
                               menuItem(HTML("<span style='font-size:17px; position: relative; top: 1px'>Outil de création</span>"),tabName="OWNMAP",icon=icon("globe-europe")),
                               
                               conditionalPanel("input.sidebarmenu=='OWNMAP'",
                                                p(""),
                                                shinyFilesButton(id="fileChoose1",label="Importer un CSV",title="Sélectionner un fichier",multiple=FALSE)),
                               
                               hr(style="border-top: 1px solid #000000;"))),
  
  dashboardBody(tags$head(tags$style(HTML('.wrapper {height: auto !important; position:relative; overflow-x:hidden; overflow-y:hidden}'))),
                tags$head(tags$script('
                // Define function to set height containers
                setHeight = function() {
                  var window_height = $(window).height();
                  var header_height = $(".main-header").height();
                  var boxHeight = window_height - header_height - 100;
                  
                  $("#mapContainerSAFRAN").height(boxHeight);
                  $("#tableContainerSAFRAN").height(boxHeight);
                };
                
                // Set input$box_height when the connection is established
                $(document).on("shiny:connected", function(event) {
                  setHeight();
                });
                
                // Refresh the box height on every window resize event
                $(window).on("resize", function(){
                  setHeight();
                });')),
                
                tabItems(
                  tabItem(tabName="HOME",
                          includeMarkdown(paste0(path,"/README.md"))),
                  
                  tabItem(tabName="SAFRAN",
                          fluidRow(column(8,box(id="mapContainerSAFRAN",
                                                title=p("Cartographie",HTML("&nbsp;"),uiOutput("paletteButtonSAFRAN",inline=TRUE),
                                                        HTML("&nbsp; &nbsp; &nbsp; &nbsp; &nbsp;"),
                                                        uiOutput("buttonSaveCartoSAFRANPDF",inline=TRUE),
                                                        uiOutput("buttonSaveCartoSAFRANJPG",inline=TRUE)),
                                                width=12,solidHeader=TRUE,status="primary",
                                                fluidRow(column(12,align="center",plotOutput("cartographySAFRAN"))))),
                                   
                                   column(4,box(id="tableContainerSAFRAN",
                                                title=p("Tableau de données",HTML("&nbsp; &nbsp; &nbsp; &nbsp; &nbsp;"),
                                                        uiOutput("buttonSaveTableSAFRAN",inline=TRUE)),
                                                width=12,solidHeader=TRUE,status="primary",
                                                div(dataTableOutput("tableSAFRAN",width="100%",height=618),style="font-size:80%"))))),
                  
                  tabItem(tabName="BNVD",
                          h2("Graphique d'utilisation des PPP")),
                  
                  tabItem(tabName="OWNMAP",
                          h2("Faites votre propre cartographie")))))






# SERVER #
server=function(input,output,session){
  volumes=getVolumes()
  shinyFileChoose(input,"fileChoose1",roots=volumes,session=session)
  
  
  # Reactive values creation
  timeRangeSAFRAN=reactiveVal(NULL)
  methodToApplySAFRAN=reactiveVal(NULL)
  variableToPlotSAFRAN=reactiveVal(NULL)
  scaleToPlotSAFRAN=reactiveVal(NULL)
  
  weatherSAFRAN=reactiveVal(NULL)
  polygonsSAFRAN=reactiveVal(NULL)
  dataPlotSAFRAN=reactiveVal(NULL)
  plotSize=reactiveVal(550)
  
  cartographySAFRAN=reactiveVal(NULL)
  legendSAFRAN=reactiveVal(NULL)
  tableSAFRAN=reactiveVal(NULL)
  
  minValPaletteSAFRAN=reactiveVal(NULL)
  maxValPaletteSAFRAN=reactiveVal(NULL)
  labelInfPaletteSAFRAN=reactiveVal(NULL)
  labelSupPaletteSAFRAN=reactiveVal(NULL)
  paletteBreaksSAFRAN=reactiveVal(NULL)
  paletteValuesSAFRAN=reactiveVal(NULL)
  
  altitudeMeshSAFRAN=reactiveVal(NULL)
  vegetalMeshSAFRAN=reactiveVal(NULL)
  forestMeshSAFRAN=reactiveVal(NULL)
  meadowMeshSAFRAN=reactiveVal(NULL)
  fieldMeshSAFRAN=reactiveVal(NULL)
  orchardMeshSAFRAN=reactiveVal(NULL)
  vineyardMeshSAFRAN=reactiveVal(NULL)
  
  
  
  # Function to reset reactive values
  # resetReactiveVal=function(){
  #   timeRangeSAFRAN(NULL)
  #   methodToApplySAFRAN(NULL)
  #   variableToPlotSAFRAN(NULL)
  #   scaleToPlotSAFRAN(NULL)
  #   
  #   weatherSAFRAN(NULL)
  #   polygonsSAFRAN(NULL)
  #   dataPlotSAFRAN(NULL)
  #   plotSize(550)
  #
  #   cartographySAFRAN(NULL)
  #   legendSAFRAN(NULL)
  #   tableSAFRAN(NULL)
  #   
  #   minValPaletteSAFRAN(NULL)
  #   maxValPaletteSAFRAN(NULL)
  #   labelInfPaletteSAFRAN(NULL)
  #   labelSupPaletteSAFRAN(NULL)
  #   paletteBreaksSAFRAN(NULL)
  #   paletteValuesSAFRAN(NULL)
  #   
  #   altitudeMeshSAFRAN(NULL)
  #   vegetalMeshSAFRAN(NULL)
  #   forestMeshSAFRAN(NULL)
  #   meadowMeshSAFRAN(NULL)
  #   fieldMeshSAFRAN(NULL)
  #   orchardMeshSAFRAN(NULL)
  #   vineyardMeshSAFRAN(NULL)
  # }
  
  
  
  # Button allowing the user to change color scale bounds
  output$paletteButtonSAFRAN=renderUI({
    if(!is.null(cartographySAFRAN())){
      isolate({
        unit=substr(variableToPlotSAFRAN(),
                    (as.numeric(gregexpr("\\[",variableToPlotSAFRAN()))),
                    (as.numeric(gregexpr("\\]",variableToPlotSAFRAN()))))
        
        return(dropdownButton(
          fluidRow(column(12,
                          fluidRow(column(6,
                                          fluidRow(style="height:85px;",numericInput("minValPaletteSAFRAN",
                                                                                     HTML(paste0("<p><span style='color: #222d32; position: relative; top: 10px;'>Borne<br>inférieure ",unit,"</span></p>")),
                                                                                     minValPaletteSAFRAN(),min=-Inf,max=+Inf,step=.1)),
                                          fluidRow(style="height:85px;",numericInput("maxValPaletteSAFRAN",
                                                                                     HTML(paste0("<p><span style='color: #222d32; position: relative; top: 10px;'>Borne<br>supérieure ",unit,"</span></p>")),
                                                                                     maxValPaletteSAFRAN(),min=-Inf,max=+Inf,step=.1))),
                                   column(6,
                                          fluidRow(style="height:50px;"),
                                          fluidRow(style="height:35px;",column(1),column(11,align="center",awesomeCheckbox("labelInfPaletteSAFRAN",
                                                                                                                           HTML(paste0("<p align=left><b><span style='color: #222d32; position: relative; bottom: 40px; right: 25px'> Afficher le<br>label <q> < </q> </span></b></p>")),
                                                                                                                           value=labelInfPaletteSAFRAN()))),
                                          fluidRow(style="height:50px;"),
                                          fluidRow(style="height:35px;",column(1),column(11,align="center",awesomeCheckbox("labelSupPaletteSAFRAN",
                                                                                                                           HTML(paste0("<p align=left><b><span style='color: #222d32; position: relative; bottom: 40px; right: 25px;'> Afficher le<br>label <q> > </q> </span></b></p>")),
                                                                                                                           value=labelSupPaletteSAFRAN()))))),
                          fluidRow(column(10,
                                          fluidRow(sliderInput("sizePlotSAFRAN",
                                                               HTML(paste0("<p><span style='color: #222d32; position: relative; top: 10px;'>Taille du graphique</span></p>")),
                                                               0,1000,plotSize(),ticks=FALSE)))))),
          size="sm",
          status="default",
          circle=TRUE,
          icon=icon("palette"),
          width=275,
          margin=20,
          tooltip=tooltipOptions(title="Changer les paramètres graphiques"),
          inline=TRUE))
      })
    }else{
      return(NULL)
    }
  })
  
  
  
  # "Go" button listener : cartography plot & datatable computing
  observeEvent(input$go,{
    gc()
    
    
    # Initial tests
    if(input$timeRangeSAFRAN[2]<input$timeRangeSAFRAN[1]){ # test if the first date of the time range asked by the user actually precede the second
      sendSweetAlert(session, # display an error message if not
                     title="Erreur",
                     type="error",
                     text=paste0("La première date renseignée (",input$timeRangeSAFRAN[1],") 
                                 se situe après la seconde (",input$timeRangeSAFRAN[2],"). 
                                 Veuillez renseigner une plage temporelle où la première 
                                 date précède la seconde."))
      # resetReactiveVal()
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
      # resetReactiveVal()
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
                     ne contient pas de données sur toute ou partie de la plage 
                     temporelle demandée (du ",input$timeRangeSAFRAN[1]," au ",input$timeRangeSAFRAN[2],
                                 "). Les années disponibles dans la base données SAFRAN sont : ",
                                 timeRangesData))
      # resetReactiveVal()
      return(NULL)
    }
    
    if(!is.null(input$minValPaletteSAFRAN)){ # test if the user gave actual numeric values for bounds legend
      if(is.na(input$minValPaletteSAFRAN) | is.na(input$maxValPaletteSAFRAN)){
        sendSweetAlert(session, # display an error message if not
                       title="Erreur",
                       type="error",
                       text="Au moins une borne renseignée pour la légende de couleur
                       n'est pas considérée comme une valeur numérique.")
        # resetReactiveVal()
        return(NULL)
      }
    }
    
    
    
    # Weather dataset importation
    show_modal_spinner(spin="orbit",text="Importation et agrégation temporelle des données",color="#08298A")
    
    importation=TRUE # test if the application actually needs to re-import weather data
    if(!is.null(variableToPlotSAFRAN())){
      if(input$variableToPlotSAFRAN==variableToPlotSAFRAN() &
         input$methodToApplySAFRAN==methodToApplySAFRAN() &
         all(input$timeRangeSAFRAN==timeRangeSAFRAN())){
        importation=FALSE
      }
    }
    
    variable=SAFRANvarList[[which(names(SAFRANvarList)==input$variableToPlotSAFRAN)]]$name
    if(importation){
      if(length(timeRangeYears)<=2){ # the time range asked by the user span over less than 2 years
        weather=readRDS(paste0(path,"/data/meteo_safran/output/",timeRangeYears[1],"/",variable,"/",timeRangeYears[1],"_",variable,".rds"))
        
        if(length(timeRangeYears)>1){
          for(i in timeRangeYears[2:length(timeRangeYears)]){
            temp=readRDS(paste0(path,"/data/meteo_safran/output/",i,"/",variable,"/",i,"_",variable,".rds"))
            weather=rbind(weather,temp)
            rm(list=c("temp"))
            gc()
          }
        }
      }else{ # time range asked by the user span over more than 2 years
        remove_modal_spinner()
        
        sendSweetAlert(session, # display an error message if not
                       title="Erreur",
                       type="error",
                       text=paste0("L'application ne prévoit pas de moyenner des données 
                       climatiques sur une plage temporelle recouvrant plus de deux années."))
        # resetReactiveVal()
        return(NULL)
      }
      
      weather=weather[which(weather$Date%in%timeRangeDays),]
      weather=weather[,which(colnames(weather)%in%c("Site",variable))]
      weather=aggregate(.~Site,weather,function(x){
        if(input$methodToApplySAFRAN=="Moyenne"){
          return(mean(x))
        }else if(input$methodToApplySAFRAN=="Minimum"){
          return(min(x))
        }else if(input$methodToApplySAFRAN=="Maximum"){
          return(max(x))
        }else if(input$methodToApplySAFRAN=="Somme"){
          return(sum(x))
        }else if(input$methodToApplySAFRAN=="Quantile 5%"){
          return(as.numeric(quantile(x,.05)))
        }else if(input$methodToApplySAFRAN=="Quantile 25%"){
          return(as.numeric(quantile(x,.25)))
        }else if(input$methodToApplySAFRAN=="Quantile 75%"){
          return(as.numeric(quantile(x,.75)))
        }else if(input$methodToApplySAFRAN=="Quantile 95%"){
          return(as.numeric(quantile(x,.95)))
        }
      })
      gc()
    }else{
      weather=weatherSAFRAN()
    }
    
    remove_modal_spinner()
    
    
    # Spatial aggregation of meteorological data
    show_modal_spinner(spin="orbit",text="Sélection des mailles SAFRAN et agrégation spatiale des données",color="#08298A")
    
    aggregation=TRUE # test if the application actually needs to re-compute polygons values
    if(!importation){
      if(input$scaleToPlotSAFRAN==scaleToPlotSAFRAN() &
         all(input$altitudeMeshSAFRAN==altitudeMeshSAFRAN()) &
         all(input$vegetalMeshSAFRAN==vegetalMeshSAFRAN()) &
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
      grid$variable=as.numeric(apply(grid,1,function(row){
        return(weather[which(weather$Site==as.numeric(row["mll_sfr"])),variable])
      }))
      
      grid$variable[which(grid$"altitud"<input$altitudeMeshSAFRAN[1] |
                            grid$"altitud">input$altitudeMeshSAFRAN[2] |
                            grid$"Vegetal"<input$vegetalMeshSAFRAN[1] |
                            grid$"Vegetal">input$vegetalMeshSAFRAN[2] |
                            grid$"Forets"<input$forestMeshSAFRAN[1] |
                            grid$"Forets">input$forestMeshSAFRAN[2] |
                            grid$"Prairies"<input$meadowMeshSAFRAN[1] |
                            grid$"Prairies">input$meadowMeshSAFRAN[2] |
                            grid$"Champs"<input$fieldMeshSAFRAN[1] |
                            grid$"Champs">input$fieldMeshSAFRAN[2] |
                            grid$"Vergers"<input$orchardMeshSAFRAN[1] |
                            grid$"Vergers">input$orchardMeshSAFRAN[2] |
                            grid$"Vignes"<input$vineyardMeshSAFRAN[1] |
                            grid$"Vignes">input$vineyardMeshSAFRAN[2])]=NA
      grid=grid[,which(colnames(grid)%in%c("geometry","variable","mll_sfr","latitud","longitd"))]
      
      if(length(which(is.na(grid$variable)))==nrow(grid)){ # test if contrasts applied to SAFRAN tiles allow to compute a map (at least 1 value) 
        remove_modal_spinner()
        
        sendSweetAlert(session, # display an error message if not
                       title="Erreur",
                       type="error",
                       text=paste0("Aucune maille SAFRAN ne répond aux contraintes demandées."))
        # resetReactiveVal()
        return(NULL)
      }
      
      if(input$scaleToPlotSAFRAN=="Mailles SAFRAN"){
        polygons=grid
        polygons=polygons[,which(colnames(polygons)%in%c("geometry","variable","mll_sfr"))]
        colnames(polygons)[which(colnames(polygons)=="mll_sfr")]="nom"
      }else{
        grid=grid[-which(is.na(grid$variable)),-which(colnames(grid)=="mll_sfr")]
        
        if(input$scaleToPlotSAFRAN=="Départements"){
          polygons=polygonsDep
        }else if(input$scaleToPlotSAFRAN=="Régions"){
          polygons=polygonsReg
        }else if(input$scaleToPlotSAFRAN=="France entière"){
          polygons=polygonsFra
        }
        polygons=polygons[,which(colnames(polygons)%in%c("geometry","nom"))]
        
        locations=as.character(polygons$nom)
        cl=makeCluster(spec=(detectCores()-1)) # clusters creation
        clusterExport(cl,c("grid","polygons"),envir=environment()) # exportation of R objects in clusters
        clusterEvalQ(cl,library("sf")) # importing the "sf" library in clusters
        sfList=parLapply(cl,locations,function(loc){
          gc()
          poly=polygons[which(polygons$nom==loc),]
          buffer=st_buffer(poly,10000)
          bounds=st_bbox(buffer)
          mesh=grid[which(grid$latitud>=as.numeric(bounds["ymin"]) &
                            grid$latitud<=as.numeric(bounds["ymax"]) &
                            grid$longitd>=as.numeric(bounds["xmin"]) &
                            grid$longitd<=as.numeric(bounds["xmax"])),]
          
          intersections=st_intersection(mesh,poly)
          intersections$area=as.numeric(st_area(intersections))
          return(intersections)
        })
        stopCluster(cl)
        gc()
        
        intersections=do.call("rbind",sfList)
        
        polygons$variable=as.numeric(apply(polygons,1,function(row){
          intersects=intersections[which(intersections$nom==row["nom"] & !is.na(intersections$variable)),]
          result=wtd.mean(intersects$variable,(intersects$area/sum(intersects$area)))
          return(result)
        }))
        
        polygons$variable_cv=as.numeric(apply(polygons,1,function(row){
          intersects=intersections[which(intersections$nom==row["nom"] & !is.na(intersections$variable)),]
          sd=sqrt(wtd.var(intersects$variable,(intersects$area/sum(intersects$area)),normwt=TRUE))
          result=(sd/as.numeric(row["variable"]))*100
          return(result)
        }))
      }
    }else{
      polygons=polygonsSAFRAN()
    }
    
    remove_modal_spinner()
    
    
    # Color palette creation and color attribution to spatial polygons
    show_modal_spinner(spin="orbit",text="Création du graphique",color="#08298A")
    
    colorization=TRUE # test if the application actually needs recompute color scale and polygon colors
    if(!aggregation){
      if(input$minValPaletteSAFRAN==minValPaletteSAFRAN() &
         input$maxValPaletteSAFRAN==maxValPaletteSAFRAN() &
         input$labelInfPaletteSAFRAN==labelInfPaletteSAFRAN() &
         input$labelSupPaletteSAFRAN==labelSupPaletteSAFRAN()){
        colorization=FALSE
      }
    }
    
    if(colorization){
      if(!is.null(input$minValPaletteSAFRAN) & !is.null(input$maxValPaletteSAFRAN) & !aggregation){
        bigStep=((input$maxValPaletteSAFRAN-input$minValPaletteSAFRAN)/9)
        paletteBreaks=round_any(seq(input$minValPaletteSAFRAN,input$maxValPaletteSAFRAN,bigStep),10^(ceiling(log10(bigStep))-2))
      }else{
        if(min(polygons$variable,na.rm=TRUE)==max(polygons$variable,na.rm=TRUE)){
          val=min(polygons$variable,na.rm=TRUE)
          bound=10^(ceiling(log10(val))-1)
          
          if((val-bound)<0 & variable%in%c("hu_q","jgel_q","preliq_q","dli_q","ff_q")){
            minimum=0
          }else{
            minimum=val-bound
          }
          if((val+bound)>100 & variable=="hu_q" & input$methodToApplySAFRAN!="Somme"){ ###### IF VAL+BOUND=1 & VAR=="jgel_q" & Method!=SOMME
            maximum=100
          }else{
            maximum=val+bound
          }
          
          bigStep=((maximum-minimum)/9)
          paletteBreaks=round_any(seq(minimum,maximum,bigStep),10^(ceiling(log10(bigStep))-2))
          
        }else{
          bigStep=((max(polygons$variable,na.rm=TRUE)-min(polygons$variable,na.rm=TRUE))/9)
          paletteBreaks=round_any(seq(min(polygons$variable,na.rm=TRUE),max(polygons$variable,na.rm=TRUE),bigStep),10^(ceiling(log10(bigStep))-2))
        }
      }
      
      if(max(nchar(paletteBreaks),na.rm=TRUE)>5){
        names(paletteBreaks)=formatC(paletteBreaks,format="e",1)
      }else{
        names(paletteBreaks)=paletteBreaks
      }
      
      if(!is.null(input$labelInfPaletteSAFRAN) & !is.null(input$labelSupPaletteSAFRAN) & !aggregation){
        if(input$labelInfPaletteSAFRAN){
          names(paletteBreaks)[1]=paste0("<",names(paletteBreaks)[1])
        }
        if(input$labelSupPaletteSAFRAN){
          names(paletteBreaks)[length(paletteBreaks)]=paste0(">",names(paletteBreaks)[length(paletteBreaks)])
        }
      }else{
        if(!(min(paletteBreaks)==0 & variable%in%c("hu_q","jgel_q","preliq_q","dli_q","ff_q"))){
          names(paletteBreaks)[1]=paste0("<",names(paletteBreaks)[1])
        }
        if(!(max(paletteBreaks)==100 & variable=="hu_q" & input$methodToApplySAFRAN!="Somme")){
          names(paletteBreaks)[length(paletteBreaks)]=paste0(">",names(paletteBreaks)[length(paletteBreaks)])
        }
      }
      
      smallStep=((max(paletteBreaks)-min(paletteBreaks))/250)
      paletteValues=seq(min(paletteBreaks),max(paletteBreaks),smallStep)
      colFunction=colorRampPalette(SAFRANvarList[[which(names(SAFRANvarList)==input$variableToPlotSAFRAN)]]$palette)
      names(paletteValues)=colFunction(length(paletteValues))
      
      dataPlot=polygons
      dataPlot$color=apply(dataPlot,1,function(row){
        if(is.na(row["variable"])){
          return("#898989")
        }else if(as.numeric(row["variable"])<min(paletteValues)){
          return(names(paletteValues)[1])
        }else if(as.numeric(row["variable"])>max(paletteValues)){
          return(names(paletteValues)[length(paletteValues)])
        }else{
          return(names(paletteValues)[which.min(abs(paletteValues-as.numeric(row["variable"])))])
        }
      })
    }else{
      paletteBreaks=paletteBreaksSAFRAN()
      paletteValues=paletteValuesSAFRAN()
      dataPlot=dataPlotSAFRAN()
    }
    
    
    # Main plot (map) making
    p=ggplot()+
      geom_sf(fill=dataPlot$color,col=NA,data=dataPlot)+
      theme(legend.position="none",
            panel.grid=element_blank(),
            panel.background=element_rect(fill="white"),
            axis.line=element_line(colour="black"),
            plot.title=element_text(size=15,hjust=0.5))+
      ggtitle(paste0(" Variable : ",input$variableToPlotSAFRAN,
                     "\n Plage temporelle : du ",input$timeRangeSAFRAN[1]," au ",input$timeRangeSAFRAN[2]," (",input$methodToApplySAFRAN,")",
                     "\n Résolution spatiale : ",input$scaleToPlotSAFRAN))
    
    
    # Adding polygons frontiers on the map if asked by the user
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
    
    
    # Second plot (legend) making
    palColors=names(paletteValues)
    palValues=as.numeric(paletteValues)
    breaksLabel=names(paletteBreaks)
    breaksValues=as.numeric(paletteBreaks)
    overlap=(palValues[2]-palValues[1])+((palValues[2]-palValues[1])/10)
    
    legend=ggplot()+
      scale_x_continuous(limits=c((min(palValues)-((max(palValues)-min(palValues))/20)),
                                  (max(palValues)+((max(palValues)-min(palValues))/20))),
                         expand=c(0,0))+
      scale_y_continuous(limits=c(0,1),expand=c(0,0))+
      geom_tile(aes(x=palValues,y=0.5),height=0.2,width=overlap,
                fill=palColors,col=NA)+
      geom_segment(aes(x=breaksValues,xend=breaksValues,y=0.43,yend=0.32),size=.8)+
      geom_text(aes(x=mean(breaksValues),y=0.75,label=input$variableToPlotSAFRAN),
                hjust=0.5,vjust=0.5,size=5)+
      theme_void()
    
    
    # Creating the displayed datatable containing numerical data for each polygons chosen by the user
    unit=substr(input$variableToPlotSAFRAN,
                (as.numeric(gregexpr("\\[",input$variableToPlotSAFRAN))),
                (as.numeric(gregexpr("\\]",input$variableToPlotSAFRAN))))
    table=as.data.frame(dataPlot)
    table=table[,which(colnames(table)%in%c("nom","variable","variable_cv"))]
    colnames(table)[which(colnames(table)=="nom")]="Zone"
    colnames(table)[which(colnames(table)=="variable")]=paste0("Moyenne ",unit)
    if(ncol(table)>2){colnames(table)[which(colnames(table)=="variable_cv")]="VarCoef [%]"}
    
    
    # Storing relevant variables in reactive values (minimize useless re-calculation)
    timeRangeSAFRAN(input$timeRangeSAFRAN)
    methodToApplySAFRAN(input$methodToApplySAFRAN)
    variableToPlotSAFRAN(input$variableToPlotSAFRAN)
    scaleToPlotSAFRAN(input$scaleToPlotSAFRAN)
    
    weatherSAFRAN(weather)
    polygonsSAFRAN(polygons)
    dataPlotSAFRAN(dataPlot)
    if(!is.null(input$sizePlotSAFRAN)){plotSize(input$sizePlotSAFRAN)}
    
    cartographySAFRAN(p)
    legendSAFRAN(legend)
    tableSAFRAN(table)
    
    minValPaletteSAFRAN(min(paletteBreaks))
    maxValPaletteSAFRAN(max(paletteBreaks))
    labelInfPaletteSAFRAN(grepl("<",names(paletteBreaks)[1]))
    labelSupPaletteSAFRAN(grepl(">",names(paletteBreaks)[length(paletteBreaks)]))
    paletteBreaksSAFRAN(paletteBreaks)
    paletteValuesSAFRAN(paletteValues)
    
    altitudeMeshSAFRAN(input$altitudeMeshSAFRAN)
    vegetalMeshSAFRAN(input$vegetalMeshSAFRAN)
    forestMeshSAFRAN(input$forestMeshSAFRAN)
    meadowMeshSAFRAN(input$meadowMeshSAFRAN)
    fieldMeshSAFRAN(input$fieldMeshSAFRAN)
    orchardMeshSAFRAN(input$orchardMeshSAFRAN)
    vineyardMeshSAFRAN(input$vineyardMeshSAFRAN)
    
    remove_modal_spinner()
    return(NULL)
  })
  
  
  
  # Rendering the map
  output$cartographySAFRAN=renderPlot({
    if(!is.null(cartographySAFRAN())){
      isolate({
        carto=cartographySAFRAN()
        breaksLabel=names(paletteBreaksSAFRAN())
        breaksValues=as.numeric(paletteBreaksSAFRAN())
        legend=legendSAFRAN()+
          geom_text(aes(x=breaksValues,y=0.3,label=breaksLabel),hjust=1,vjust=1,angle=45,size=4)
        
        invisible({plot=grid.arrange(carto,legend,ncol=1,heights=c(7.5,2.5))})
        return(plot)
      })
    }else{
      return(NULL)
    }
  },width=function(){plotSize()},height=function(){plotSize()})
  
  
  
  # Saving the map in PDF
  output$buttonSaveCartoSAFRANPDF=renderUI({ # button
    if(!is.null(cartographySAFRAN())){
      return(actionBttn("saveCartoSAFRANPDF",label=".pdf",style="simple",color="success",icon=icon("save")))
    }else{
      return(NULL)
    }
  })
  
  observeEvent(input$saveCartoSAFRANPDF,{ # button listener
    if(!is.null(cartographySAFRAN())){
      savePath=choose.dir()
      
      if(is.na(savePath)){
        return(NULL)
      }else{
        variable=substr(variableToPlotSAFRAN(),1,(as.numeric(gregexpr("\\[",variableToPlotSAFRAN()))-2))
        filename=paste0(savePath,"/",variable,"_du_",timeRangeSAFRAN()[1],"_au_",timeRangeSAFRAN()[2],"_(",methodToApplySAFRAN(),")_",scaleToPlotSAFRAN(),".pdf")
        
        if(file.exists(filename)){
          sendSweetAlert(session,
                         title="Erreur",
                         type="error",
                         text=paste0("L'enregistrement à échoué car le fichier ",gsub("\\\\","/",filename)," existe déjà."))
          return(NULL)
        }else{
          carto=cartographySAFRAN()
          breaksLabel=names(paletteBreaksSAFRAN())
          breaksValues=as.numeric(paletteBreaksSAFRAN())
          legend=legendSAFRAN()+
            geom_text(aes(x=breaksValues,y=0.3,label=breaksLabel),hjust=1,vjust=1,angle=45,size=3)
          
          pdf(filename,7,7)
          invisible({plot=grid.arrange(carto,legend,ncol=1,heights=c(7.5,2.5))})
          dev.off()
          return(NULL)
        }
      }
    }else{
      return(NULL)
    }
  })
  
  
  
  # Saving the map in JPG
  output$buttonSaveCartoSAFRANJPG=renderUI({ # button
    if(!is.null(cartographySAFRAN())){
      return(actionBttn("saveCartoSAFRANJPG",label=".jpg",style="simple",color="success",icon=icon("save")))
    }else{
      return(NULL)
    }
  })
  
  observeEvent(input$saveCartoSAFRANJPG,{ # button listener
    if(!is.null(cartographySAFRAN())){
      savePath=choose.dir()
      
      if(is.na(savePath)){
        return(NULL)
      }else{
        variable=substr(variableToPlotSAFRAN(),1,(as.numeric(gregexpr("\\[",variableToPlotSAFRAN()))-2))
        filename=paste0(savePath,"/",variable,"_du_",timeRangeSAFRAN()[1],"_au_",timeRangeSAFRAN()[2],"_(",methodToApplySAFRAN(),")_",scaleToPlotSAFRAN(),".jpg")
        
        if(file.exists(filename)){
          sendSweetAlert(session,
                         title="Erreur",
                         type="error",
                         text=paste0("L'enregistrement à échoué car le fichier ",gsub("\\\\","/",filename)," existe déjà."))
          return(NULL)
        }else{
          carto=cartographySAFRAN()
          breaksLabel=names(paletteBreaksSAFRAN())
          breaksValues=as.numeric(paletteBreaksSAFRAN())
          legend=legendSAFRAN()+
            geom_text(aes(x=breaksValues,y=0.3,label=breaksLabel),hjust=1,vjust=1,angle=45,size=3)
          
          jpeg(filename,7,7,units="in",res=600)
          invisible({plot=grid.arrange(carto,legend,ncol=1,heights=c(7.5,2.5))})
          dev.off()
          return(NULL)
        }
      }
    }else{
      return(NULL)
    }
  })
  
  
  
  # Rendering the datatable
  output$tableSAFRAN=renderDataTable({
    if(!is.null(tableSAFRAN())){
      table=tableSAFRAN()
      round=round_any(table[,2],10^(ceiling(log10(mean(abs(table[,2]),na.rm=TRUE)))-3))
      
      if(mean(nchar(round),na.rm=TRUE)>5){
        table[,2]=formatC(table[,2],format="e",3)
      }else{
        table[,2]=round
      }
      
      if(ncol(table)>2){table[,"VarCoef [%]"]=round(table[,"VarCoef [%]"],2)} # always true exept when chosen spatial aggregation is the raw SAFRAN grid
      
      return(table)
    }else{
      return(NULL)
    }
  },options=list(pageLength=10,lengthMenu=c(5,10,15,20),dom="lftp",columnDefs=list(list(className='dt-center',width='60px',targets="_all"))),rownames=FALSE)
  
  
  
  # Saving the datatable in CSV
  output$buttonSaveTableSAFRAN=renderUI({ # button
    if(!is.null(tableSAFRAN())){
      return(actionBttn("saveTableSAFRAN",label=".csv",style="simple",color="success",icon=icon("save")))
    }else{
      return(NULL)
    }
  })
  
  observeEvent(input$saveTableSAFRAN,{ # button listener
    if(!is.null(tableSAFRAN())){
      savePath=choose.dir()
      
      if(is.na(savePath)){
        return(NULL)
      }else{
        variable=substr(variableToPlotSAFRAN(),1,(as.numeric(gregexpr("\\[",variableToPlotSAFRAN()))-2))
        filename=paste0(savePath,"/",variable,"_du_",timeRangeSAFRAN()[1],"_au_",timeRangeSAFRAN()[2],"_(",methodToApplySAFRAN(),")_",scaleToPlotSAFRAN(),".csv")
        
        if(file.exists(filename)){
          sendSweetAlert(session,
                         title="Erreur",
                         type="error",
                         text=paste0("L'enregistrement à échoué car le fichier ",gsub("\\\\","/",filename)," existe déjà."))
          return(NULL)
        }else{
          write.table(tableSAFRAN(),filename,sep=";",row.names=FALSE)
          return(NULL)
        }
      }
    }else{
      return(NULL)
    }
  })
  
  
  
  # Shutdown the application when user quit the web browser page
  session$onSessionEnded(function(){
    stopApp()
  })
}






# RUN #
runApp(list(ui=ui,server=server),launch.browser=TRUE)