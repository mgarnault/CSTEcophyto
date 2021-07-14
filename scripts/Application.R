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


# Working directory
path=dirname(rstudioapi::getSourceEditorContext()$path)
path=substr(path,1,(as.numeric(gregexpr("/scripts",dirname(rstudioapi::getSourceEditorContext()$path)))-1))
setwd(path)


# Global variables
SAFRANvarList=list("Concentration de CO2 [ppm]"="co2",
                   "Contenu en eau gelée dans la couche de racinaire à 06 UTC [m³/m³]"="wgi_racine_q",
                   "Contenu en eau liquide dans la couche racinaire à 06 UTC [m³/m³]"="wg_racine_q",
                   "Drainage (cumul quotidien 06-06 UTC) [mm]"="drainc_q",
                   "Ecoulement à la base du manteau neigeux [mm]"="ecoulement_q",
                   "Epaisseur du manteau neigeux (moyenne quotidienne 06-06 UTC) [m]"="hteurneige_q",
                   "Epaisseur du manteau neigeux maximum au cours de la journée [m]"="hteurneigex_q",
                   "Epaisseur du manteau neigeux à 06 UTC [m]"="hteurneige6_q",
                   "Equivalent en eau du manteau neigeux (moyenne quotidienne 06-06 UTC) [mm]"="resr_neige_q",
                   "Equivalent en eau du manteau neigeux à 06 UTC [mm]"="resr_neige6_q",
                   "Evapotranspiration potentielle (formule de Penman-Monteith) [mm]"="etp_q",
                   "Evapotranspiration potentielle calculée par SICLIMA (formule de Penman-Monteith) [mm]"="etppm",
                   "Evapotranspiration réelle (cumul quotidien 06-06 UTC) [mm]"="evap_q",
                   "Fraction recouverte par la neige (moyenne quotidienne 06-06 UTC) [%]"="snow_frac_q",
                   "Humidité relative (moyenne quotidienne) [%]"="hu_q",
                   "Humidité spécifique (moyenne quotidienne) [g/kg]"="q_q",
                   "Indice d’humidité des sols (moyenne quotidienne 06-06 UTC) [%]"="swi_q",
                   "Pluies efficaces (cumul quotidien) [mm]"="pe_q",
                   "Précipitations liquides (cumul quotidien 06-06 UTC) [mm]"="preliq_q",
                   "Précipitations solides (cumul quotidien 06-06 UTC) [mm]"="prenei_q",
                   "Rayonnement atmosphérique (cumul quotidien) [J/cm²]"="dli_q",
                   "Rayonnement visible (cumul quotidien) [J/cm²]"="ssi_q",
                   "Ruissellement (cumul quotidien 06-06 UTC) [mm]"="runc_q",
                   "Température (moyenne quotidienne) [°C]"="t_q",
                   "Température maximale des 24 températures horaires [°C]"="tsup_h_q",
                   "Température minimale des 24 températures horaires [°C]"="tinf_h_q",
                   "Vent à 10m (moyenne quotidienne) [m/s]"="ff_q")






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
                                 fluidRow(column(12,pickerInput("scaleToPlotSAFRAN","Échelle d'agrégation spatiale",
                                                                choices=c("Mailles SAFRAN","Départements","Régions","France entière"),
                                                                multiple=FALSE))),
                                 fluidRow(column(12,prettySwitch(inputId="showMeshCharac",label="Afficher les contraintes sur les mailles",status="primary",slim=TRUE))),
                                 conditionalPanel("input.showMeshCharac==1",
                                   fluidRow(column(1),
                                            column(11,sliderInput("altitudeMeshSAFRAN","Altitude",
                                                                  0,3000,c(0,3000),ticks=FALSE)))),
                                 fluidRow(column(12,align="center",actionBttn("go","Go",icon=icon("sync-alt"))))),
                hr(),
                menuItem("Vente Phytos",tabName="BNVD",icon=icon("spray-can")))),
  dashboardBody(
    tabItems(tabItem(tabName="SAFRAN",h2("Cartographie des données SAFRAN"),
                     plotOutput("cartographySAFRAN")),
             tabItem(tabName="BNVD",h2("Graphique d'utilisation des PPP")
      )
    )
  )
)




# SERVER #
server=function(input,output,session){
  
  observeEvent(input$go,{
    timeRangeLimits=input$timeRangeSAFRAN # stores time range asked by the user
    
    if(timeRangeLimits[2]<timeRangeLimits[1]){ # test if the first date of the time range asked by the user is actually before the second one
      sendSweetAlert(session, # display an error message if not
                     title="Erreur",
                     type="error",
                     text=paste0("La première date renseignée (",timeRangeLimits[1],") 
                                 se situe après la seconde (",timeRangeLimits[2],"). 
                                 Veuillez renseigner une plage temporelle où la première 
                                 date précède la seconde."))
      cartographySAFRAN(NULL)
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
      
      cartographySAFRAN(NULL)
      return(NULL)
    }
    
    timeRangeDays=seq(timeRangeLimits[1],timeRangeLimits[2],by="days") # determines sequence of days from the begin to the end of the time range asked by the user
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
                     temporelle demandée (du ",timeRangeLimits[1]," au ",timeRangeLimits[2],
                     "). Les années disponibles dans la base données SAFRAN sont : ",
                     timeRangesData))
      
      cartographySAFRAN(NULL)
      return(NULL)
    }
    
    show_modal_spinner(spin="orbit",text="Calcul en cours",color="#08298A")
    if(length(timeRangeYears)==1){ # the time range asked by the user span over only one year
      variable=SAFRANvarList[[which(names(SAFRANvarList)==input$variableToPlotSAFRAN)]]
      
      weather=read.table(paste0(path,"/data/meteo_safran/output/",timeRangeYears,"/Total_",timeRangeYears,".csv"),
                         sep=";",header=TRUE,stringsAsFactors=FALSE)
      weather=weather[,which(colnames(weather)%in%c("Site","Year","Month","DOM",variable))]
      weather$Date=as.Date(paste(weather$Year,weather$Month,weather$DOM,sep="-"))
      weather=weather[,which(colnames(weather)%in%c("Site","Date",variable))]
      weather=weather[which(weather$Date%in%timeRangeDays),]
      weather=weather[,which(colnames(weather)%in%c("Site",variable))]
      weather=aggregate(.~Site,weather,"mean")
      
      grid=st_read(paste0(path,"/data/mailles_safran/output/SAFRANgrid_handmade"),options="ENCODING=UTF-8",quiet=TRUE)

      grid$variable=apply(grid,1,function(row){
        if(as.numeric(row["altitud"])>=input$altitudeMeshSAFRAN[1] &
           as.numeric(row["altitud"])<=input$altitudeMeshSAFRAN[2]){
          return(weather[which(weather$Site==as.numeric(row["mll_sfr"])),variable])
        }else{
          return(NA)
        }
      })
      grid=grid[,which(colnames(grid)%in%c("geometry","variable"))]
      
      p=ggplot()+
        geom_sf(aes(fill=variable),col="black",data=grid)+
        scale_fill_gradient2(low="#4444fe",mid="#f8fe44",high="#fe4444",midpoint=mean(grid$variable))
      cartographySAFRAN(p)

    }else if(length(timeRangeYears)==2){ # time range asked by the user span over only two years
      sendSweetAlert(session,
                     title="En construction",
                     type="warning",
                     text="La plage temporelle sélectionnée recouvre plus d'une année.")
      
      cartographySAFRAN(NULL)
      
    }else{ # time range asked by the user span over more than two years
      sendSweetAlert(session,
                     title="En construction",
                     type="warning",
                     text="La plage temporelle sélectionnée recouvre plus de deux années")
      
      cartographySAFRAN(NULL)
    }
    
    remove_modal_spinner()
    return(NULL)
  })
  
  cartographySAFRAN=reactiveVal(NULL)
  output$cartographySAFRAN=renderPlot({
    return(cartographySAFRAN())
  })
  
  
  # Shutdown the application when user quit the web browser page
  session$onSessionEnded(function(){
    stopApp()
  })
}





# RUN #
runApp(list(ui=ui,server=server),launch.browser=TRUE)