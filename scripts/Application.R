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


# Working directory
path=dirname(rstudioapi::getSourceEditorContext()$path)
path=substr(path,1,(as.numeric(gregexpr("/scripts",dirname(rstudioapi::getSourceEditorContext()$path)))-1))
setwd(path)






# USER INTERFACE #
ui=dashboardPage(
  dashboardHeader(title="CST Ecophyto II+ Tool"),
  dashboardSidebar(width=350,
    sidebarMenu(id="sidebarmenu",
                menuItem("Météo SAFRAN",tabName="SAFRAN",icon=icon("temperature-high")),
                conditionalPanel("input.sidebarmenu=='SAFRAN'",
                                 fluidRow(column(12,dateRangeInput("timeRangeSAFRAN","Plage temporelle"))),
                                 fluidRow(column(12,align="center",actionBttn("go","Go",icon=icon("sync-alt"))))),
                hr(),
                menuItem("Vente Phytos",tabName="BNVD",icon=icon("spray-can")))),
  dashboardBody(
    tabItems(tabItem(tabName="SAFRAN",h2("Cartographie des données SAFRAN"),
                     uiOutput("timeRangeSAFRANtext")),
             tabItem(tabName="BNVD",h2("Graphique d'utilisation des PPP")
      )
    )
  )
)




# SERVER #
server=function(input,output,session){
  
  timeRangeSAFRAN=reactiveVal(NULL)
  observeEvent(input$go,{
    timeRangeSAFRAN(input$timeRangeSAFRAN)
  })
  
  output$timeRangeSAFRANtext=renderText({
    return(paste(timeRangeSAFRAN()[1],timeRangeSAFRAN()[2]))
  })
  
  
  # Shutdown the application when user quit the web browser page
  session$onSessionEnded(function(){
    stopApp()
  })
}





# RUN #
runApp(list(ui=ui,server=server),launch.browser=TRUE)