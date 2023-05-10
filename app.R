library(shiny)
library(shinydashboard)
library(mapview)
library(sf)
library(sp)
library(RColorBrewer)
library(leaflet)
library(dplyr)
library(DT)
library(colorRamps)

source("helpers.R")

# load pre made data (saves reading from DB) #
 # contains gps, capture, serology, PCR, study sheep and summary table data frames
 #  the capture table here is reduced in fields and has been joined to test data for a comp view
 #  test tables are complete with added attributes for filtering
load("data/appdata.rda")

    

    # Convert the dataframe to a spatial object. Note that the
    # crs= 4326 parameter assigns a WGS84 coordinate system to the 
    # spatial object
    gps.sf <- st_as_sf(gps, coords = c("Longitude", "Latitude"), crs = 4326)
    
    # Polygon home ranges (not implemented )#
    #hr <- st_read("/data/BurntRiver.gpkg",
    #              layer = "Homeranges")
      
  
ui <- dashboardPage(
  dashboardHeader(title = "Tri-State Bighorn Test and Remove Data Viewer",titleWidth = 450),
  dashboardSidebar(img(src = "IMG_0368.JPG", height = 150, width = 200),
                   #strong("Idaho, Oregon, Washington"),
                   p("Capture, GPS, and health testing display BETA"),
                   br(),
                   dateRangeInput("dates",label="Date Range for GPS location display:", 
                                  start  = "2023-02-01",
                                  end    = "2023-03-01",
                                  min="2021-01-01",max="2025-12-31"),
                   br(),
                   selectInput("zcol", "Classify and Display GPS points by:", 
                               choices = list("AnimalID" = "AnimalID", "Herd" = "Herd",
                                              "Sex" = "Sex","Capture PCR Status"="CapturePCRStatus","Capture ELISA Status"="CaptureELISAStatus", "Capture Movi ELISA" = "Capture_cELISA"), selected = "Herd"),
                   br(),
                   checkboxGroupInput("checkHerd", label = "Bighorn Herd(s):", 
                                      choices = list("Burnt River" = "Burnt River", "Lookout Mountain" = "Lookout Mountain", "Yakima Canyon" = "Yakima Canyon", "Cleman Mountain" = "Cleman Mountain",
                                                     "Lower Panther" = "Lower Panther Main Salmon", "Lower Salmon" = "Lower Salmon"),
                                      selected = "Burnt River"),
                   br(),
                   selectInput("datachoice", label = "Choose data table to display:", 
                                      choices = list("Capture" = "cap", "Study Sheep" = "studysheep", "PCR" = "pcr", "Serology" = "ser"),
                                      selected = "Capture")
                   ),
  dashboardBody(
    
    tabsetPanel(type = "tabs",
                tabPanel("Map", mapviewOutput("map",width="95%",height=800)),
                tabPanel("Summary", DT::dataTableOutput("table"),
                         downloadButton("downloadSummaryData", "Download")),
                tabPanel("Data Table", DT::dataTableOutput("datatable"),
                         downloadButton("downloadTableData", "Download"))
                )
    
    )
)

server <- function(input, output) {
  
  # Reactive value for selected dataset ----
  summaryInput <- reactive({
    summary.table %>% filter(Herd %in% input$checkHerd)
  })
  
  inputHerd <- reactive({
    input$checkHerd
  })
  
  # Reactive value for data table choice
  datatableInput <- reactive({
    switch(input$datachoice, 
                   "cap" = cap,
                   "pcr" = pcr,
                   "ser" = ser,
                   "studysheep" = studysheep) %>% filter(Herd %in% inputHerd())
  })
  
  output$map<-renderLeaflet({
    mindate <- as.POSIXct(input$dates[1],tz="UTC")
    maxdate <- as.POSIXct(input$dates[2],tz="UTC")
    zcol <- input$zcol
    plotdata <- subset(gps.sf, acquisitiontime >= mindate & acquisitiontime <= maxdate) %>% filter(Herd %in% inputHerd())
    n.animals <- length(unique(plotdata$AnimalID))
    n.herd <- length(unique(plotdata$Herd))
    n.pcr <- length(unique(plotdata$CapturePCRStatus))  # only 3 possible choices here
    n.ser <- length(unique(plotdata$CaptureELISAStatus))
    n.cel <- length(unique(plotdata$Capture_cELISA))
    
    qual_col_pals = brewer.pal.info[brewer.pal.info$category == 'qual',]
    col_vector = unlist(mapply(brewer.pal, qual_col_pals$maxcolors, rownames(qual_col_pals)))
    id.colors <- sample(col_vector, n.animals)
    
    sex.col <- c("pink","blue")
    celisa.col <- colorRamps::matlab.like(7)
                    
    mycolors <- switch(input$zcol, 
                   "AnimalID" = id.colors,
                   "Sex" = sex.col,
                   "Herd" = colorRampPalette(brewer.pal(8, "Set1"))(n.herd),
                   "CapturePCRStatus" = colorRampPalette(brewer.pal(8, "RdYlGn"))(n.pcr),
                   "CaptureELISAStatus" = colorRampPalette(brewer.pal(8, "RdYlGn"))(n.ser),
                   "Capture_cELISA" = celisa.col
                   )
    
   #  
   makeMap(plotdata, zcol=zcol,colors=mycolors,alpha=0.90)
   
  })
  
  output$table <- DT::renderDataTable({
    DT::datatable(summaryInput())
  })
  
  output$datatable <- DT::renderDataTable({
    DT::datatable(datatableInput(),options = list(
      pageLength=10, scrollX='400px'), filter = 'top')
  })
  
  # Downloadable csv of selected summary data ----
  output$downloadSummaryData <- downloadHandler(
    filename = function() {
      paste(inputHerd(), "Summary.csv", sep = "")
    },
    content = function(file) {
      write.csv(summaryInput(), file, row.names = FALSE)
    }
  )
  
  # Downloadable csv of selected table dataset ----
  output$downloadTableData <- downloadHandler(
    filename = function() {
      paste(inputHerd(),input$datachoice, "Table.csv", sep = "")
    },
    content = function(file) {
      write.csv(datatableInput(), file, row.names = FALSE)
    }
  )
}

shinyApp(ui, server)
