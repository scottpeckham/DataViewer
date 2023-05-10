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
load("data/gpsdata.rda")

    

    # Convert the dataframe to a spatial object. Note that the
    # crs= 4326 parameter assigns a WGS84 coordinate system to the 
    # spatial object
    gps.sf <- st_as_sf(gps, coords = c("Longitude", "Latitude"), crs = 4326)
    
    # Polygon home ranges (not implemented )#
    #hr <- st_read("/data/BurntRiver.gpkg",
    #              layer = "Homeranges")
    
    # determine some bookends from the data #
    #   these will be needed to modify the button options
    herds <- unique(gps$Herd)
    avail.dates <- gps %>% group_by(Herd) %>% summarise(MinDate=min(acquisitiontime),MaxDate=max(acquisitiontime))
    avail.dates$MinDate <- strftime(avail.dates$MinDate, format="%Y-%m-%d", tz="UTC")
    avail.dates$MaxDate <- strftime(avail.dates$MaxDate, format="%Y-%m-%d", tz="UTC")
    
  
ui <- dashboardPage(
  dashboardHeader(title = "Tri-State Bighorn Test and Remove Data Viewer",titleWidth = 450),
  dashboardSidebar(img(src = "IMG_0368.JPG", height = 150, width = 200),
                   #strong("Idaho, Oregon, Washington"),
                   p("Capture, GPS, and health testing display BETA"),
                   selectInput("selectHerd", label = "Bighorn Herd:", choices = herds,selected = ""),
                   selectInput("zcol", "Classify and Display GPS points by:", 
                               choices = list("AnimalID" = "AnimalID",
                                              "Sex" = "Sex","Capture PCR Status"="CapturePCRStatus","Capture ELISA Status"="CaptureELISAStatus", "Capture Movi ELISA" = "Capture_cELISA"), selected = "AnimalID"),
                   
                   uiOutput("subsetSelect"),
                   actionButton("drawMap", "Load Map")
                   
                   ),
  dashboardBody(
    
    tabsetPanel(type = "tabs",
                tabPanel("Map", mapviewOutput("map",width="95%",height=800)),
                tabPanel( "Data Table", radioButtons("tableHerd", "Bighorn herd:",
                                                     c("Burnt River" = "Burnt River", "Lookout Mountain" = "Lookout Mountain", "Yakima Canyon" = "Yakima Canyon", "Cleman Mountain" = "Cleman Mountain",
                                                       "Lower Panther" = "Lower Panther Main Salmon", "Lower Salmon" = "Lower Salmon"),
                                                     inline=TRUE),
                          selectInput("tablechoice", label = "Choose data table to display:", 
                                                    choices = list("Capture" = "cap", "Study Sheep" = "studysheep", "PCR" = "pcr", "Serology" = "ser"),
                                                    selected = "Capture"),
                          DT::dataTableOutput("datatable"),
                         downloadButton("downloadTableData", "Download")),
                tabPanel("Summary", checkboxGroupInput("summaryHerd", label = "Bighorn Herd(s):", 
                                           choices = list("Burnt River" = "Burnt River", "Lookout Mountain" = "Lookout Mountain", "Yakima Canyon" = "Yakima Canyon", "Cleman Mountain" = "Cleman Mountain",
                                                          "Lower Panther" = "Lower Panther Main Salmon", "Lower Salmon" = "Lower Salmon"),
                                           selected = "Burnt River", inline=TRUE),
                         DT::dataTableOutput("table"),
                         downloadButton("downloadSummaryData", "Download"))
                )
    
    )
)

server <- function(input, output) {
  
  # first deal with the reactive UI issue (i.e. we don't know number of years or dates)
  output$subsetSelect <- renderUI({
    
    
    mind=unlist(c(avail.dates[which(inputHerd() %in% avail.dates$Herd),2]))
    maxd=unlist(c(avail.dates[which(inputHerd() %in% avail.dates$Herd),3]))
    dateRangeInput("dates",label="Date Range for home range computation:",
                   start  = "2023-01-01",
                   end    = "2023-03-31",
                   min=mind,
                   max=maxd)
    
  })
  
  # Reactive value for selected dataset ----
  summaryInput <- reactive({
    summary.table %>% filter(Herd %in% summaryHerd())
  })
  
  summaryHerd <- reactive({
    input$summaryHerd
  })
  
  inputHerd <- reactive({
    input$selectHerd
  })
  
  TableHerd <- reactive({
    input$tableHerd
  })
  
  # Reactive value for data table choice
  datatableInput <- reactive({
    switch(input$tablechoice, 
                   "cap" = cap,
                   "pcr" = pcr,
                   "ser" = ser,
                   "studysheep" = studysheep) %>% filter(Herd %in% TableHerd())
  })
  
  getData <- eventReactive(input$drawMap, {
    t1 <- as.POSIXct(input$dates[1],tz="UTC")
    t2 <- as.POSIXct(input$dates[2],tz="UTC")
    gps.sf %>% filter(Herd %in% inputHerd()) %>% filter(acquisitiontime>= t1 & acquisitiontime <= t2)
    
  })
  
  output$map<-renderLeaflet({
    
    zcol <- input$zcol
    plotdata <- getData()
    
    n.animals <- length(unique(plotdata$AnimalID))
    #n.pcr <- length(unique(plotdata$CapturePCRStatus))  # only 3 possible choices here
    #n.ser <- length(unique(plotdata$CaptureELISAStatus))
    n.cel <- length(unique(plotdata$Capture_cELISA))
    
    qual_col_pals = brewer.pal.info[brewer.pal.info$category == 'qual',]
    col_vector = unlist(mapply(brewer.pal, qual_col_pals$maxcolors, rownames(qual_col_pals)))
    
    if (n.animals < length(col_vector)) id.colors <- sample(col_vector, n.animals) else id.colors <- sample(col_vector, n.animals,
                                                                                                        replace=TRUE)
    
    sex.col <- c("pink","blue")
    celisa.col <- colorRamps::matlab.like(7)
                    
    mycolors <- switch(input$zcol, 
                   "AnimalID" = id.colors,
                   "Sex" = sex.col,
                   #"Herd" = colorRampPalette(brewer.pal(8, "Set1"))(n.herd),
                   "CapturePCRStatus" = brewer.pal(3, "RdYlGn"),
                   "CaptureELISAStatus" = brewer.pal(3, "RdYlGn"),
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
