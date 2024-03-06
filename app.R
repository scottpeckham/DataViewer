# Shiny app to view GPS data and testing, capture tables from a DB snapshot

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
                tabPanel("Map", leafletOutput("map",width="95%",height=800)),
                tabPanel( "Data Table", radioButtons("tableHerd", "Bighorn herd:",
                                                     c("Burnt River" = "Burnt River", "Lookout Mountain" = "Lookout Mountain", "Lostine" = "Lostine", "Yakima Canyon" = "Yakima Canyon", "Cleman Mountain" = "Cleman Mountain",
                                                       "Lower Panther" = "Lower Panther Main Salmon", "Lower Salmon" = "Lower Salmon"),
                                                     inline=TRUE),
                          selectInput("tablechoice", label = "Choose data table to display:", 
                                                    choices = list("Capture" = "cap", "Study Sheep" = "studysheep", "PCR" = "pcr", "Serology" = "ser"),
                                                    selected = "Capture"),
                          DT::dataTableOutput("datatable"),
                         downloadButton("downloadTableData", "Download")),
                tabPanel("Summary", checkboxGroupInput("summaryHerd", label = "Bighorn Herd(s):", 
                                           choices = list("Burnt River" = "Burnt River", "Lookout Mountain" = "Lookout Mountain", "Lostine" = "Lostine", "Yakima Canyon" = "Yakima Canyon", "Cleman Mountain" = "Cleman Mountain",
                                                          "Lower Panther" = "Lower Panther Main Salmon", "Lower Salmon" = "Lower Salmon"),
                                           selected = "Burnt River", inline=TRUE),
                         DT::dataTableOutput("table"),
                         downloadButton("downloadSummaryData", "Download")),
                tabPanel("Viewer Info", br(),
                         hr(),
                         h4(strong("Tool Description")),
                         p(style="text-align: justify; font-size = 30px",
                           "This application displays GPS data and health testing tables and summaries from our TriState database. 
    
    The map will draw for one herd at a time, and is responsive to changes in display category of the GPS points. To switch herds or get a new date range, simply click the Load Map button after making your selection. Note that this app doesn't show the full GPS set. 
    It's a subsample of 25-40% depending on the number of locations per herd. The full set causes the application to run slower.
    "),
                        br(),
                        p(style="text-align: justify; font-size = 25px",
                          "On the table tab, you can choose the herd and data table you wish to view. These are searchable and you can apply filters to find specific values. Tables can be downloaded using the download button at the bottom.
                          On the summary tab, you can view testing summaries by herd and download the summary table using the download button."))
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
    
    # we want same color pattern for test results (having no "Detected" can swap colors between plots)
    # colors for test results #
    
    # handle missing or NA in testing fields so they don't screw up the color mapping
    plotdata$CaptureELISAStatus[which(plotdata$CaptureELISAStatus==""| is.na(plotdata$CaptureELISAStatus),arr.ind=TRUE)] <- "No Record"
    plotdata$CapturePCRStatus[which(plotdata$CapturePCRStatus==""| is.na(plotdata$CapturePCRStatus),arr.ind=TRUE)] <- "No Record"
    
    tcol <- c(brewer.pal(3,'RdYlGn'),"#CCCCCC")
    tcol.shuf <- c(tcol[1:2],tcol[4],tcol[3])
    tcol <- tcol.shuf
    pcr.col.map <- case_when(
      plotdata$CapturePCRStatus == "Detected" ~ 1,
      plotdata$CapturePCRStatus == "Indeterminate" ~ 2,
      plotdata$CapturePCRStatus == "Not detected" ~ 3,
      plotdata$CapturePCRStatus == "No Record" ~ 4
    )
    pcr.colors <- tcol[sort(unique(pcr.col.map))]
    #pcr.colors <- tcol[pcr.col.map]
    
    ser.col.map <- case_when(
      plotdata$CaptureELISAStatus == "Detected" ~ 1,
      plotdata$CaptureELISAStatus == "Indeterminate" ~ 2,
      plotdata$CaptureELISAStatus == "Not detected" ~ 4,
      plotdata$CaptureELISAStatus == "No Record" ~ 3
    )
    ser.colors <- tcol[sort(unique(ser.col.map))]
    
    
    sex.col <- c("pink","blue")
    celisa.col <- colorRamps::matlab.like(7)
    
    mycolors <- switch(input$zcol, 
                   "AnimalID" = id.colors,
                   "Sex" = sex.col,
                   #"Herd" = colorRampPalette(brewer.pal(8, "Set1"))(n.herd),
                   "CapturePCRStatus" = pcr.colors,
                   "CaptureELISAStatus" = ser.colors,
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
