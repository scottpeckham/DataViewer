# Shiny app to view GPS data basic animal data tables, --capture tables from a DB snapshot
#   rev 11/1/24 to update to using path to db instead of snapshots
#     Started V3 2/25/ to make it behave more interactively adding ability to select animals, added last N locations

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
library(shinyFiles)
library(shinyWidgets)

source("helpers.R")

# Hard-coded name of GPS table (maybe make selectable later)
gps.tab.name <- "AnimalID_GPS"

#   
ui <- dashboardPage(
  dashboardHeader(title = "Tri-State Bighorn Test and Remove Data Viewer",titleWidth = 450),
  dashboardSidebar(img(src = "IMG_0368.JPG", height = 150, width = 200),
                   #strong("Idaho, Oregon, Washington"),
                   p("Capture, GPS, and health testing display BETA"),
                   shinyFilesButton("Btn_GetFile", "Choose SQLite database file" ,
                                    title = "Please select a file:", multiple = FALSE,
                                    buttonType = "default", class = NULL),
                   selectInput("selectHerd", label = "Bighorn Herd:", choices = "",selected = ""),
                   selectInput("zcol", "Classify and Display GPS points by:", 
                               choices = list("AnimalID" = "AnimalID",
                                              "Sex" = "Sex","Capture PCR Status"="CapturePCRStatus","Capture ELISA Status"="CaptureELISAStatus", "Capture Movi ELISA" = "Capture_cELISA"), selected = "AnimalID"),
                   radioButtons("radio", "Select locations by:",
                                choices = c("Date Range" = "drange",
                                            "Last N Locations" = "nlocations"),
                                selected = "drange"),
                   uiOutput("subsetSelect"),
                   uiOutput("animalSelect"),
                   actionButton("drawMap", "Load Data")
                   
                   ),
  dashboardBody(
    
    tabsetPanel(type = "tabs",
                tabPanel("Map", leafletOutput("map",width="95%",height=800)),
                tabPanel( "GPS Data Table",
                          DT::dataTableOutput("datatable"),
                         downloadButton("downloadTableData", "Download")),
                tabPanel("Study Sheep",
                         DT::dataTableOutput("table"),
                         downloadButton("downloadSheepData", "Download")),
                tabPanel("Testing Results",
                         DT::dataTableOutput("testtable"),
                         downloadButton("downloadTestData", "Download")),
                tabPanel("Viewer Info", br(),
                         hr(),
                         h4(strong("Tool Description")),
                         p(style="text-align: justify; font-size = 30px",
                           "This application displays GPS data and selected tables and summaries from our TriState database. 
    
    The map will draw for one herd at a time, and is responsive to changes in display category of the GPS points. 
    To switch herds or get a new date range or last N locations, simply click the 'Load Data' button after making your selection. You can turn on/off various layers using the tools on the left side of the map screen.
    The first point in a trajectory will be circled in green, the last point in red. You can click on points to obtain attributes about that location and animal.
    "),
                        br(),
                        p(style="text-align: justify; font-size = 25px",
                          "On the study sheep tab, it will display the table entries for the sheep on the map (that are valid for your date or location selection). These are searchable and you can apply filters to find specific values. Tables can be downloaded using the download button at the bottom.
                          The Testing Results tab will show a table of all the testing results for the AnimalIDs that were returned by the date range in your input criteria.
                          On the summary tab (deprecated now, but can be returned), you can view testing summaries by herd and download the summary table using the download button."))
                )
    
    )
)

server <- function(input, output, session) {
  
  # handle file path button
  volumes = getVolumes()
  
  fileval <- reactive({
    l <- parseFilePaths(volumes, input$Btn_GetFile)
    as.character(l$datapath)
  })
  
  observe({  
    shinyFileChoose(input, "Btn_GetFile", roots = volumes, session = session)
    
    if(!is.null(input$Btn_GetFile)){
      # browser()
      
      output$db_file <- renderText(fileval())
      
      
    }
  })
  
  # when we choose the DB file, populate some tables for menu items
  observeEvent(fileval(), {
    nfile <- fileval()
    if (length(nfile)>0) {
      
      if (file.exists(nfile)) {
        
        # Connect to the data base read GPS table we need #
        con <- dbConnect(RSQLite::SQLite(),fileval(), extended_types=TRUE)
        
        # query for gps, don't read into memory
        gps_db <- tbl(con, gps.tab.name) # reference to the table
        
        herds <- gps_db %>% select(Herd) %>% collect()
        herds <- unique(herds)
        
        # close out DB connection
        dbDisconnect(con)
        
        # upadte the list input(s)  
        updateSelectInput(inputId = "selectHerd", choices = herds)
      }
    }
  })
  
  inputHerd <- reactive({
    input$selectHerd
    
  })
  
  # first deal with the reactive UI issue (i.e. we don't know number of years or dates)
  output$subsetSelect <- renderUI({
    
    # if we've chosen our DB file and herd then do this
    nfile <- fileval()
    herd <- inputHerd()
    if (length(nfile)>0 & herd != "") {
      
      # query DB for data #
      
      # Connect to the data base read GPS table we need #
      con <- dbConnect(RSQLite::SQLite(),fileval(), extended_types=TRUE)
      
      dbpath <- "/Users/scottp/DocumentsNew/BighornSheep/FY22-WSF-GIA/Databases/BHS_TriState.db"
      dbpath <- nfile
      
      con <- dbConnect(RSQLite::SQLite(),dbpath, extended_types=TRUE)
      
        # query for gps, don't read into memory
        gps_db <- tbl(con, gps.tab.name) # reference to the table
        
        herd <- input$selectHerd
        
        avail.dates <- gps_db %>% filter(Herd==herd) %>% summarise(MinDate=min(acquisitiontime),MaxDate=max(acquisitiontime)) %>% collect()
        avail.dates$MinDate <- strftime(avail.dates$MinDate, format="%Y-%m-%d", tz="UTC")
        avail.dates$MaxDate <- strftime(avail.dates$MaxDate, format="%Y-%m-%d", tz="UTC")
      
      # close out DB connection
      dbDisconnect(con)
      
      # some default dates for the input range, use current two weeks, starting back 2 days for typical lag in DB updates
      endd <- strftime(Sys.time()-60*60*24*2,format="%Y-%m-%d")
      startd <- strftime(Sys.time()-60*60*24*16,format="%Y-%m-%d")
      
      # condition on the radio button to draw UI elements
      switch(input$radio,
        "drange" = dateRangeInput("dates",label="Date range for display:",
                     start  = startd,
                     end    = endd,
                     min=avail.dates$MinDate[1],
                     max=avail.dates$MaxDate[1]),
        "nlocations" =  numericInput("lastNloc", "Number of locations:",
                                  value = 10, min=0))
      
      
    }
    
  })

  # first deal with the reactive UI issue (i.e. we don't know number of years or dates)
  output$animalSelect <- renderUI({
    
    # if we've chosen our DB file and herd then do this
    nfile <- fileval()
    herd <- inputHerd()
    if (length(nfile)>0 & herd != "") {
      
      # query DB for data #
      
      # Connect to the data base read GPS table we need #
      con <- dbConnect(RSQLite::SQLite(),fileval(), extended_types=TRUE)
      
      #dbpath <- "/Users/scottp/DocumentsNew/BighornSheep/FY22-WSF-GIA/Databases/BHS_TriState.db"
      dbpath <- nfile
      
      con <- dbConnect(RSQLite::SQLite(),dbpath, extended_types=TRUE)
      
      # query for gps, don't read into memory
      gps_db <- tbl(con, gps.tab.name) # reference to the table
      
      herd <- input$selectHerd
      
      avail.ID <- gps_db %>% filter(Herd==herd) %>% select(AnimalID) %>% collect() %>% unique()
      avail.ID <- avail.ID[,1]
      
      # close out DB connection
      dbDisconnect(con)
      
      # add checkbox for AnimalIDs
      #checkboxGroupInput("aID", "AnimalID", avail.ID$AnimalID, selected = avail.ID$AnimalID, inline = FALSE,
       #                  width = NULL)
      # selectInput(
      #   "aID", "AnimalID", avail.ID$AnimalID, selected = avail.ID$AnimalID, multiple = TRUE,
      #   selectize = FALSE)
      pickerInput("aID","AnimalID", choices=avail.ID$AnimalID,
                  options=list(`live-search`=TRUE, actionsBox=TRUE,size=10),multiple = T)
    }
    
  })
  
  getGPSData <- eventReactive(input$drawMap, {
    # query DB for data #
    gps.tab.name <- "AnimalID_GPS"
    
    
    t1 <- as.POSIXct(input$dates[1],tz="UTC")
    t2 <- as.POSIXct(input$dates[2],tz="UTC")
    
    # Connect to the data base read GPS table we need #
    con <- dbConnect(RSQLite::SQLite(),fileval(), extended_types=TRUE)
    
    # query for gps, don't read into memory
    gps_db <- tbl(con, gps.tab.name) # reference to the table
    
    # query and store in data frame (note condition on gender input), conditioning on radio button choice
     gps <- switch(input$radio,
             "drange" =  gps_db %>% filter(Herd==input$selectHerd) %>% 
                        filter(acquisitiontime>= t1 & acquisitiontime <= t2) %>% filter(AnimalID %in% input$aID) %>% collect(),
              "nlocations" = FetchLastNFixes(gps_db,input$lastNloc,ids=input$aID)
     )
    
    # close out DB connection
    dbDisconnect(con)
    
    # remove missing values, duplicatses 
    gps <- gps %>% removeMissingGPS() %>% distinct()
    
    
    # crs= 4326 parameter assigns a WGS84 coordinate system 
    st_as_sf(gps, coords = c("Longitude", "Latitude"), crs = 4326)
    
  })
  
  getSheepData <- eventReactive(input$drawMap, {
    
    plotdata <- getGPSData()
    animals <- unique(plotdata$AnimalID)
    
    tab.name <- "TriState_StudySheep"
    
    # Connect to the data base read GPS table we need #
    con <- dbConnect(RSQLite::SQLite(),fileval(), extended_types=TRUE)
    
    # query for gps, don't read into memory
    tab_db <- tbl(con, tab.name) # reference to the table
    
    out.dat <- tab_db %>% filter(Herd==input$selectHerd) %>% filter(AnimalID %in% animals) %>% collect() 
    
    # close out DB connection
    dbDisconnect(con)
    
    return(out.dat)
    
  })
  
  getTestData <- eventReactive(input$drawMap, {
    
    plotdata <- getGPSData()
    animals <- unique(plotdata$AnimalID)
    
    pcr.name <- "TriState_Bacteriology"
    ser.name <- "TriState_Serology"
    
    # Connect to the data base read GPS table we need #
    con <- dbConnect(RSQLite::SQLite(),fileval(), extended_types=TRUE)
    
    # query for gps, don't read into memory
    pcr_db <- tbl(con, pcr.name) # reference to the table
    ser_db <- tbl(con, ser.name)
    
    pcr.dat <- pcr_db %>% filter(AnimalID %in% animals) %>% collect() %>% dplyr::select(AnimalID,Sample_ID,SampleDate,WADDLNo,MoviPCR)
    ser.dat <- ser_db %>% filter(AnimalID %in% animals) %>% collect() %>% dplyr::select(AnimalID,Sample_ID,SampleDate,WADDLNo,Movi_Elisa,Movi_Status)
    
    # close out DB connection
    dbDisconnect(con)
    
    # join them together by ID and date, but don't exclude ones that don't match
    out.dat <- full_join(pcr.dat,ser.dat,by=join_by(AnimalID,SampleDate),relationship="many-to-many",suffix=c(".PCR",".Serology"))
    return(out.dat)
    
  })
  
  getGPSDataOutput <- eventReactive(input$drawMap, {
    # query DB for data #
    gps.tab.name <- "AnimalID_GPS"
    t1 <- as.POSIXct(input$dates[1],tz="UTC")
    t2 <- as.POSIXct(input$dates[2],tz="UTC")
    
    # Connect to the data base read GPS table we need #
    con <- dbConnect(RSQLite::SQLite(),fileval(), extended_types=TRUE)
    
    # query for gps, don't read into memory
    gps_db <- tbl(con, gps.tab.name) # reference to the table
    
    # query and store in data frame (note condition on gender input)
    gps <- gps_db %>% filter(Herd==input$selectHerd) %>% 
      filter(acquisitiontime>= t1 & acquisitiontime <= t2) %>% collect() 
    
    # close out DB connection
    dbDisconnect(con)
    
    return(gps)
    
  })
  
  loadnew <- observeEvent(input$drawMap,{
  
  output$map<-renderLeaflet({
    
    zcol <- input$zcol
    plotdata <- getGPSData()
    
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
    DT::datatable(getSheepData(),options = list(
      pageLength=10, scrollX='400px'), filter = 'top')
  })
  
  output$datatable <- DT::renderDataTable({
    DT::datatable(getGPSData(),options = list(
      pageLength=10, scrollX='400px'), filter = 'top')
  })
  
  output$testtable <- DT::renderDataTable({
    DT::datatable(getTestData(),options = list(
      pageLength=10, scrollX='400px'), filter = 'top')
  })
  
  # Downloadable csv of selected summary data ----
  output$downloadSheepData <- downloadHandler(
    filename = function() {
      paste(inputHerd(), "StudySheep.csv", sep = "")
    },
    content = function(file) {
      write.csv(getSheepData(), file, row.names = FALSE)
    }
  )
  
  output$downloadTestData <- downloadHandler(
    filename = function() {
      paste(inputHerd(), "TestData.csv", sep = "")
    },
    content = function(file) {
      write.csv(getTestData(), file, row.names = FALSE)
    }
  )

  # Downloadable csv of selected gps dataset ----
  output$downloadTableData <- downloadHandler(
    filename = function() {
      paste(inputHerd(), "Table.csv", sep = "")
    },
    content = function(file) {
      write.csv(getGPSDataOutput(), file, row.names = FALSE)
    }
  )


})
}

shinyApp(ui, server,options = list(launch.browser = TRUE))
