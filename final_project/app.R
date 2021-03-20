# Shiny R for Operations
# Homework 2
# Andrew ID: zhilianz


library(shiny)
library(ggplot2)
library(dplyr)
library(DT)
library(shinyBS)
library(shinydashboard)
library(flexdashboard)
library(httr)
library(jsonlite)
library(shinyjs)
library(plotly)
library(leaflet)
library(rgeos)
library(rgdal)
library(leaflet.extras)

# Read data
url <- URLencode('https://data.wprdc.org/api/3/action/datastore_search_sql?sql=SELECT * from "b6340d98-69a0-4965-a9b4-3480cea1182b"', repeated = TRUE)

# Send request
get <- GET(url)
# Retrieve results
rawdata <- fromJSON(content(get, "text"))$result$records
# Data originally contains 500,000 rows, manually created a subset of original data
data <- rawdata %>% sample_n(500, replace = FALSE)
#Drop entire column if
data <- data[,colSums(is.na(data))<nrow(data)]

pitts <- readOGR("https://opendata.arcgis.com/datasets/9de0e9c07af04e638dbc9cb9070962c2_0.geojson")
#pitts<- readOGR("./county/Allegheny_County_Census_Block_Groups_2016.shp", layer = "Allegheny_County_Census_Block_Groups_2016")


ui <- dashboardPage(
    dashboardHeader(title = "911 Fire Dispatches"),
    dashboardSidebar(
        sidebarMenu(
            menuItem("Map", tabName = "map", icon = icon("dashboard")),
            menuItem("Chart",tabName = "chart", icon=icon("diagnoses")),
            menuItem("Data Table", tabName = "datatable", icon = icon("table"))
            
        )
    ),
    dashboardBody(
        tabItems(
            # First tab content
            tabItem(tabName = "map",
                    shinyjs::useShinyjs(),
                    tags$style(type = "text/css", ".leaflet {height: 60vh !important;}"),
                    leafletOutput("leafletmap"),
                    fluidRow(
                        column(4,
                               selectInput("city","select_city", multiple = TRUE,choices = unique(data$city_name),selected ='PITTSBURGH' )
                               
                        ),
                        column(4,
                               checkboxGroupInput("year","select_year",choices = unique(data$call_year))
                               
                        )
                    )
                    
                    
            ),
            
            
            # Second tab content 
            tabItem(tabName = "chart",
                    fluidRow(
                        column(4,
                               checkboxGroupInput("view_year","Select year",choices = unique(data$call_year),selected = c('2016','2017','2018','2019') ),
                               
                        ),
                        column(4,
                               selectInput("view_city","Select city", multiple = TRUE, choices = unique(data$city_name),selected =c('PITTSBURGH','FINDLAY','PLUM','CRAFTON') ),
                               
                        ),
                        column(4,
                               selectInput("view_priority","Select priority",choices = unique(data$priority_desc),selected ="EMS ALS life threatening response" ),
                               
                        )
                    ),
                    fluidRow(
                        column(4,
                               plotlyOutput("plot1")
                        ),
                        column(4,
                               plotlyOutput("plot2")
                        )
                    ) 
                    
            ),
            
            # third tab content
            tabItem(tabName = "datatable",
                    DT::dataTableOutput("table")
            )
        )
    )
)

server <- function(input, output) {
    
    output$leafletmap <- renderLeaflet({
        leaflet() %>%
            addProviderTiles("OpenStreetMap.HOT", group = "HOT") %>%
            addProviderTiles("Stamen.TonerLite", group = "Toner Lite") %>%
            addPolygons(data = pitts, color = "darkblue",group = "Show County",weight = 2 ) %>%
            setView(-80.000, 40.400, 11) %>%
            addLayersControl(
                baseGroups = c("HOT", "Toner Lite"),
                overlayGroups = "Show County",
                options = layersControlOptions(collapsed = FALSE)
            )
    })
    
    data_subset <- reactive({
        #filted by city 
        data2 <- subset(data, city_name %in% input$city)
        data2 <- subset(data, call_year %in% input$year)
        
        return(data2)
    })
    
    observe({
        #add interactive markers based on year and city selected
        newdata <-  data_subset()
        leafletProxy("leafletmap", data = newdata) %>%
            clearMarkers() %>%
            addMarkers(lng=newdata$census_block_group_center__x,lat = newdata$census_block_group_center__y)
        
    })
    
    data_subset2 <- reactive({
        #filted by city 
        data3 <- data
        data3 <- subset(data3, call_year %in% input$view_year)
        data3 <- subset(data3, city_name %in% input$view_city)
        data3 <- subset(data3 ,priority_desc %in% input$view_priority)
        print(data3)
        return(data3)
    })
    output$plot1 <- renderPlotly({
        ggplot(data = data_subset2(),
               aes(x = call_year, fill = call_year)) +
            labs (y = "Incident counts on specific priority", x = "Incident Year") +
            geom_bar(stat = 'count') 
    })
    output$plot2 <- renderPlotly({
        ggplot(data = data_subset2(),
               aes(x = city_name, fill = city_name)) +
            labs (x = "City name", x = "Incident counts on specific city") +
            geom_bar(stat = 'count') 
    })
    
    
    
    #Create table 
    output$table = DT::renderDataTable(
        DT::datatable(data = data,
                      options = list(pageLength = 10),
                      rownames = FALSE
        )
        
    )
    
    
    
    
}

# Run the application 
shinyApp(ui = ui, server = server)
