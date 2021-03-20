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

pitts <- readOGR("https://pghgishub-pittsburghpa.opendata.arcgis.com/datasets/a99f25fffb7b41c8a4adf9ea676a3a0b_0.geojson?outSR=%7B%22latestWkid%22%3A2272%2C%22wkid%22%3A102729%7D")
# pitts<- readOGR("./county/Allegheny_County_Census_Block_Groups_2016.shp", layer = "Allegheny_County_Census_Block_Groups_2016")


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
                               checkboxGroupInput("city","select_city",choices = unique(data$city_name))
                               
                        ),
                        column(4,
                               checkboxGroupInput("year","select_year",choices = unique(data$call_year))
                               
                        )
                    )
                    
                    
            ),
            
            
            # Second tab content 
            tabItem(tabName = "chart",
                    
                    
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
