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
url <- URLencode('https://data.wprdc.org/api/3/action/datastore_search_sql?sql=SELECT * from "ff33ca18-2e0c-4cb5-bdcd-60a5dc3c0418"', repeated = TRUE)
# Send request
get <- GET(url)
# Retrieve results
rawdata <- fromJSON(content(get, "text"))$result$records
# Data originally contains 500,000 rows, manually created a subset of original data
data <- rawdata %>% sample_n(500, replace = FALSE)


#pitts <- readOGR("https://pghgishub-pittsburghpa.opendata.arcgis.com/datasets/a99f25fffb7b41c8a4adf9ea676a3a0b_0.geojson?outSR=%7B%22latestWkid%22%3A2272%2C%22wkid%22%3A102729%7D")
pitts <- readOGR("./county/Allegheny_County_Census_Block_Groups_2016.shp", layer = "Allegheny_County_Census_Block_Groups_2016")


plotui <- dashboardPage(
    dashboardHeader(title = "911 EMS Dispatches"),
    dashboardSidebar(
        sidebarMenu(
            menuItem("Map", tabName = "map", icon = icon("dashboard")),
            menuItem("Chart",tabName = "chart", icon=icon("diagnoses")),
            menuItem("Data Table", tabName = "datatable", icon = icon("table"))
            
        )
    ),
    dashboardBody(
        tabItems(
            # First tab content, display basic statistics from five angles in gauge
            tabItem(tabName = "map",
                    
            ),
            
            
            # Second tab content generate reactive charts based on how many states selected
            tabItem(tabName = "chart",
                    
                    
            ),

            # fourth tab content, table output
            tabItem(tabName = "datatable",
                    DT::dataTableOutput("table")
            )
        )
    )
)

server <- function(input, output) {


        #Create table output
        output$table = DT::renderDataTable(
            DT::datatable(data = selected_data,
                          options = list(pageLength = 10),
                          rownames = FALSE
            )
            
        )
        

    
    
}

# Run the application 
shinyApp(ui = ui, server = server)
