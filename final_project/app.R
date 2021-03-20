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

# Read data 



ui <- dashboardPage(
    dashboardHeader(title = "placeholder"),
    dashboardSidebar(
        sidebarMenu(
            menuItem("Map", tabName = "gauge", icon = icon("dashboard")),
            menuItem("Chart",tabName = "analyze", icon=icon("diagnoses")),
            menuItem("Data Table", tabName = "datatable", icon = icon("table"))
            
        )
    ),
    dashboardBody(
        tabItems(
            # First tab content, display basic statistics from five angles in gauge
            tabItem(tabName = "gauge",
                    
            ),
            
            
            # Second tab content generate reactive charts based on how many states selected
            tabItem(tabName = "chart",
                    
                    
            ),
            # third tab content, analyze relationship between stats for selected states
            tabItem(tabName = "analyze",
           
            ),
            # fourth tab content, table output
            tabItem(tabName = "datatable",
                    DT::dataTableOutput("table")
            )
        )
    )
)

server <- function(input, output) {

    
    observeEvent(c(input$size,input$select_stat,input$select_stat2),{
        

        
        #Create table output
        output$table = DT::renderDataTable(
            DT::datatable(data = selected_data,
                          options = list(pageLength = 10),
                          rownames = FALSE
            )
            
        )
        
    })
    
    
}

# Run the application 
shinyApp(ui = ui, server = server)
