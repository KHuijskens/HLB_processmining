rm(list = ls())

library(dplyr)
library(ggplot2)
library(shiny)
library(shinythemes)
library(shinyWidgets)
library(shinyjs)
library(shinydashboard)
library(DiagrammeR)
library(bupaR)
library(edeaR)
library(processmapR)
library(processanimateR)
library(shinycssloaders)
library(dplyr)
library(DT)
library(plotly)
library(rsconnect)


# as.POSIXct() # om timestamp te behouden
dummy_data <- read.csv("~/kimberley.huijskens/Shiny/final_set.csv")
View(dummy_data)
dummy_data$Eind_date <- as.Date(dummy_data$Eind) # Haalt timestamp weg, puur datum
dummy_data$Eind_posixct <- as.POSIXct(dummy_data$Eind)


# Data verwerken in eventlog
eventlog <- eventlog(
  dummy_data,
  case_id = "Factuurnr.",
  activity_id = "Taak",
  activity_instance_id = "X",
  lifecycle_id = "Taak",
  timestamp = "Eind_posixct",
  resource_id = "Resource")


# Define UI 
ui <- fluidPage(
  # Application title
  titlePanel('Process Mining Dashboard - KLANTNAAM - JAAR - APPLICATIE'),
  
  # Theme
  theme = shinytheme("flatly"),
  
  # To use reset buttons
  useShinyjs(),

  
  navbarPage('Process Mining', 
  #---------------------------------------------------------------------------
  # Tab 1: Algemeen 
             
  tabPanel("Algemeen",
                      
  # Sidebar with a select input for entities and reset and update button
  sidebarLayout(
      sidebarPanel(
        div( # to reset the filters in tab 1
            id = "form_tab1", 
            pickerInput("Taakpicker","Selecteer taak (voor plot en tabel)", choices = unique(dummy_data$Taak), selected = dummy_data$Taak, options = list(`actions-box` = TRUE),multiple = T),
            pickerInput("Factuurpicker","Selecteer factuur (voor resource map en tabel)", choices = unique(dummy_data$Factuurnr.), selected = dummy_data$Factuurnr., options = list(`actions-box` = TRUE),multiple = T),
            pickerInput("Resourcepicker","Selecteer resource (voor tabel)", choices = unique(dummy_data$Resource), selected = dummy_data$Resource, options = list(`actions-box` = TRUE),multiple = T),
            dateRangeInput("Daterange", "Selecteer datarange (voor tabel)", start = min(dummy_data$Datum), end = max(dummy_data$Datum), min = min(dummy_data$Datum), max = max(dummy_data$Datum))),
    
            actionButton("update", "Update"),
            actionButton("reset", "Reset filters")),
                          
  # Main panel with a plot for doorlooptijd and a table for actions performed
      mainPanel(
        infoBox(title = "Aantal facturen: ", value = eventlog %>% n_cases, icon = icon("file-invoice-dollar"), color = "aqua", fill = TRUE),
        infoBox(title = "Aantal stromen: ", value = eventlog %>% n_traces, icon = icon("arrow-alt-circle-right"), color = "light-blue", fill = TRUE),
        infoBox(title = "Gemiddelde doorlooptijd: ", value = round(throughput_time(eventlog, level = "log", units = "days")[4], 1), " dagen", icon = icon("business-time"), color = "navy", fill = TRUE), 
         tabsetPanel(
            tabPanel("Process map", process_map(eventlog)),
            tabPanel("Resource map", grVizOutput(outputId = "process")),
            tabPanel("Plot doorlooptijd", plotly::plotlyOutput("doorlooptijd")))))),
                      
  #---------------------------------------------------------------------------
  # Tab 2: Details
                      
  tabPanel("Details",
            tabPanel("Tabel", DT::DTOutput("table1")))))


#---------------------------------------------------------------------------

# Initializing the selected data for the eventlog (plot) and table
subset_of_data_event_log <- subset(eventlog, Taak == eventlog$Taak[1])
subset_of_data_table <- dummy_data[dummy_data$Taak == dummy_data$Taak[1] & dummy_data$Resource == dummy_data$Resource[1] & dummy_data$Factuurnr. == dummy_data$Factuurnr.[1],]

# Tab 1 - algemeen - doorlooptijd plot, process map, resource map
# Tab 2 - details - table with filters

# Define server logic required to draw a histogram 
server <- function(input, output, session) {
  #---------------------------------------------------------------------------
  # Tab 1: Algemeen 
  
  # Initial plot
  output$doorlooptijd <- plotly::renderPlotly({
    doorlooptijd <- throughput_time(eventlog = eventlog, level = c('case'), units = c('days'))
    # Plot 1 doorlooptijd
    ggplot(doorlooptijd, aes(doorlooptijd$throughput_time)) +
      geom_histogram(fill = 'deepskyblue3', color = 'deepskyblue4', bins = 50) + 
      ggtitle('Verdeling doorlooptijd facturen - JAAR') + 
      xlab('Doorlooptijd in dagen') + 
      ylab('Frequentie') + 
      theme_classic()
  })
  
  # Initial resource map
  output$process <- renderGrViz({
    plot <- resource_map(eventlog = eventlog, render = F)
    render_graph(plot)
  })
  
  table_data <- reactive(dummy_data)
  
  # Initial table
  output$table1 <- DT::renderDT(server = FALSE, {
    datatable(table_data(),
              extensions = 'Buttons', 
              options = list(dom = 'Bfrtip',
                             buttons = c('copy', 'csv', 'excel', 'pdf', 'print')))
    
  })  
  
  
  # Update button tab 1 
  observeEvent(input$update, {
    
    # Updating the subset happens outside of the renderfunction, else it will   
    # overrule the update-button
    subset_of_data_event_log <- subset(eventlog, Taak %in% input$Taakpicker)
    subset_of_data_event_log_resource_map <- subset(eventlog, Factuurnr. %in% input$Factuurpicker)
    
    # 1. Updating the filter on plot, rendering the plot
    output$doorlooptijd <- plotly::renderPlotly({
      doorlooptijd <- throughput_time(eventlog = subset_of_data_event_log, level = c('case'), units = c('days'))
      # Plot 1 doorlooptijd
      ggplot(doorlooptijd, aes(doorlooptijd$throughput_time)) +
        geom_histogram(fill = 'deepskyblue3', color = 'deepskyblue4', bins = 50) + 
        ggtitle('Verdeling doorlooptijd facturen - JAAR') + 
        xlab('Doorlooptijd in dagen') + 
        ylab('Frequentie') + 
        theme_classic()
    })
    
    # 2. Updating the factuur filter on resourcemap
    output$process <- renderGrViz({
      plot <- resource_map(eventlog = subset_of_data_event_log_resource_map, render = F)
      render_graph(plot)
    })
    
    
    # 3. Resetting the filters tab 1
    observeEvent(input$reset, {
      reset("form_tab1")
    })
    

  })
  
  #---------------------------------------------------------------------------
  # Tab 2: Details
  
  # 1. Update button tab 2
  observeEvent(input$update, {
    
    # Updating the subset happens outside of the render function, else it will   
    # overrule the update-button
    subset_of_data_table <- dummy_data[dummy_data$Taak  %in%  input$Taakpicker & dummy_data$Resource  %in%  input$Resourcepicker & dummy_data$Factuurnr. %in% input$Factuurpicker & dummy_data$Datum >= input$Daterange[1] & dummy_data$Datum <= input$Daterange[2],]
    table_data <- reactive(subset_of_data_table)
    
    # 1. Updating the filter on table (tab 2) + adding download/copy/print buttons
    output$table1 <- DT::renderDT(server = FALSE, {
      datatable(table_data(),
      extensions = 'Buttons', 
      options = list(dom = 'Bfrtip',
                     buttons = c('copy', 'csv', 'excel', 'pdf', 'print')))
      
    })    
    
  })


 # )
  
}


# Run the application 
shinyApp(ui = ui, server = server)

