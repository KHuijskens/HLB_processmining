# Function to load required packages and download packages that are missing
require_packages <- function(x){
  for( i in x ){
    #  require returns TRUE invisibly if it was able to load package
    if( ! require( i , character.only = TRUE ) ){
      #  If package was not able to be loaded then re-install
      install.packages( i , dependencies = TRUE )
      #  Load package after installing
      require( i , character.only = TRUE )
    }
  }
}

#  Then try/install packages...
require_packages(c("dplyr", "shiny", "shinythemes", "shinyWidgets", "shinyjs", "shinydashboard",
      "ggplot2","DiagrammeR", "bupaR", "edeaR", "processmapR", "processanimateR", "DT",
      "plotly", "rsconnect", "RCurl", "daqapo", "imager", "bslib"))



x <- url( "https://raw.githubusercontent.com/KHuijskens/HLB_processmining/e24595472493b5bca51bdaf99b3088a82f4fd591/final_set.csv")
dummy_data <- read.csv(x)
dummy_data$Eind <- as.POSIXct(dummy_data$Eind)


# Data verwerken in eventlog
eventlog <- eventlog(
  dummy_data,
  case_id = "Factuurnr.",
  activity_id = "Taak",
  activity_instance_id = "X",
  lifecycle_id = "Taak",
  timestamp = "Eind",
  resource_id = "Resource")


# Define UI 
ui <- fluidPage(
  # Application title,
  img(src= "HLBIcon.png", align = "left", height = 63, width = 230), # To add HLB logo
  h2(id='big-heading','Process Mining Dashboard - KLANTNAAM - JAAR - APPLICATIE', align = 'center'), # To center title
  tags$style(HTML("#big-heading{color: #1A5276;}")), # To make title blue
  
  
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
                selectInput("processmap", "Selecteer type processmap", choices = c("Frequentie", "Tijd in uren")), # these choices represent: c(frequency(), performance(FUN = median, units = "hours")
                dateRangeInput("Daterange", "Selecteer datarange (voor tabel)", start = min(dummy_data$Datum), end = max(dummy_data$Datum), min = min(dummy_data$Datum), max = max(dummy_data$Datum))),
                          
              actionButton("update", "Update"),
              actionButton("reset", "Reset filters")),
                        
          # Main panel with a plot for doorlooptijd and a table for actions performed
          mainPanel( # infoboxes to show basic facts
              infoBox(title = "Aantal facturen: ", value = eventlog %>% n_cases, icon = icon("file-invoice-dollar"), color = "aqua", fill = TRUE),
              infoBox(title = "Aantal stromen: ", value = eventlog %>% n_traces, icon = icon("arrow-alt-circle-right"), color = "light-blue", fill = TRUE),
              infoBox(title = "Gemiddelde doorlooptijd: ", value = round(throughput_time(eventlog, level = "log", units = "days")[4], 1), " dagen", icon = icon("business-time"), color = "navy", fill = TRUE), 
              tabsetPanel(
                tags$style(type = "text/css", "a{color: #1A5276;}"),  # tags$style to change the color of the tabnames to blue
                tabPanel("Process map", grVizOutput("processmap")),
                tabPanel("Animatie process",tabPanel("Animatie"), processanimaterOutput("animatie")),
                tabPanel("Resource map", grVizOutput(outputId = "resource")),
                tabPanel("Resource-activity matrix", plotOutput("resource_freq")),
                tabPanel("Plot doorlooptijd", plotly::plotlyOutput("doorlooptijd")))))),
             
 #---------------------------------------------------------------------------
 # Tab 2: Details
             
 tabPanel("Details",
          tabPanel("Tabel", DT::DTOutput("table1")))))


#---------------------------------------------------------------------------



# Initializing the selected data for the eventlog (plot) and table
subset_of_data_event_log <- subset(eventlog, Taak == eventlog$Taak[1])
subset_of_data_table <- dummy_data[dummy_data$Taak == dummy_data$Taak[1] & dummy_data$Resource == dummy_data$Resource[1] & dummy_data$Factuurnr. == dummy_data$Factuurnr.[1],]




# Define server logic required to draw a histogram 

server <- function(input, output, session) {
  #---------------------------------------------------------------------------
  # Tab 1: Algemeen 

    # Initial animation
  output$animatie <- renderProcessanimater({
    animate_process(eventlog, mode = "relative", jitter = 1, initial_state = "paused", 
                    repeat_count = 3, repeat_delay = 5, 
                    mapping = token_aes(color = token_scale("resource", scale = "ordinal")))
  })
  
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
  
  # Initial process map
  output$processmap <- renderGrViz({
    plot <- process_map(eventlog = eventlog, type = frequency(), render = F)
    render_graph(plot)
  })
  
  # Initial resource map
  output$resource <- renderGrViz({
    plot <- resource_map(eventlog = eventlog, type = frequency(), render = F)
    render_graph(plot)
  })
  
  # Initial resource-activity matrix
  output$resource_freq <- renderPlot({
  resource_freq <- resource_frequency(eventlog, "resource-activity")
  plot(resource_freq)
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

    # If else statement to update the type-argument in process_map for changed input
    if (input$processmap == "Frequentie") { 
      type_update = frequency()} 
    else { type_update = performance(FUN = median, units = "hours")} 
    
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
    
    # 2. Updating the filter on process map 
    output$processmap <- renderGrViz({
      
      plot <- process_map(eventlog = eventlog, type = type_update, render = F)
      render_graph(plot)
    })
    
    
    # 3. Updating the factuur filter on resourcemap
    output$resource <- renderGrViz({
      plot <- resource_map(eventlog = subset_of_data_event_log_resource_map, render = F)
      render_graph(plot)
    })
    
    
    
    # 4. Resetting the filters tab 1
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

