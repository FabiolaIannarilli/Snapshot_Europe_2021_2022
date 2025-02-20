# Visualize Snapshot EU 2024 data

# load libraries
library(shiny)
library(leaflet)
library(leaflet.extras)
library(tidyverse)
# for plotting locations
awesome <- makeAwesomeIcon(
  icon = "info",
  iconColor = "black",
  markerColor = "blue",
  library = "fa"
)


# load data
load("data_for_shiny/data_prep.RData")

# Define UI for application that draws a histogram
ui <- fluidPage(
  
  fluidRow(
    column(12, align = "center",
           h1("Snapshot Europe 2021-2022: data exploration"), 
           h5("Last update: 19th Feb 2025. For questions, contact us at: snapshoteu@ab.mpg.de")
    ),
    column(12, align = "center",
            selectInput(inputId = "subproj",
                        label = "Select one or more subprojects",
                        choices = subprojs,
                        selected = subprojs[1], 
                        multiple = TRUE)
             ),
  tabsetPanel(
    tabPanel("Spatial coverage", align = "center",
             p("Check that the location of each camera is correct. You can measure the distance between two locations using the 'Draw a polyline` button (third from the top, on the left side of the map). Any error should be fixed in the WI page of the project.  "),
             leafletOutput("depl_map", width = "80%", height = 500)
    ), 
    tabPanel("Temporal coverage", align = "center",
             p("Check that the sampling period at each location (grey bars) and the dates associated with the sequences (colored dots) are as expected. The last day of sampling at each site should be the last day a camera was active (not the day of retrieval). Any error should be fixed in the WI page of the project."),
             plotOutput("plot_operability", width = "80%", height = 500)
             ),
    tabPanel("Taxonomic coverage", align = "center",
             plotOutput("plot_species", width = "100%", height = 500)
    )
  )
  
 )
)


# Define server logic required to draw a histogram
server <- function(input, output) {
  
  output$depl_map <- renderLeaflet({
    # select deployment based on selected subproject
    temp_loc <- loc %>%
      filter(subproject_name %in% input$subproj)
    
    
    # map deployments
    leaflet(data = temp_loc) %>%
      #addTiles() %>%  # Add default OpenStreetMap map tiles
      addProviderTiles('Esri.WorldImagery') %>% 
      addAwesomeMarkers(~longitude, 
                        ~latitude, 
                        icon = awesome,
                        popup = ~paste("loc:", as.character(placename), 
                                       "- subproj:", as.character(subproject_name))) %>% 
      addCircleMarkers(~longitude, 
                       ~latitude, 
                       label = ~paste("depl:", as.character(placename)),
                       color = "1dafdb",
                       weight = 3,
                       radius = 5,
                       clusterOptions = markerClusterOptions(showCoverageOnHover = FALSE)) %>% 
      addDrawToolbar(
        polylineOptions = drawPolylineOptions(metric=TRUE),
        editOptions=editToolbarOptions(selectedPathOptions=selectedPathOptions())
      )
  })
  
  output$plot_operability <- renderPlot({
    
    # select deployment based on selected subproject
    temp_oper <- depl_info %>%
      filter(subproject_name %in% input$subproj)
    
    temp_seq_oper <- n_daily_seq %>%
      filter(subproject_name %in% input$subproj)
    #browser()
    ggplot() +
      geom_errorbarh(data = temp_oper, aes(y = deployment_id,
                                           xmin = as.Date(start_date), 
                                           xmax = as.Date(end_date),
                                           #colour = placename
                                           ),
                     #height = 0, size = 3,
                     inherit.aes = FALSE) +
      geom_point(data = temp_seq_oper, aes(y = deployment_id,
                                           x = as.Date(record_date),
                                           color = n_seq, fill = n_seq), 
                 size = 3, 
                 inherit.aes = FALSE) +
      scale_color_viridis_c() +
      scale_fill_viridis_c() +
      scale_x_date() +
      labs(color = "Number of sequences", fill = "Number of sequences", size = NULL) +
      theme_classic() +
      theme(legend.position = "bottom",
            axis.title = element_blank(),
            axis.text = element_text(size = 14, face = "bold"),
            strip.text.y.right = element_text(angle = 0, size = 14)) +
      facet_grid(placename~., scales = "free_y")
    
  })
  
  output$plot_species <- renderPlot({
    
    # select deployment based on selected subproject
    temp_species <- n_species_seq %>%
      filter(subproject_name %in% input$subproj)
    
    ggplot(data = temp_species) +
      geom_bar(aes(y = common_name, x = n_seq, fill = subproject_name),
               inherit.aes = FALSE, stat="identity") +
      scale_fill_viridis_d() +
      labs(x = "Number of sequences",
           title = "Number of sequence by tag", 
           fill = "Subproject") +
      theme_classic() +
      theme(legend.position = "bottom",
            plot.title = element_text(size = 14, face = "bold"),
            axis.title = element_blank(),
            axis.text = element_text(size = 14, face = "bold")) 
    
  })
  
  
}


# Run the application 
shinyApp(ui = ui, server = server)

# Deploy app --------------------------------------------------------------
#library(rsconnect)
# Only at first deployment: connect account using token: shinyapp.io -> user -> token. Copy and run in R
#deployApp()