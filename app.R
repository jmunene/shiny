library(shiny)
library(dplyr)
library(leaflet)
library(leafletplugins)
library(tidyr)
library(ggplot2)
library(htmltools)

## Village Level APP:

# Load the required data sets

# Field verification dataset for map:
field_data_map <- read.csv('data/Water_Points_data_withCode_sep_20160924.csv',header = TRUE)

# Field verification dataset for chart/map:
field_data_table <- read.csv('data/Water_Points_data_withCode_20160924.csv',header = TRUE)

# Call verification dataset:
call_data_sub <- read.csv('data/Call_data_FieldSub_201609.csv',header = TRUE)
# July PbR dataset
july_pbr_sub <- read.csv('data/july_pbr_sub.csv',header = TRUE)

#The APP"

app <- shinyApp(
  
  # THE UI SIDE CODE:

  ui <- fluidPage(
    # Title of the app:
    titlePanel('Tanzania Water Points Mapping'),
    # The filter control: Under choices=levels(you can change these depending on the filter source)
      sidebarPanel("Please select a Village to see the Water Points in the villages:",
                   selectInput("select", "Ward:", choices = levels(field_data_table$village_merge))),
                   
    mainPanel(
      tabsetPanel(
  # Maps tab split into 2 columns:
        tabPanel("Map",
          fluidRow(
            column(6,"July PbR Data Map - Village Level",
                   leafletOutput('myMap_pbr')),
            column(6,
                   "Field Verification Data Map - Village Level",
                   leafletOutput('myMap_field'))
            )),
  # Charts tab split into 3 columns:     
          tabPanel("Charts", 
               fluidRow(
                 column(4,
                       "July PbR Data Status Chart - Village Level",
                       plotOutput("status_pbr")),
                 column(4,
                       "Call Verification Data Status Chart - Village Level",
                       plotOutput("status_call")),
                 column(4,
                       "Field Validation Data Status Chart - Village Level",
                       plotOutput("status_field"))
                 )),
  # Table tab.... still nothing(You can remove or comment it our)
      tabPanel("Table", tableOutput("table"))),
  
    width = 12) # This sets the Main Panel width to the Maximun 12.
    ),

   # The SERVER CODE:

  server <- function(input, output) {
    
    size <- 5 # This creates the size of the circle marker
    
    status_color <- colorFactor(c('blue','yellow','red'),domain = c('Functional','Functional needs repair','Not functional')) # This creates the map legend colors
    
    # Call the reactive select input filter, created on the UI side:
    select <- reactive({
      input$select})
    
    ## PBR Map

    output$myMap_pbr <- renderLeaflet({
      
      # Filter the data for the map with the selected Village:
      
      july_pbr_sub_f <-  july_pbr_sub %>%
      filter(july_pbr_sub$village==select())
  
    wp_map_pbr  <- leaflet(july_pbr_sub_f)%>% 
      
      addTiles() %>%
      fitBounds(~min(longitude), ~min(latitude), ~max(longitude), ~max(latitude))%>% # Sets the view to the selected Village points
      addCircleMarkers(radius = ~ size,color = july_pbr_sub_f$status,fill=FALSE)%>% # Adds the circle markers
      addProviderTiles("CartoDB.Positron") %>% # Adds the provider tiles
      
  # If you want to use the MapBox map you shared:
      # - delete line 93(addProviderTiles())
      # - Add this code below line 70, tz_map <- 'https://api.mapbox.com/styles/v1/ona/cisap1jep00082xogzitxxyw9/tiles/256/{z}/{x}/{y}?access_token=pk.eyJ1Ijoib25hIiwiYSI6IlVYbkdyclkifQ.0Bz-QOOXZZK01dq4MuMImQ'
      # - change line 90(addTiles()), into this: addTiles(urlTemplate = tz_map) %>% 
      
      addCircleMarkers(radius= ~size,color=~status_color(status),group = 'marker', label = ~wptname,stroke=FALSE,fillOpacity = 0.5,popup=~htmlEscape(wptname)) %>% # Adds the circle markes colors and pop up name
      
      # Add the Legend
      addLegend("bottomleft", 
                colors = c("blue","yellow", "red"),
                labels = c("Functional.",
                           "Functional needs repair.",
                           "Not functional."), 
                opacity = 0.8)
    }
    )
 
  ## Field Map (comments on code same as above)

    
    output$myMap_field <- renderLeaflet({
      field_data_mapf <- field_data_map %>%
        filter(field_data_map$village_merge==select())
      
      wp_map_field  <- leaflet(field_data_mapf)%>% 
        fitBounds(~min(long), ~min(lat), ~max(long), ~max(lat)) %>%
        addTiles() %>%
        addCircleMarkers(radius = ~ size,color = field_data_mapf$wpt_status,fill=FALSE)%>%
        addProviderTiles("CartoDB.Positron") %>%
        addCircleMarkers(radius= ~size,color=~status_color(wpt_status),group = 'marker', label = ~wpt_name,stroke=FALSE,fillOpacity = 0.5,popup=~htmlEscape(wpt_name)) %>%
        addLegend("bottomleft", 
                  colors = c("blue","yellow","red"),
                  labels = c("Functional.",
                             'Functional needs repair',
                             "Not functional."), 
                  opacity = 0.8)
    }
    )
    
   ## PBR Table:
    
    output$status_pbr <- renderPlot({
      
      # Filter the data based on the selected Village:
      
      july_pbr_sub_f <- july_pbr_sub %>%
        filter(july_pbr_sub$village==select())
      
      # Create a Table using the Table function, droplevels() on the second argument drops irrelevant villages, otherwise the table will be cluttered
      
      pbr_wp_status <- table(july_pbr_sub_f$status,droplevels(july_pbr_sub_f$village))
      
      # Plot the table output from the above code:
      
          status_pbr <- barplot((pbr_wp_status),beside = TRUE,xlab = "Villages", ylab = "Count of Status",ylim=c(0,30),
                                           col = c("blue","yellow",'red'), # Adds color to the 3 WP status options
                                legend = levels(unique(july_pbr_sub_f$status))) # Adds a Legend with status of the WP as the levels
      text(status_pbr,pbr_wp_status+3, as.character(pbr_wp_status)) # Adds value labels to the chart bars, +3 is the distance between the value label and the top of the bar
    })

## Call Table: (Code comments as above)
    
    output$status_call <- renderPlot({
     call_data_sub_f <- call_data_sub %>%
        filter(call_data_sub$village_merge==select())
      call_wp_status <- table(call_data_sub_f$WPts_current_status,droplevels(call_data_sub_f$village_merge))
      status_call <- barplot((call_wp_status),beside = TRUE,xlab = "Villages", ylab = "Count of Status",ylim=c(0,30),
                        col = c("blue","red"),
                        legend = levels(unique(call_data_sub_f$WPts_current_status)))
      text(status_call,call_wp_status+3, as.character(call_wp_status))
    })
## Field Table: (Code comments as above)
    
    output$status_field <- renderPlot({
      field_data_tablef <- field_data_table %>%
        filter(field_data_table$village_merge==select())
      field_wp_status <- table(field_data_tablef$wpt_status,droplevels(field_data_tablef$village_merge))
      status_field <- barplot((field_wp_status),beside = TRUE,xlab = "Villages", ylab = "Count of Status",ylim=c(0,30),
                              col = c('blue','yellow','red'),
                              legend = levels(unique(field_data_tablef$wpt_status)))
      text(status_field,field_wp_status+3, as.character(field_wp_status))
    })
  }
)

