# Load required libraries

library(shiny)
library(leaflet)
library(sf)
library(RColorBrewer)  

# Read the CSV data
data <- read.csv("/Users/ruaidhrihiggins-lavery/Desktop/Cobens Changes/R/Outputs_R.csv")

# Read the shapefile data
shapefile <- st_read("/Users/ruaidhrihiggins-lavery/Desktop/Cobens Changes/R/infuse_dist_lyr_2011")

# Reproject shapefile data to WGS84
shapefile <- st_transform(shapefile, crs = st_crs("+proj=longlat +datum=WGS84"))

# Merge the data
merged_data <- merge(data, shapefile, by.x = "Local.Authority", by.y = "name", all.x = TRUE)

# Filter out polygons with empty geometry
merged_sf <- st_as_sf(merged_data)
merged_sf <- merged_sf[!st_is_empty(merged_sf$geometry), ]

# Define UI
ui <- fluidPage(
  titlePanel("UK Local Authorities Map"),
  mainPanel(
    selectInput("cobenefit", "Select Co-benefit Category", choices = colnames(data)[-1]), # Exclude the first column (Local Authority)
    leafletOutput("map"),
    div(id = "legend", class = "info legend", style = "position:fixed; bottom:50px; left:10px; background:white; padding:6px; border:1px solid #777; border-radius:5px; font-size:80%; z-index:999;")
  )
)

# Define server logic
server <- function(input, output) {
  
  # Render the map
  output$map <- renderLeaflet({
    
    # Create a leaflet map object
    leaflet() %>%
      addTiles() %>%
      setView(lng = -2.5, lat = 54.5, zoom = 6) %>%  # Set initial view to the UK
      
      # Add polygons layer
      addPolygons(
        data = merged_sf[!is.na(merged_sf[[input$cobenefit]]), ],  # Filter out NA values
        fillColor = ~colorNumeric(
          palette = "YlOrRd", 
          domain = merged_sf[[input$cobenefit]][!is.na(merged_sf[[input$cobenefit]])]
        )(merged_sf[[input$cobenefit]][!is.na(merged_sf[[input$cobenefit]])]),
        fillOpacity = 0.7,
        weight = 1,
        popup = ~Local.Authority  # Use 'Local.Authority' as popup content
      )
  })
  
  # Update the legend based on the selected co-benefit category
  observe({
    output$legend <- renderUI({
      legend <- paste("Legend for", input$cobenefit)
      for (i in 1:9) {
        legend <- paste(
          legend, "<br>", 
          round(seq(min(merged_sf[[input$cobenefit]][!is.na(merged_sf[[input$cobenefit]])]), 
                    max(merged_sf[[input$cobenefit]][!is.na(merged_sf[[input$cobenefit]])]), 
                    length.out = 9)[i], 2), 
          "<div style='background:", 
          brewer.pal(9, "YlOrRd")[i], 
          "; width: 20px; height: 20px; display: inline-block;'></div>"
        )
      }
      HTML(legend)
    })
  })
}

# Run the application
shinyApp(ui = ui, server = server)
