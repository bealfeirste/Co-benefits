# Load required libraries
library(shiny)
library(leaflet)
library(sf)
library(RColorBrewer)  
library(openxlsx)

# Read the Excel data
data <- read.xlsx("/Users/ruaidhrihiggins-lavery/Desktop/Cobens Changes/R/Shiny/DZ/predicted_data_per.cap.xlsx")

# Read the shapefile data
shapefile <- st_read("/Users/ruaidhrihiggins-lavery/Desktop/GIS/29_04/mreged_uk_dz_boundaries_2.shp")

# Reproject shapefile data to WGS84
shapefile <- st_transform(shapefile, crs = st_crs("+proj=longlat +datum=WGS84"))

# Merge the data
merged_data <- merge(data, shapefile, by.x = "Datazone", by.y = "DZ", all.x = TRUE)

# Filter out polygons with empty geometry
merged_sf <- st_as_sf(merged_data)
merged_sf <- merged_sf[!st_is_empty(merged_sf$geometry), ]

# Define UI
ui <- fluidPage(
  titlePanel("UK Datazones Map"),
  mainPanel(
    selectInput("cobenefit", "Select Co-benefit Category", choices = colnames(data)[-1]), # Exclude the first column (Datazone)
    leafletOutput("map"),
    uiOutput("legend")  # Render the legend in UI
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
        weight = 0.5,    # Set polygon boundary weight to make them thinner
        color = "transparent",  # Set polygon boundaries to transparent (no color)
        opacity = 0.7,   # Set polygon boundary opacity
        popup = ~Datazone  # Use 'Datazone' as popup content
      )
  })
  
 # Update the legend based on the selected co-benefit category
output$legend <- renderUI({
  legend <- paste("<div><strong>Co-Benefits per capita (£, 2024 - 2050)</strong></div>")
  
  # Define thresholds or ranges
  thresholds <- c(0, 10, 50, 100, 500, 1000, 5000, 10000, 50000)
  
  for (i in 1:(length(thresholds) - 1)) {
    legend <- paste(
      legend,
      "<div>",
      thresholds[i], "-", thresholds[i + 1],
      "<div style='background:",
      brewer.pal(9, "YlOrRd")[i],
      "; width: 20px; height: 20px; display: inline-block;'></div>",
      "</div>"
    )
  }
  HTML(legend)
})
}

# Run the application
shinyApp(ui = ui, server = server)
