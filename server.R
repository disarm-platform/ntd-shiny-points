library(raster)
library(sp)
library(leaflet)
library(RANN)
library(rgeos)
library(rjson)
library(httr)
library(wesanderson)
library(DT)
library(geoR)
library(sf)
library(disarmr)

run_checks <- dget("preprocess.R")

options(shiny.maxRequestSize=30*1024^2)

# Define map
map <- leaflet(max) %>%
  addTiles(
    "https://cartodb-basemaps-{s}.global.ssl.fastly.net/light_all/{z}/{x}/{y}{r}.png"
  )

shinyServer(function(input, output) {
  
  map_data <- eventReactive(input$get_predictions, {
    inFile <- input$File
    #inFile_pred <- input$predFile
    if (is.null(inFile))
      return(NULL)
    
    # if (is.null(inFile_pred))
    #   return(NULL)
    
    
    # Give loading bar
    withProgress(message = 'Hold on',
                 detail = 'Crunching data..',
                 value = 5,
                 {
                   
                   points <- read.csv(inFile$datapath)
                   
                   # Run checks
                   points <- run_checks(points)
                   
                   # Change IDs to characters in case they come in as levels
                   points$id <- as.character(points$id)
                   
                   # Convert to sf object
                   point_data <- st_as_sf(SpatialPointsDataFrame(SpatialPoints(points[,c("lng", "lat")]),
                                                                 points[,c("n_trials", "n_positive", "id")]))
                   
                   
                   
                   # Prepare for function
                   exceedance_threshold = input$threshold / 100
                   uncertainty_fieldname = "exceedance_probability"
                   if(input$batch_size == 0){
                         batch_size <- NULL
                       }else{
                       batch_size = input$batch_size
                   }
                   layer_names = c("elev_m",
                                   "dist_to_water_m",
                                   "dist_to_road_m",
                                   "bioclim1", 
                                   "bioclim4",
                                   "bioclim12", 
                                   "bioclim15")
                 
                   # Make call to algorithm
                   print("Making request")
                   result_sf <- prevalence_predictor_mgcv(point_data = point_data, 
                                                          layer_names = layer_names,
                                                          exceedance_threshold = exceedance_threshold,
                                                          batch_size = batch_size,
                                                          uncertainty_fieldname = uncertainty_fieldname)
                   

                   return(
                     list(
                       points = points,
                       result_sf = result_sf
                     )
                   )
                 })
  })
  
  output$pred_table <- DT::renderDT({
    if (is.null(map_data())) {
      return(NULL)
    }

    output_table <- as.data.frame(map_data()$result_sf)
    if(is.null(output_table$adaptively_selected)){
    output_table <- output_table[,c("id", 
                                    "exceedance_probability", 
                                    "prevalence_prediction")]
    }else{
      output_table <- output_table[,c("id", 
                                      "exceedance_probability", 
                                      "prevalence_prediction",
                                      "adaptively_selected")]
    }

    output_table[, 2:3] <- round(output_table[, 2:3], 2)
    names(output_table) <-
      c("Location ID", 
        "Probability of being a hotspot", 
        "Predicted prevalence",
        "Adaptively selected")[1:ncol(output_table)]
    DT::datatable(caption = "Results",
                  output_table,
                  options = list(pageLength = 10),
                  rownames = F)
  })
  
  
  output$hotspot_map <- renderLeaflet({
    
    if(input$get_predictions[1]==0){
      return(map %>% setView(0,0,zoom=2))
    }
    
    # Define color palette
    pal <-
      colorNumeric(wes_palette("Zissou1", 10, type = "continuous")[1:10],
                   seq(0, 1, 0.01))
    
    prev <- map_data()$points$n_positive / map_data()$points$n_trials
    prev_pal <- 
      colorNumeric(wes_palette("Zissou1", 10, type = "continuous")[1:10],
                   prev, na.color = NA)
    
    labels <- sprintf(
      "<strong>%s</strong><br/>Hotspot probability %g",
      map_data()$result_sf$id,
      round(map_data()$result_sf$exceedance_probability, 3)
    ) %>% lapply(htmltools::HTML)
    
    # Map
    hotspot_class <-
      ifelse(map_data()$result_sf$exceedance_probability >= input$prob_threshold / 100,
             1,
             0)
    
    map %>% 
      
      addCircleMarkers(    
        data = map_data()$result_sf,
        color = pal(hotspot_class),
        fillOpacity = 0.6,
        weight = 1,
        radius = 4,
        popup = map_data()$result_sf$id,
        group = "Hotspot predictions") %>%
      
      addCircleMarkers(
        #map_data()$points$lng[!is.na(map_data()$points$n_trials)],
        #map_data()$points$lat[!is.na(map_data()$points$n_trials)],
        map_data()$points$lng,
        map_data()$points$lat,
        group = "Initial survey points",
        col = prev_pal(prev),
        radius = 2,
        popup = map_data()$points$id
      ) %>%
      
      addCircleMarkers(
        data = map_data()$result_sf[map_data()$result_sf$adaptively_selected,],
        group = "Recommended survey points",
        col = "green",
        radius = 3,
        popup = map_data()$result_adaptive_sf$id[map_data()$result_sf$adaptively_selected]
      ) %>%
      
      leaflet::addLegend(colors = pal(c(0, 1)),
                         labels = c("Not hotspot", "Hotspot"),
                         group = "Hotspot predictions") %>%
      
      leaflet::addLegend(pal = prev_pal,
                         values = prev,
                         group = "Initial survey points",
                         title = "Prevalence") %>%
      
      addLayersControl(overlayGroups = c("Initial survey points",
                                         "Hotspot predictions",
                                         "Recommended survey points"),
                       options = layersControlOptions(collapsed = F)) %>%
      
      hideGroup( c("Initial survey points",
                   "Recommended survey points"))
  })
  
  
  
  output$posterior <- renderPlot({
    
    threshold <- input$post_threshold
    
    set.seed(1981)
    sample <- rbinom(1000, 100, 0.10)
    binom <- density(sample,0.7)
    binom <- data.frame(x=binom$x, y=binom$y)
    plot(binom$x, binom$y, type="l", lwd=4, axes=F, cex.lab=1.5,
         xlab="Infection prevalence (%)",ylab="Relative probability")
    axis(1,xlim=c(0,25))
    text(17,0.11, paste0("Probability that prevalence exceeds ", 
                         threshold,"% is ", round(mean(sample>=threshold),3)),cex=1.5)
    polygon(c(binom$x,min(binom$x)), c(binom$y,binom$y[1]),
            col="gray80",
            border=NA)
    lines(binom$x, binom$y, type="l", lwd=4)
    
    
    
    closest_x <- which.min(abs(threshold - binom$x))
    lines(rep(threshold,2), c(0,binom$y[closest_x]), col="red", lwd=3)
    polygon(c(threshold,threshold,binom$x[binom$x>threshold], threshold),
            c(0,binom$y[closest_x], binom$y[binom$x>threshold],0),
            col=rgb(1,0.2,0.1, 0.5),
            border=NA)
    lines(binom$x, binom$y, type="l", lwd=4)
    
  })
  
  # logos
  output$logo <- renderImage({
    # Return a list containing the filename
    list(src = "logo_transparent.png")
  }, deleteFile = FALSE)
  
  
  # Handle downloads
  output$downloadData <- downloadHandler(
    filename = function() {
      paste("predictions.csv")
    },
    content = function(file) {
      #download_table <- as.data.frame(st_read(as.json(result$result)))
      download_table <- as.data.frame(map_data()$result_sf)
      
      # Drop geometery
      download_table <- subset(download_table, select = -c(geometry))
      download_table <- cbind(download_table, st_coordinates(map_data()$result_sf))
      write.csv(download_table, file, row.names = FALSE)
    }
  )
  
  output$downloadGeoData <- downloadHandler(
    filename = function() {
      paste("predictions.geojson")
    },
    content = function(file) {
      st_write(map_data()$result_sf, file)
    }
  )
  
  output$downloadAdaptiveData <- downloadHandler(
    filename = function() {
      paste("adaptive_samples.csv")
    },
    content = function(file) {
      download_table_adaptive <- as.data.frame(result_sf)
      write.csv(download_table_adaptive, file, row.names = FALSE)
    }
  )
  
  output$downloadAdaptiveGeoData <- downloadHandler(
    filename = function() {
      paste("adaptive_samples.geojson")
    },
    content = function(file) {
      st_write(result_adaptive_sf, file)
    }
  )
  
})


