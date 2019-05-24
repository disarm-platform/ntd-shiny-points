library(raster)
library(sp)
library(leaflet)
library(RANN)
library(rgeos)
#library(geosphere)
library(rjson)
library(httr)
library(wesanderson)
library(readr)
library(stringi)
library(DT)
library(ggplot2)
library(geoR)
library(sf)
library(RJSONIO)

run_checks <- dget("preprocess.R")

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
                   #pred_points <- read.csv(inFile_pred$datapath)
                   
                   # Run checks
                   run_checks(points)
                   
                   # Change IDs to characters in case they come in as levels
                   points$id <- as.character(points$id)
                   #pred_points$ID <- as.character(pred_points$ID)
                   

                   
                   # Convert to sf object
                   points_sf <- st_as_sf(SpatialPointsDataFrame(SpatialPoints(points[,c("lng", "lat")]),
                                                                       points[,c("n_trials", "n_positive", "id")]))
                   
                   
                   # Prepare input as JSON
                   input_data_list <-
                     list(
                       point_data = geojson_list(points_sf),
                       exceedance_threshold = input$threshold /
                                                   100,
                       layer_names = c("elev_m",
                                       "dist_to_water_m",
                                       "bioclim1",
                                       "bioclim4",
                                       "bioclim12",
                                       "bioclim15")
                     )
                   
                   
                   # Make call to algorithm
                   print("Making request")
                   response <-  httr::POST(url = "https://faas.srv.disarm.io/function/fn-prevalence-predictor",
                                           #url = "https://enflaqfc4jbk.x.pipedream.net",
                                           body = RJSONIO::toJSON(input_data_list, .na="null"),
                                           content_type_json())
                   
                   print("Got response")
                   print(response$status_code)
                   # Check it ran. If not, run again.
                   if (response$status_code != 200) {
                     print("Trying again")
                     response <- request_call()
                     print("Got second response")
                   }
                   
                   # parse result
                   json_response <-
                     httr::content(response, as = 'text') # this extracts the response from the request object
                   
                   result <-
                     rjson::fromJSON(json_response) # this will put the response in a useful format
                   
                   result_sf <- st_read(as.json(result$result))
                  
                   # Add ID column back
                   result_sf$id <- points_sf$id
                   result_sf <<- result_sf

                   # Get adaptive sampling recommendations
                   input_data_list_adaptive <- list(
                     point_data = geojson_list(result_sf),
                     uncertainty_fieldname = "exceedance_uncertainty",
                     batch_size = input$batch_size
                   )
                   
                   adaptive_sampling_response <-  httr::POST(
                     url = "https://faas.srv.disarm.io/function/fn-adaptive-sampling",
                     body = toJSON(input_data_list_adaptive),
                     content_type_json())
                   
                   # parse result
                   json_response_adaptive <-
                     httr::content(adaptive_sampling_response, as = 'text') # this extracts the response from the request object
                   
                   result_adaptive <-
                     rjson::fromJSON(json_response_adaptive) # this will put the response in a useful format
                   
                   result_adaptive_sf <<- st_read(as.json(result_adaptive))

                   
                   return(
                     list(
                       points = points,
                       result_sf = result_sf,
                       result_adaptive_sf = result_adaptive_sf
                     )
                   )
                 })
  })
  
  output$pred_table <- DT::renderDT({
    if (is.null(map_data())) {
      return(NULL)
    }
    #uncertainty <- abs(map_data()$sp_points_df$probability - 0.5)
    output_table <-
      as.data.frame(map_data()$result_adaptive_sf)[,c("id", "exceedance_probability")]
    output_table[, 2] <- round(output_table[, 2], 2)
    names(output_table) <-
      c("Location ID", "Probability of being a hotspot")
    DT::datatable(caption = "Recommended survey points",
                  output_table,
                  options = list(pageLength = 10),
                  rownames = F)
  })
  
  output$hotspot_table <- DT::renderDT({
    if (is.null(map_data())) {
      return(NULL)
    }
    hotspot_index <-
      which(map_data()$result_sf$exceedance_probability >= input$prob_threshold / 100)
    hotspot_table <- as.data.frame(map_data()$result_sf)[hotspot_index, c("id", "exceedance_probability")]
    hotspot_table[, 2] <- round(hotspot_table[, 2], 2)
    names(hotspot_table) <-
      c("Location ID", "Probability of being a hotspot")
    DT::datatable(
      caption = "Predictions",
      hotspot_table,
      options = list(pageLength = 10,
                     columnDefs = list(
                       list(className = 'dt-center',
                            target = 1:2)
                     )),
      rownames = F
    )
  })
  
  output$hotspot_map <- renderLeaflet({
    # if (is.null(map_data())) {
    #   return(map %>% setView(0, 0, zoom = 2))
    # }
    
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
        map_data()$points$lng[!is.na(map_data()$points$n_trials)],
        map_data()$points$lat[!is.na(map_data()$points$n_trials)],
        group = "Initial survey points",
        col = prev_pal(prev),
        radius = 2,
        popup = map_data()$points$id
      ) %>%
      
      addCircleMarkers(
        data = map_data()$result_adaptive_sf,
        group = "Recommended survey points",
        col = "green",
        radius = 3,
        popup = map_data()$result_adaptive_sf$id
      ) %>%
      
      addLegend(colors = pal(c(0, 1)),
                labels = c("Not hotspot", "Hotspot"),
                group = "Hotspot predictions") %>%
      
      addLegend(pal = prev_pal,
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
  
  # output$prob_map <- renderLeaflet({
  #   if (is.null(map_data())) {
  #     return(map %>% setView(0, 0, zoom = 2))
  #   }
  #   
  #   # Define color palette
  #   pal <-
  #     colorNumeric(wes_palette("Zissou1", 10, type = "continuous")[1:10],
  #                  seq(0, 1, 0.01))
  #   
  #   # define uncertainty
  #   uncertainty <- map_data()$result_sf$exceedance_uncertainty
  #   
  #   # map
  #   labels <- sprintf(
  #     "<strong>%s</strong><br/>Hotspot probability %g",
  #     map_data()$result_sf$id,
  #     round(map_data()$result_sf$exceedance_probability, 3)
  #   ) %>% lapply(htmltools::HTML)
  #   
  #   map %>% 
  #     
  #     addCircleMarkers(    
  #       data = map_data()$result_sf,
  #       color = pal(map_data()$result_sf$exceedance_probability),
  #       fillOpacity = 0.6,
  #       weight = 1,
  #       radius = 4) %>%
  #     
  #     addCircleMarkers(
  #       data = map_data()$result_adaptive_sf,
  #       group = "Adaptively selected survey points",
  #       col = "green",
  #       radius = 3
  #     ) %>%
  #     
  #     addLegend(
  #       colors = wes_palette("Zissou1", 10, type = "continuous")[1:10],
  #       labels = seq(0.1, 1, 0.1),
  #       title = "Hotspot probability"
  #     ) %>%
  #     
  #     addLayersControl(
  #       overlayGroups = c("Adaptively selected survey points"),
  #       options = layersControlOptions(collapsed = F)
  #     )
    
  #}) # end loading bar
  
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
      download_table <- as.data.frame(result_sf)
      #names(download_table)[2] <- "Hotspot probability" 
      write.csv(download_table, file, row.names = FALSE)
    }
  )
  
  output$downloadGeoData <- downloadHandler(
    filename = function() {
      paste("predictions.geojson")
    },
    content = function(file) {
      st_write(result_sf, file)
    }
  )
  
  output$downloadAdaptiveData <- downloadHandler(
    filename = function() {
      paste("adaptive_samples.csv")
    },
    content = function(file) {
      download_table_adaptive <- as.data.frame(result_adaptive_sf)
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


