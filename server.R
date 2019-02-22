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

source("buff_voronoi.R")

# Define map

map <- leaflet(max) %>%
  addTiles(
    "https://cartodb-basemaps-{s}.global.ssl.fastly.net/light_all/{z}/{x}/{y}{r}.png"
  )

shinyServer(function(input, output) {
  map_data <- reactive({
    inFile <- input$File
    inFile_pred <- input$predFile
    if (is.null(inFile))
      return(NULL)
    
    if (is.null(inFile_pred))
      return(NULL)
    
    
    # Give loading bar
    withProgress(message = 'Hold on',
                 detail = 'Crunching data..',
                 value = 5,
                 {
                   points <- read.csv(inFile$datapath)
                   pred_points <- read.csv(inFile_pred$datapath)
                   
                   # If pred_points IDs are not unique, then make them
                   if(length(unique(pred_points$ID))!=nrow(pred_points)){
                     
                     pred_points$ID <- paste0(pred_points$ID, 1:nrow(pred_points))
                   }
                   
                   # Check for any missing data in survey data
                   if(sum(!complete.cases(points))>0){
                  
                     showNotification(paste("Removed", sum(!complete.cases(points)), "survey points with missing data"))
                     points <- points[complete.cases(points),]
                   }
                   
                   if(sum(points$Nex==0)>0){
                     
                     showNotification(paste("Removed", sum(points$Nex==0), "survey points with 0 individuals examined"))
                     points <- points[-which(points$Nex==0),]
                   }
                   
                   #Check for missing or duplicate coords in pred data
                   if(sum(is.na(pred_points$lng))>0){
                     
                     showNotification(paste("Removed", sum(is.na(pred_points$lng)), "prediction points with missing coordinates"))
                     pred_points <- pred_points[complete.cases(pred_points),]
                   }
                   
                   dups <- dup.coords(pred_points[,c("lng", "lat")])
                   if(length(dups)>0){
                     
                     drop <- unlist(sapply(dups, function(x){as.numeric(x[-1])}))
                     pred_points <- pred_points[-drop,]
                     
                     showNotification(paste("Removed", length(drop), "prediction points with duplicate coordinates"))
                     
                   }
                   
                   
                   # Prepare input as JSON
                   input_data_list <-
                     list(
                       region_definition = list(
                         lng = pred_points$lng,
                         lat = pred_points$lat,
                         id = pred_points$ID
                       ),
                       train_data = list(
                         lng = points$lng,
                         lat = points$lat,
                         n_trials = points$Nex,
                         n_positive = points$Npos
                       ),
                       request_parameters = list(threshold = input$threshold /
                                                   100)
                     )
                   
                   # Make call to algorithm
                   print("Making request")
                   
                   response <-  httr::POST(url = "http://srv.tmpry.com:8080/function/fn-hotspot-gears_0-0-2",
                                           body = toJSON(input_data_list),
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
                   
                   result <<-
                     rjson::fromJSON(json_response) # this will put the response in a useful format
                   
                   # Create buffered polygons
                   #st_geometry(pred_points) <- st_geometry(st_as_sf(pred_points,
                  #                                                  coords = c("lng", "lat")))
                   
                   sp_Polygons <-
                     # buff_voronoi_test(
                     #   data.frame(
                     #     x = pred_points$lng,
                     #     y = pred_points$lat,
                     #     id = pred_points$ID
                     #   ),
                     #   w_buff = 0.15
                     # )
                     SpatialPointsDataFrame(pred_points[,c("lng", "lat")],
                                            data.frame(id = pred_points$ID))
                     #sp_Polygons <- st_buffer(sp_Polygons, 0.1)
                   
                   
                   # create spdf
                   spdf_data <-
                     data.frame(
                       probability = result$estimates$exceedance_prob,
                       id = result$estimates$id,
                       class = result$estimates$category
                     )
                   
                   # Merge
                   sp_Polygons_df <- merge(sp_Polygons, spdf_data, by="id")
                   
                   return(
                     list(
                       points = points,
                       pred_points = pred_points,
                       #sp_Polygons = sp_Polygons,
                       #spdf_data = spdf_data
                       sp_Polygons_df = sp_Polygons_df
                     )
                   )
                 })
  })
  
  output$pred_table <- DT::renderDT({
    if (is.null(map_data())) {
      return(NULL)
    }
    uncertainty <- abs(map_data()$sp_Polygons_df$probability - 0.5)
    output_table <-
      map_data()$sp_Polygons_df@data[order(uncertainty),][1:5, 1:2]
    output_table[, 2] <- round(output_table[, 2], 2)
    names(output_table) <-
      c("Village ID", "Probability of being a hotspot")
    DT::datatable(output_table,
                  options = list(pageLength = 15),
                  rownames = F)
  })
  
  output$hotspot_table <- DT::renderDT({
    if (is.null(map_data())) {
      return(NULL)
    }
    hotspot_index <-
      which(map_data()$sp_Polygons_df$probability >= input$prob_threshold / 100)
    hotspot_table <- map_data()$sp_Polygons_df@data[hotspot_index, 1:2]
    hotspot_table[, 2] <- round(hotspot_table[, 2], 2)
    names(hotspot_table) <-
      c("Village ID", "Probability of being a hotspot")
    DT::datatable(
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
    if (is.null(map_data())) {
      return(map %>% setView(0, 0, zoom = 2))
    }
    
    # Define color palette
    pal <-
      colorNumeric(wes_palette("Zissou1", 10, type = "continuous")[1:10],
                   seq(0, 1, 0.01))
    
    labels <- sprintf(
      "<strong>%s</strong><br/>Hotspot probability %g",
      map_data()$sp_Polygons_df$id,
      round(map_data()$sp_Polygons_df$probability, 3)
    ) %>% lapply(htmltools::HTML)
    
    # Map
    hotspot_class <-
      ifelse(map_data()$sp_Polygons_df$probability >= input$prob_threshold / 100,
             1,
             0)
    map %>% 
      
    #   addPolygons(
    #   data = map_data()$sp_Polygons_df,
    #   color = pal(hotspot_class),
    #   fillOpacity = 0.6,
    #   weight = 1,
    #   highlightOptions = highlightOptions(
    #     weight = 5,
    #     color = "#666",
    #     bringToFront = TRUE,
    #     fillOpacity = 0.7
    #   ),
    #   label = labels
    # ) %>%
    
    addCircleMarkers(    
        data = map_data()$sp_Polygons_df,
        color = pal(hotspot_class),
        fillOpacity = 0.6,
        weight = 1,
        radius = 4) %>%
      
      addCircleMarkers(
        map_data()$points$lng,
        map_data()$points$lat,
        group = "Survey points",
        col = "black",
        radius = 2
      ) %>%
      
      addLegend(colors = pal(c(0, 1)),
                labels = c("Not hotspot", "Hotspot")) %>%
      
      addLayersControl(overlayGroups = c("Survey points"),
                       options = layersControlOptions(collapsed = F))
  })
  
  output$prob_map <- renderLeaflet({
    if (is.null(map_data())) {
      return(map %>% setView(0, 0, zoom = 2))
    }
    
    # Define color palette
    pal <-
      colorNumeric(wes_palette("Zissou1", 10, type = "continuous")[1:10],
                   seq(0, 1, 0.01))
    
    # define uncertainty
    uncertainty <- abs(map_data()$sp_Polygons_df$probability - 0.5)
    
    # map
    labels <- sprintf(
      "<strong>%s</strong><br/>Hotspot probability %g",
      map_data()$sp_Polygons_df$id,
      round(map_data()$sp_Polygons_df$probability, 3)
    ) %>% lapply(htmltools::HTML)
    
    map %>% 
    #   addPolygons(
    #   data = map_data()$sp_Polygons_df,
    #   color = pal(map_data()$sp_Polygons_df$probability),
    #   fillOpacity = 0.6,
    #   weight = 1,
    #   highlightOptions = highlightOptions(
    #     weight = 5,
    #     color = "#666",
    #     bringToFront = TRUE,
    #     fillOpacity = 0.7
    #   ),
    #   label = labels
    # ) %>%
      
      # addPolygons(
      #   data = map_data()$sp_Polygons_df[order(uncertainty)[1:5],],
      #   col = "deeppink",
      #   opacity = 1,
      #   fillOpacity = 0.1,
      #   group = "Villages to sample",
      #   highlightOptions = highlightOptions(
      #     weight = 5,
      #     color = "#666",
      #     bringToFront = TRUE,
      #     fillOpacity = 0.7
      #   ),
      #   label = labels[order(uncertainty)[1:5]]
      # ) %>%
      
      addCircleMarkers(    
        data = map_data()$sp_Polygons_df,
        color = pal(map_data()$sp_Polygons_df$probability),
        fillOpacity = 0.6,
        weight = 1,
        radius = 4) %>%
      
      addCircleMarkers(
        map_data()$points$lng,
        map_data()$points$lat,
        # popup = paste0("<p><strong>Name: </strong>", map_data()$points$ID,
        #                                        "<br><strong>Prevalence </strong>",
        #                                        c(map_data()$points$Npos / map_data()$points$Nex),
        #                                        "<br><strong>N = </strong>",
        #                                        map_data()$points$Nex),
        group = "Survey points",
        col = "black",
        radius = 2
      ) %>%
      
      addLegend(
        colors = wes_palette("Zissou1", 10, type = "continuous")[1:10],
        labels = seq(0.1, 1, 0.1),
        title = "Hotspot probability"
      ) %>%
      
      addLayersControl(
        overlayGroups = c("Villages to sample", "Survey points"),
        options = layersControlOptions(collapsed = F)
      )
    
  }) # end loading bar
  
  output$posterior <- renderPlot({

    
    threshold <- input$post_threshold
    
    set.seed(1981)
    sample <- rbinom(500, 100, 0.10)
    binom <- density(sample,0.7)
    binom <- data.frame(x=binom$x, y=binom$y)
    plot(binom$x, binom$y, type="l", lwd=4, axes=F, cex.lab=1.5,
         xlab="Infection prevalence (%)",ylab="Relative probability")
    axis(1,xlim=c(0,25))
    text(17,0.11, paste0("Probability that prevalence exceeds ", 
                       threshold,"% is ", round(mean(sample>=threshold),2)),cex=1.5)
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
  
  #output$Instructions <- textOutput("File with 'lng' and 'lat' columns")
  
  output$EE_logo <- renderImage({
    # Return a list containing the filename
    list(src = "GoogleEarthEngine_logo.png")
  }, deleteFile = FALSE)
  
})
