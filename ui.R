library(leaflet)
library(lubridate)
library(geojsonio)
library(shinydashboard)

dashboardPage(
  dashboardHeader(),
  dashboardSidebar(disable = T),
  dashboardBody(
    fluidRow(
      
      includeCSS("styles.css"),
      
      box(width = 12, 
          h3("NTD mapping app"), p('This application is designed to help understand whether having village level
                                              predictions of hotspots is useful to NTD programs. Given an input of infection/sero
                                              prevalence at villages, and locations of all other villages, the app is designed
                                              to automatically fit a geospatial model (see',strong('Methods'), 'tab below) using
                                              climatological/environmenal variables (currently elevation, distance to water,
                                              temperature, precipitation and seasonality) to provide two outputs:'),
          
          br(tags$ol(
            tags$li("Location of likely hotspot villages and"),
            tags$li("Optimal locations to next visit to collect more data in
                                              order to update your hotspot prediction map")
          )),
          
          p('See', a("here", 
                     href="https://github.com/HughSt/ntd-shiny-points/blob/master/README.md"), 
            'for more detailed instructions on how to use the app and', a("here", 
                                                                          href="https://www.dropbox.com/s/t9a7o8umgufe8tw/dummy_data_hotspots.csv?dl=1"),
            'for a demo dataset of schistosomiasis in Liberia.'),
          
          column(width=12,h4("Inputs")),
          column(width=3,
                 sliderInput("threshold", "Set hotspot prevalence threshold",
                             min=0.1, max=100, value=10, round=TRUE)),
          column(width=3,sliderInput("batch_size", 
                                     "Select number of sites to adaptively select",
                                     min = 0, max= 100, value=0)),
          column(width=3,
                 fileInput("File", "Points")),
          
          column(width=3, actionButton("get_predictions", "Get results"))
          
      ),
      
      
      tabBox(width = 12, height = 1800,
             tabPanel(title = strong("Hotspots"), width = 12, 
                      
                      box(
                        column(
                          p('The', strong('Hotspots'), 'tab allows hotspot villages to be identified by choosing the predicted 
                                probability that a village is a hotspot (i.e. where infection/sero prevalence 
                               is greater than the threshold set using the slider above). For example, if the slider is at 50%, the map will show all those
                               villages where the probability the village is a hotspot is at least 50%. For a more conservative
                               estimate of hotspots, a lower threshold can be used. For example, a program might be willing to 
                               classify a village as a hotspot if the model is only 30% sure the village is actually a hotspot. 
                               In that case, the slider should be moved to 30% and the map and table will update.'), width = 8),
                        
                        column(sliderInput("prob_threshold", 
                                           "Select areas where the probability of being a hotspot is at least",
                                           min = 0, max= 100, value=50, post = "%"), width = 4),
                        
                        width = 12),
                      
                      
                      box(leafletOutput("hotspot_map", height = 700, width="100%"), width=12),
                      
                      # box(DT::DTOutput('hotspot_table'), 
                      #     conditionalPanel(condition = "input.get_predictions > 0",
                      #                             downloadButton("downloadData", "Download as table"),
                      #                             downloadButton("downloadGeoData", "Download as GeoJSON")),
                      #     width = 6), 
                      box(DT::DTOutput('pred_table'), 
                          conditionalPanel(condition = "input.get_predictions > 0",
                                           downloadButton("downloadData", "Download as table"),
                                           downloadButton("downloadGeoData", "Download as GeoJSON")),
                          width = 12)),
             
             
             tabPanel(title = strong("Methods"),
                      
                      box(h4(p("UNDER CONSTRUCTION")),
                          
                          p(br('The outputs shown in the', strong('Hotspots'), 'tab come from a geospatial model. The model works by
                                       characterizing the relationship between the observed prevalence values (numbers positive/numbers tested) 
                                       at each location and the climatological/enviromnetal conditions (long term rainfall, temperature, seasonality,
                                       elevation and distance to water) at those locations. For example, if we were just to 
                                       use temperature and elevation, the model would establish what, if any, relationship there is between prevalence
                                       and these two variables. For example, the model might find that higher prevalence values are found in warmer 
                                       areas with lower elevation. Once the model has quantified
                                       these relationships, it is possible to predict prevalence anywhere where we know elevation and temperature.'),
                            
                            br('Instead of just making a best guess as to what prevalence is at any given location, the model gives us a 
                                       range or distribution of possible values prevalence could take at every location. Where the model is very certain, 
                                       this distribution will be narrow. Where the model is less certain, the distribution will be large. 
                                       This distribution also allows us to estimate the probability
                                       that prevalence is above or below a certain value. We can do this by looking at what proportion of the distribution 
                                       is above that threshold value. This is a nice way of using predictions as it allows us to incorporate the
                                       model uncertainty when interpreting them. Below is an example of this concept. The plot shows a the distribution of prevalence values a single village
                                       could have. In this example, the model predicts that prevalence at this village could be between 0 - 25%.
                                       The height of the distribution shows us the relative probability of a prevalence value, e.g. in this case prevalence 
                                       of 10% is more likely that a prevalence of 5%. By moving the slider, you can estimate the probability that 
                                       prevalence exceeds that value.'),
                            
                            br('For the technical audience - we use the framework of stacked generalization. First, 10 fold cross validated predictions
                                         are generated from a random forest model, modeling the binomial outcome at each site against the set of covariates listed above.
                                         These cross-validated predictions are then included as a covariate in a spatial model using 
                                         a low-rank Gaussian process smooth fit using the R package mgcv.
                                         Realizations at every locations are then generated using conditional simulation in order to build a posterior 
                                         distribution of prevalence values at each location.'),
                            
                            
                            br(column(width=1,""),
                               column(width=9, 
                                      offest=1,
                                      sliderInput("post_threshold", "Set hotspot prevalence threshold", min=0, max=20, value=10))),
                            plotOutput("posterior"),
                            width = 12),
                          
                          br(br(br('We can also use the uncertainty estimates to identify where the model is least sure about whether 
                                       that location is a hotspot or not. In situations where field teams will be conducting more surveys,
                                       it makes sense to direct teams to those locations in order to improve the hotspot map. Computerized 
                                       simulations have shown that selecting survey locations in this way, as opposed to randomly, allows 
                                       the geospatial models to perform better, improving the certainty we have in the predictions.'))),
                          width = 12)
             )
             
      )
    )
    
  )
)


