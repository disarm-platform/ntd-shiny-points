library(leaflet)
library(lubridate)
library(shinyBS)


library(shinydashboard)

dashboardPage(
  dashboardHeader(),
  dashboardSidebar(disable = T),
  dashboardBody(
    fluidRow(
      
      includeCSS("styles.css"),

             box(width = 12, 
                    h3("NTD mapping app"), h4(p('This application is designed to help understand whether having village level
                                              predictions of hotspots is useful to NTD programs. Given an input of infection/sero 
                                              prevalence at villages, and locations of all other villages, the app is designed 
                                              to automatically fit a geospatial model (see',strong('Methods'), 'tab below) using 
                                              climatological variables (currently 
                                              temperature, precipitation and seasonality) to provide two outputs:'),
                                              
                                              br(tags$ol(
                                                tags$li("Location of likely hotspot villages and"), 
                                                tags$li("optimal location to next visit to collect more data in
                                              order to update your hotspot prediction map")
                                              )),
                                              
                                              
                                              p("To test the app, you can download the 
                                              demo files below to see their structure and then upload using the upload box. 
                                              It just needs 2 files:"),
                                            
                                            br(tags$ol(
                                                 tags$li("Survey data (in this case",tags$i("S. mansoni"), "infection prevalence in Cote D'Ivoire)."), 
                                                 tags$li("Locations of all other villages")
                                               )),
                                            
                                            p('Or you can run using your own data, as long as
                                              they are in the same format as the demo data. You also need to set the prevalence threshold to define 
                                              a hotspot.'),
                 
                                            br('Once the data are uploaded, the two tabs below show the two outputs. The', strong('Hotspots'), 'tab
                                              allows hotspot villages to be identified. The ', strong('Adaptive sampling'), 
                                              'tab provides guidance on where to survey next in order to survey 
                                              a village that will provide the most valuable data.')),
                 
                                             
                 
                 helpText(a("Demo survey data",     
                            href="https://www.dropbox.com/s/dxpdwvqez2pvszm/Sm_cdi_observations.csv?dl=1"),
                          target="_blank"), 
                 
                 helpText(a("Demo village data",     
                            href="https://www.dropbox.com/s/tn4lmpvlgubtrey/Sm_cdi_villages.csv?dl=1"),
                          target="_blank"), 
                 
                        sliderInput("threshold", "Set hotspot prevalence threshold",
                                    min=0, max=100, value=10, round=TRUE, width = 500),
                 
                        fileInput("File", "Survey data", width = "20%"),
                        fileInput("predFile", "Villages", width = "20%")),
            
             
             tabBox(width = 12, height = 1200,
                    tabPanel(title = strong("Hotspots"), width = 12, 
                             
                             box(
                                h4(p('The', strong('Hotspots'), 'tab allows hotspot villages to be identified by choosing the predicted 
                                probability that a village is a hotspot (i.e. where infection/sero prevalence 
                               is greater than the threshold set using the slider above). For example, if the slider is at 50%, the map will show all those
                               villages where the probability the village is a hotspot is at least 50%. For a more conservative
                               estimate of hotspots, a lower threshold can be used. For example, a program might be willing to 
                               classify a village as a hotspot if the model is only 30% sure the village is actually a hotspot. 
                               In that case, the slider should be moved to 30% and the map and table will update.')), width = 12),
                             
                             
                             box(leafletOutput("hotspot_map", height = 800), width = 8),
                             box(sliderInput("prob_threshold", 
                                             "Select areas where the probability of being a hotspot is at least",
                                             min = 0, max= 100, value=50, post = "%"), width = 3),
                             box(DT::DTOutput('hotspot_table'), width = 3)),
                    
                    tabPanel(title = strong("Adaptive sampling"),

                              box(h4(p('The ', strong('Adaptive sampling'), 'tab provides guidance on where to survey next in order to survey 
                                              a village that will provide the most valuable data. In this case, the village at which the 
                               algorithm is least certain about whether it is a hotspot or not is the most sensible location
                               to collect more data. Research has shown that this type of adaptive sampling improves predictions 
                                from the geospatial model.
                                Rather than identifying the single most valuable village to visit, the 
                               application provides 5 village to choose from. Once data at one of these 5 villages is collected
                               the application can be updated and the hotspot and adaptive sampling maps will update.')), width = 12),
                             
                             box(leafletOutput("prob_map", height = 800), width = 8),
                             box(DT::DTOutput('pred_table'), width = 3)),
                    
                    tabPanel(title = strong("Methods"),
                             
                             box(h4(p("UNDER CONSTRUCTION"),
                                    
                                    br('The outputs shown in the', strong('Hotspots'), 'tab and the',
                                       strong('Adaptive sampling'), 'tab both come from the same geospatial model. The model works by
                                       characterizing the relationship between the observed prevalence values (numbers positive/numbers tested) 
                                       at each location and the climatological/enviromnetal conditions at those locations. For example, if we were just to 
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
                                    

                                 sliderInput("post_threshold", "Set hotspot prevalence threshold", min=0, max=20, value=10),
                                 plotOutput("posterior"),
                                 width = 12),
                             
                             br(h4('We can also use the uncertainty estimates to identify where the model is least sure about whether 
                                        that location is a hotspot or not. In situations where field teams will be conducting more surveys,
                                       it makes sense to direct teams to those locations in order to improve the hotspot map. Computerized 
                                       simulations have shown that selecting survey locations in this way, as opposed to randomly, allows 
                                       the geospatial models to perform better, improving the certainty we have in the predictions.')),
                             width = 12)
                    )
                    
             )
             )

      )
)
      

