## Hotspot prediction and adaptive sampling prototype tool
This application is designed to help understand whether having village level predictions of hotspots is useful to NTD programs. Given an input of infection/sero prevalence at villages, and locations of all other villages, the app is designed to automatically fit a geospatial model using climatological/environmental variables (currently elevation, distance to water, temperature, precipitation and seasonality) to provide two outputs:

- Location of likely hotspot villages (probability that prevalence is greater than a user defined prevalence threshold) and
- Optimal locations to next visit to collect more data in order to update your hotspot prediction map. Note that these are not the locations most likely to be hotspots, rather they are the places we are least certain about whether they are a hotspot or not. 

To use the app, you need to provide a .csv file of point level data (e.g. villages or schools) containing at least the following columns (demo data provided using the link). 

- `id` - the ID code of the point (can be character or numeric)
- `lat` - the latitude in decimal degrees
- `lng` - the longitude in decimal degrees
- `n_trials` - the number of people examined/tested 
- `n_positive` - the number of people testing positive

You also need to set 2 parameters: 

**Set hotspot prevalence threshold** - This sets the prevalence threshold that defines a hotspot. For example, a location might be considered a hotspot if prevalence of infection is >10%. In which case, set this threshold to 10%. 

**Select number of sites to adaptively select** - If you are using this tool to obtain recommendations on the optimal locations to conduct further surveys, this sets the number of sites you wish to receive recommendations for. 

The application will make predictions at each location provided in the .csv file. If you wish to obtain predictions (preicted prevalence, probability of being a hotspot) at unsurveyed locations, you can these locations in the .csv file, leaving the n_trials and n_positive columns blank. An example of the expected input file can be found [here](https://www.dropbox.com/s/l6t9cx51805to7n/Sh_liberia_withNA.csv?dl=1).

Once you hit `Get results`, the application will find the necessary climatological/environmental layers, fit a model and make predictions. These are then shown on the map and tables below. Please note that the first time the app is run for a country, the app takes a while to gather the necessary data. Subsequent calls of the algorithm will be much quicker.

The map shows 3 layers:

1. **Initial survey points** - The original survey data, colored by prevalence
2. **Hotspot predictions** - Predictions at all locations (surveyed and unsurveyed) of hotspot class (hotspot or not hotspot). By default the app classifies places as hotspots if there is a more than 50% chance it is a hotspot. If you wish to be more conservative about classifying a place as a hotspot, you can move the slider titled **Select areas where the probability of being a hotspot is at least** down. 
3. **Recommended survey points** - The locations of recommendated sites to conduct further surveys to improve hotspot predicitions. 

There are also 2 tables beneath the map showing 'Hotspot predictions' and 'Recommended survey points'. 

Results can be downloaded either as tables or a GoeJSON which can be used in GIS/mapping software. 

**WARNING:** You should check for any outliers, both in terms of erroneous prevalence values and erroneous longitude/latitude, before using the app. You should also be confident that your survey data are representative of the underlying population to avoid misleading results. For example, often survey data are obtained from purposefully sampled locations of high expected prevalence. Using such data will bias results and predictions. Similarly, any supplied unsurveyed locations to predict to should be from the same population. Providing locations in neighbouring areas from which no survey data are available may result in unrealistic predictions.  

