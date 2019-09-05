

# Run checks on input
function(points){
  
  # Check correct columns are present
  columns <- c("id", "n_trials", "n_positive")
  present <- columns %in% names(points)
  if(sum(present)<3){
    stop(paste("Point data does not contain required", columns[!present], "fields "))
  }
  
      if(length(unique(points$id))!=nrow(points)){
        
        points$id <- paste0(points$id, "_", 1:nrow(points))
        showNotification(paste("Renamed IDs as duplicates were found"))
      }
      
      
      if(sum(points$n_trials==0, na.rm=T)>0){
        
        showNotification(paste("Removed", sum(points$n_trials==0), "survey points with 0 individuals examined"))
        points <- points[-which(points$n_trials==0),]
      }
      
      #Check for missing or duplicate coords in pred data
      if(sum(is.na(points$lng))>0){
        
        showNotification(paste("Removed", sum(is.na(points$lng)), "points with missing coordinates"))
        points <- points[complete.cases(points$lng),]
      }
      
      dups <- dup.coords(points[,c("lng", "lat")])
      if(length(dups)>0){
        
        drop <- unlist(sapply(dups, function(x){as.numeric(x[-1])}))
        points <- points[-drop,]
        
        showNotification(paste("Removed", length(drop), "points with duplicate coordinates"))
        
      }
      
      return(points)


}