
data <- read.csv("E:/Box/Box Sync/SOM_tarball/tarball_10-18-2018.csv", header=TRUE, stringsAsFactors = FALSE)

coords <- data[,c(18,12:14)]

#how many unique coords do we have?
lat.uniq <- unique(data$lat)
long.uniq <- unique(data$long)


#Cut down to only unique values, then use output as lookup table later
coords <- unique(coords)

library(measurements) #provides coordinate conversions

    #example

    # x = read.table(text = "
    #    lat     long
    # 105252 30°25.264 9°01.331
    
    # change the degree symbol to a space
    # x$lat = gsub('°', ' ', x$lat)
    # x$long = gsub('°', ' ', x$long)
    
    # convert from decimal minutes to decimal degrees
    # x$lat = measurements::conv_unit(x$lat, from = 'deg_dec_min', to = 'dec_deg')
    # x$long = measurements::conv_unit(x$long, from = 'deg_dec_min', to = 'dec_deg')

###Clean up lat-longs

#Remove 'W" and Â from longitudes
coords$long <- gsub("W","",coords$long)
coords$long <- gsub("Â","",coords$long)
coords$long <- gsub("'","",coords$long)
coords$long <- gsub("\"","",coords$long)
coords$long <- gsub(" ","",coords$long)
coords$long <- gsub("°$","",coords$long)

#Remove 'N" and Â from longitudes
coords$lat <- gsub("N","",coords$lat)
coords$lat <- gsub("Â","",coords$lat)
coords$lat <- gsub("'","",coords$lat)
coords$lat <- gsub("\"","",coords$lat)
coords$lat <- gsub(" ","",coords$lat)
coords$lat <- gsub("°$","",coords$lat)  #Does this work? Trying to conditional replace if ° is last element in string


#Export to csv (maybe faster to just fix this thing manually...)
#write.csv(coords,"SOM_coords.csv")

conv.coords <- NULL

#Loop through each coordinate value
for(i in 1:nrow(coords)){

  #Open loop storage variables
  df <- NULL
  lat <- 0
  long <- 0
  stor.lat <- 0
  stor.long <- 0

  #Check if latitude string is all numeric
  if(!is.na(as.numeric(coords$lat[i]))){

    #If value within proper range for decimal degrees, pass on
    if(abs(as.numeric(coords$lat[i])<70)) {lat <- abs(as.numeric(coords$lat[i]))}
    
    #If value matches longitude instead of latitude, add SWAP flag
    if(abs(as.numeric(coords$lat[i]))>70 & abs(as.numeric(coords$lat[i]))<170) {lat <- paste0(as.character(coords$lat[i]),"SWAP")}
    
    #If value outside range, add flag to string
    if(abs(as.numeric(coords$lat[i]))>170) {lat <- paste0(as.character(coords$lat[i]),"_FLAG")}
  }


  #Check if longitude string is all numeric
  if(!is.na(as.numeric(coords$long[i]))){
    
    #If value within proper range for decimal degrees, pass on
    if(abs(as.numeric(coords$long[i])<170)) {long <- abs(as.numeric(coords$long[i]))}
    
    #If value outside range, add flag to string
    if(abs(as.numeric(coords$long[i]))>170) {long <- paste0(as.character(coords$long[i]),"_FLAG")}
  }

  #Swap lat-long if mismatched
  #if(grepl("SWAP",lat)) {
  #  lat <- gsub("SWAP","",lat)
  #  stor.lat <- lat
  #  lat <- long
  #  long <- stor.lat
  #}

  #If string matches format for DMS, convert and pass on
  if(grepl("°",coords$lat[i])) {
    
    #Fix coord format spacing
    stor.lat <- gsub("°"," ",coords$lat[i])
    
    #Convert lat from DMS to DD
    lat <- conv_unit(stor.lat, from = 'deg_dec_min', to = 'dec_deg')
  }

  #If string matches format for DMS, convert and pass on
  if(grepl("°",coords$long[i])) {
    
    #Fix coord format spacing
    stor.long <- gsub("°"," ",coords$long[i])
    
    #Convert long from DMS to DD
    long <- as.numeric(measurements::conv_unit(stor.long, from = 'deg_dec_min', to = 'dec_deg'))
  }


  #If none of the above, add flag to string to add conditional fix
  if(lat == 0){lat <- paste0(coords$lat[i],"_FLAG")}
  if(long == 0){long <- paste0(coords$long[i],"_FLAG")}

  df <- data.frame(i,coords$lat[i], coords$long[i], stor.lat, stor.long, lat, long)

  conv.coords <- rbind(conv.coords,df)
}


#Look at the results
conv.coords$lat
conv.coords$long

#Look at inputs
conv.coords[,2]
conv.coords[,3]
