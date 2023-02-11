library(sp)
#data = read.csv("27-1997data.csv")
state_coords_lon_lat <- state_coords_lon_lat[!(apply(is.na(state_coords_lon_lat),1,sum)>0),]
state_coords_lon_lat <- state_coords_lon_lat[-9,]
# Setting existing coordinate as lat-long system
cord.dec = SpatialPoints(cbind(state_coords_lon_lat$lon, -state_coords_lon_lat$lat), proj4string = CRS("+proj=longlat +datum=WGS84"))

cord.dec = SpatialPoints(cbind(state_coords_lon_lat$lon, -state_coords_lon_lat$lat), proj4string = CRS("+proj=longlat +datum=WGS84"))
cord.UTM <-spTransform(cord.dec, CRS("+proj=utm +zone=51 ellps=WGS84"))

x11()
par(mfrow = c(1, 2))
plot(cord.dec, axes = TRUE, main = "Lat-Long Coordinates", cex.axis = 0.95)
plot(cord.UTM, axes = TRUE, main = "UTM Coordinates", col = "red", cex.axis = 0.95)

library(leaflet)
leaflet(state_coords_lon_lat) %>% 
  addTiles() %>% addMarkers(~lon, ~lat )
