###################
# OSM: Useful functions 
###################

# Libraries ---------------------------------------------------------------
library(pacman)
p_load(osmdata, tidyverse, sf)


# Function 1: Retrieve any object from OSM and store centroids --------------------------------------------

retrieve_amenities <- function(bbox, key, value, type="polygons") {
  ###
  # Function to retrieve any object from OSM. Use key and value from OSM. 
  # Argument "bbox" refers to a geographic bounding box. 
  # Argument 'type' can only take two options "polygons" or "points". Default is "polygons". 
  ###
  
  amenity_osm <- opq(bbox) %>%
    add_osm_feature(key = key, value = value) %>%
    osmdata_sf()
  
  if (type=="points"){
    amenity_osm_points <-amenity_osm$osm_points 
  }
  
  if (type=="polygons"){
    amenity_osm_points <-amenity_osm$osm_polygons 
  }  
  
  amenity_osm_points <- amenity_osm_points %>%
    st_centroid() 
  
  amenity_osm_points
}


# Function 2: Find distance to nearest centroid (list of OSM amenities for example) ---------------------------------------------------------------
nearest_amenity <- function(x,y) {
  ###
  #Find the nearest point of y to each point in x. In this case, x should be the dataset of properties and prices and 
  #y should be the dataset of objects from OSM. 
  ###
  
  #Indices of nearest amenity
  indices_nearest <- st_nearest_feature(x, y)
  data_nearest <- y[indices_nearest,]
  
  #Calculate distance to nearest amenities
  res <- st_distance(x, data_nearest, by_element = TRUE)
  
  #Return dataset with distances
  res
}

# Function 3: Find number of amenities in radius ---------------------------------------------------------------
amenities_radius <- function(point, y, radius = 100, crs = 3116) {
  ###
  #Find how many obs of y are in a radius of point. In this case, point should be a single geometry.
  #y should be the dataset of objects from OSM. 
  ###
  
  #Reproject for radius in meters (3116 is optimal for Bogota)
  point <- point %>%
    st_transform(crs)
  y <- y %>%
    st_transform(crs)  
  
  #Radius
  buffer <- st_buffer(point, radius)
  
  #Find objects in y within buffer
  intersection <- y %>%
    st_intersection(buffer)
  
  res <- dim(intersection)[1]
  
  #Return n
  res
}
