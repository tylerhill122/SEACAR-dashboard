# Sample Locations
point <- st_read(paste0(seacar_shape_location,"SampleLocations12dec2023/seacar_dbo_vw_SampleLocation_Point.shp"))

# ORCP boundaries
orcp_shp <- st_read(paste0(seacar_shape_location, "orcp_all_sites/ORCP_Managed_Areas.shp")) %>%
  st_transform('+proj=longlat +datum=WGS84')

###############
## FUNCTIONS ##
###############

# Allows location of shapefile for each MA
find_shape <- function(ma){
  shape_file <- orcp_shp %>% filter(LONG_NAME==ma)
  return(shape_file)
}

get_shape_coordinates <- function(ma_shape){
  maxmin <- st_bbox(st_geometry(ma_shape))
  return(maxmin)
}