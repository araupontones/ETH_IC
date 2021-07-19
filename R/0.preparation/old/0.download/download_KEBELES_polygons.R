#Download Kebeles polygons from hub.arcgis.com
source("set_up.R")

#'Download data from arcgis-----------------------------------------------------

  # define query to download
  GIS_query = "https://opendata.arcgis.com/datasets/98602254daac431799db5ffbf82c6547_0.geojson"
  
  # create a temporary json file where the download will be temporarily stored
  MyJsonFile <- tempfile(fileext = ".json")
  
  # Get the polygons in json format
  GIS_response = GET(GIS_query,
                     write_disk(MyJsonFile, overwrite = T))
  
  #convert the polygons to sf format
  GIS_polygons = geojsonsf::geojson_sf(MyJsonFile)
  
  
  GIS_centroids = sf::st_centroid(GIS_sf)
  
  #check that the polygons are correct
  ggplot(data = GIS_polygons) +
    geom_sf()
  
  #check that the centroids are ok
  ggplot(data = GIS_centroids) +
    geom_sf()

  names(GIS_centroids)
# Save to reference data folder -----------------------------------------------
  
  #polygons
  export(GIS_polygons, file.path(dir_data_reference_raw, "kebele_polygons_raw.rds"))
  #centroids
  export(GIS_centroids, file.path(dir_data_reference_raw, "kebele_centroids_raw.rds"))
  

# clear environment to ease space (do not do this in every script)--------------
  rm(list = ls(all.names = TRUE))

