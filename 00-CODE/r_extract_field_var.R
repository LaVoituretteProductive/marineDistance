library(sf)
library(terra)
library(dplyr)
library(stringr)

setwd("M:/BiodivMed/Distance_to_port_MPA_canyon/")

chemin_buffers <- "./02-MID/buffer_with_dist_canyon_port_mpa_stats.gpkg"
chemin_rasters <- "./output_cost_rasters/"
chemin_canyons <- "./01-DATA/canyon_med.geojson"
chemin_ports   <- "./01-DATA/ZonePortuaire_combine_2154_area.gpkg"
chemin_mpa     <- "./01-DATA/protectionMed_fr_modif_fully.gpkg"

buffers <- st_read(chemin_buffers)
canyons <- st_read(chemin_canyons)
ports   <- st_read(chemin_ports)
mpas    <- st_read(chemin_mpa)

buffers <- st_transform(buffers, 2154)
canyons <- st_transform(canyons, 2154)
ports   <- st_transform(ports, 2154)
mpas    <- st_transform(mpas, 2154)

resultats <- buffers

trouver_plus_proche <- function(buffer_geom, couche, couche_nom) {
  idx <- st_nearest_feature(buffer_geom, couche)
  dist <- st_distance(buffer_geom, couche[idx,], by_element = TRUE)
  attr <- couche[idx,] %>% st_drop_geometry()
  noms <- paste0(couche_nom, "_", names(attr))
  names(attr) <- noms
  attr[[paste0(couche_nom, "_dist_min_from_buff_m")]] <- as.numeric(dist)
  return(attr)
}

resultats_list <- list()

for (i in seq_len(nrow(buffers))) {
  buffer <- buffers[i,]
  buffer_id <- str_pad(i, 4, pad = "0")
  
  raster_path <- paste0(chemin_rasters, paste0("buffer_", buffer_id, "_accCost.tif"))
  
  if (!file.exists(raster_path)) {
    warning(paste("Raster non trouvé pour buffer", buffer_id))
    next
  }
  
  rast_cost <- rast(raster_path)
  
  attr_canyon <- trouver_plus_proche(buffer, canyons, "canyon")
  attr_port   <- trouver_plus_proche(buffer, ports, "port")
  attr_mpa    <- trouver_plus_proche(buffer, mpas, "mpa")
  
  row_result <- cbind(st_drop_geometry(buffer), attr_canyon, attr_port, attr_mpa)
  resultats_list[[i]] <- row_result
}


resultats_df <- bind_rows(resultats_list)

final <- left_join(buffers, resultats_df, by = "ID")  # Remplace "id" par la bonne clé si besoin

final <- st_as_sf(cbind(geometry = st_geometry(buffers), resultats_df))

st_write(final, "./01-RES/buffer_with_closest_feats.gpkg", delete_dsn = TRUE)
