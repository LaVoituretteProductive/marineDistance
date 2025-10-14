library(sf)
library(terra)
library(dplyr)
library(stringr)

# === 1. Chemins ===
chemin_buffers <- "C:/Users/miche/Desktop/09-marieke/res_canyon/07-port/02-donnees_travaillees/buffer_with_dist_canyon_port_mpa_stats_v2.gpkg"
chemin_rasters <- "C:/Users/miche/Desktop/09-marieke/res_canyon/07-port/01-DATA/05-test_parallel/output_cost_rasters/"
chemin_canyons <- "C:/Users/miche/Desktop/09-marieke/canyon_med.geojson"
chemin_ports   <- "C:/Users/miche/Desktop/09-marieke/res_canyon/07-port/02-donnees_travaillees/ZonePortuaire_combine_2154_area.gpkg"
chemin_mpa     <- "C:/Users/miche/Downloads/protectionMed_fr_modif_fully.gpkg"

# === 2. Charger les données ===
buffers <- st_read(chemin_buffers)
canyons <- st_read(chemin_canyons)
ports   <- st_read(chemin_ports)
mpas    <- st_read(chemin_mpa)

# S'assurer que toutes les données sont dans le même CRS
buffers <- st_transform(buffers, 2154)
canyons <- st_transform(canyons, 2154)
ports   <- st_transform(ports, 2154)
mpas    <- st_transform(mpas, 2154)

# === 3. Initialisation du résultat ===
resultats <- buffers

# === 4. Fonction pour trouver les attributs de la géométrie la plus proche ===
trouver_plus_proche <- function(buffer_geom, couche, couche_nom) {
  idx <- st_nearest_feature(buffer_geom, couche)
  dist <- st_distance(buffer_geom, couche[idx,], by_element = TRUE)
  attr <- couche[idx,] %>% st_drop_geometry()
  noms <- paste0(couche_nom, "_", names(attr))
  names(attr) <- noms
  attr[[paste0(couche_nom, "_dist_min_from_buff_m")]] <- as.numeric(dist)
  return(attr)
}

# === 5. Boucle principale ===
resultats_list <- list()

for (i in seq_len(nrow(buffers))) {
  buffer <- buffers[i,]
  buffer_id <- str_pad(i, 4, pad = "0")
  
  # Trouver le raster correspondant
  raster_path <- paste0(chemin_rasters, paste0("buffer_", buffer_id, "_accCost.tif"))
  
  if (!file.exists(raster_path)) {
    warning(paste("Raster non trouvé pour buffer", buffer_id))
    next
  }
  
  # Charger le raster
  rast_cost <- rast(raster_path)
  
  # Extraire les attributs les plus proches
  attr_canyon <- trouver_plus_proche(buffer, canyons, "canyon")
  attr_port   <- trouver_plus_proche(buffer, ports, "port")
  attr_mpa    <- trouver_plus_proche(buffer, mpas, "mpa")
  
  # Fusionner toutes les infos
  row_result <- cbind(st_drop_geometry(buffer), attr_canyon, attr_port, attr_mpa)
  resultats_list[[i]] <- row_result
}


# === 6. Fusionner tous les résultats en un seul data.frame ===
resultats_df <- bind_rows(resultats_list)

# === 7. Joindre avec les géométries originales ===
final <- left_join(buffers, resultats_df, by = "ID")  # Remplace "id" par la bonne clé si besoin

final <- st_as_sf(cbind(geometry = st_geometry(buffers), resultats_df))

# === 8. Sauvegarde ===
st_write(final, "C:/Users/miche/Desktop/09-marieke/res_canyon/07-port/02-donnees_travaillees/buffer_with_closest_feats.gpkg", delete_dsn = TRUE)
