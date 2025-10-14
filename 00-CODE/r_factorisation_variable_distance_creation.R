library(gdistance)
library(raster)
library(sf)
library(terra)
library(dplyr)

# -------------------------------------------------
# ---------------- FONCTIONS ----------------------
# -------------------------------------------------

charger_vecteurs <- function(chemin_mer, chemin_transects, chemin_zones, crs_cible) {
  vect_mer <- st_read(chemin_mer) |> st_transform(crs_cible)
  buffers  <- st_read(chemin_transects) |> st_transform(crs_cible)
  zones    <- st_read(chemin_zones) |> st_transform(crs_cible)
  list(mer = vect_mer, buffers = buffers, zones = zones)
}

creer_raster_friction <- function(vect_mer, resolution = 100) {
  vect_mer$friction <- 1
  r_template <- rast(ext(vect(vect_mer)), resolution = resolution, crs = crs(vect(vect_mer)))
  rasterize(vect(vect_mer), r_template, field = "friction", background = NA)
}

rasteriser_objets <- function(objets, raster_template) {
  lapply(objets, function(obj) rasterize(obj, raster_template))
}

creer_matrice_transition <- function(r_friction) {
  friction_raster <- raster(r_friction)
  message("Début création matrice bip bip")
  print(now())
  tr <- transition(friction_raster, function(x) 1 / mean(x), directions = 8)
  print(now())
  message("Fin création matrice bip bip")
  
  message("Début géocorrection bip bip")
  tr_geo <- geoCorrection(tr, type = "c")
  print(now())
  message("Fin géocorrection bip bip")
  
  tr_geo
}

creer_raster_cout_depuis_tr_geo <- function(tr_geo, coords_sources) {
  accCost(tr_geo, SpatialPoints(coords_sources))
}

extraire_valeurs <- function(raster_cout, vecteur) {
  terra::extract(raster_cout, vect(vecteur), weights = TRUE)
}

calculer_stats_distance <- function(valData, buffers) {
  val_col <- names(valData)[2]
  valData <- valData %>%
    mutate(value = .data[[val_col]]) %>%
    mutate(value = ifelse(value < 0 | is.infinite(value), NA, value)) %>%
    filter(!is.na(value))
  
  stopifnot("weight" %in% names(valData))
  
  min_values  <- aggregate(valData$value, by = list(ID = valData$ID), FUN = min, na.rm = TRUE)
  max_values  <- aggregate(valData$value, by = list(ID = valData$ID), FUN = max, na.rm = TRUE)
  mean_values <- aggregate(valData$value, by = list(ID = valData$ID), FUN = mean, na.rm = TRUE)
  
  range_values <- left_join(max_values, min_values, by = "ID") %>%
    mutate(dist_port_m_range = .[[2]] - .[[3]]) %>%
    select(ID, dist_port_m_range)
  
  weighted_means <- valData %>%
    group_by(ID) %>%
    summarise(
      dist_port_m_weighted = if (all(is.na(weight)) || sum(weight, na.rm = TRUE) == 0) {
        NA_real_
      } else {
        sum(value * weight, na.rm = TRUE) / sum(weight, na.rm = TRUE)
      },
      .groups = "drop"
    )
  
  full_ids <- data.frame(ID = seq_len(nrow(buffers)))
  summary_values <- full_ids %>%
    left_join(min_values, by = "ID") %>%
    left_join(max_values, by = "ID") %>%
    left_join(mean_values, by = "ID") %>%
    left_join(range_values, by = "ID") %>%
    left_join(weighted_means, by = "ID")
  
  names(summary_values)[2:6] <- c("dist_m_min", "dist_m_max", "dist_m_mean", "dist_m_range", "dist_m_weight")
  summary_values
}

ajouter_stats_a_buffers <- function(buffers, stats_df, prefix) {
  buffers$ID <- seq_len(nrow(buffers))
  buffers <- buffers[, c("ID", setdiff(names(buffers), "ID"))]
  
  colnames(stats_df)[-1] <- paste0(prefix, "_", colnames(stats_df)[-1])
  
  left_join(buffers, stats_df, by = "ID")
}

# -------------------------------------------------
# ---------------- SCRIPT PRINCIPAL ---------------
# -------------------------------------------------

# --- Paramètres
target_crs <- "EPSG:2154"
chemin_mer      <- "C:/Users/miche/Desktop/09-marieke/res_canyon/07-port/02-donnees_travaillees/inverse_poly.gpkg"
chemin_transects <- "C:/Users/miche/Downloads/mtdt_6.gpkg"
chemin_canyons   <- "C:/Users/miche/Desktop/09-marieke/canyon_med.geojson"
chemin_ports     <- "C:/Users/miche/Desktop/09-marieke/res_canyon/07-port/02-donnees_travaillees/ZonePortuaire_combine_2154_area.gpkg"
chemin_mpa <- "C:/Users/miche/Downloads/protectionMed_fr_modif_fully.gpkg"

# --- Chargement & friction
layers <- charger_vecteurs(chemin_mer, chemin_transects, chemin_canyons, target_crs)
r_friction <- creer_raster_friction(layers$mer)

# --- Création matrice de coût une fois pour toutes
tr_geo <- creer_matrice_transition(r_friction)

# --- Fonction de traitement général pour une couche (canyons ou ports)
traiter_distance <- function(zone_path, tr_geo, buffers, crs_cible, prefix, raster_export_path = NULL) {
  zone <- st_read(zone_path) |> st_transform(crs_cible)
  zone_raster <- rasterize(vect(zone), r_friction)
  zone_cells  <- which(!is.na(values(zone_raster)))
  zone_coords <- xyFromCell(r_friction, zone_cells)
  
  # Calcul raster coût
  raster_cout <- creer_raster_cout_depuis_tr_geo(tr_geo, zone_coords)
  raster_cout_spat <- rast(raster_cout)
  crs(raster_cout_spat) <- st_crs(buffers)$wkt
  raster_cout_proj <- project(raster_cout_spat, st_crs(buffers)$wkt)
  
  if (!is.null(raster_export_path)) {
    writeRaster(raster_cout_proj, raster_export_path, overwrite = TRUE)
  }
  
  # Extraction
  valData <- extraire_valeurs(raster_cout_proj, buffers)
  stats_df <- calculer_stats_distance(valData, buffers)
  
  ajouter_stats_a_buffers(buffers, stats_df, prefix)
}

# --- Appliquer pour canyon
buffers_canyon <- traiter_distance(
  zone_path = chemin_canyons,
  tr_geo = tr_geo,
  buffers = layers$buffers,
  crs_cible = target_crs,
  prefix = "canyon",
  raster_export_path = "C:/Users/miche/Desktop/09-marieke/res_canyon/06-export/raster_cost_canyon.tif"
)

# --- Appliquer pour port
buffers_final <- traiter_distance(
  zone_path = chemin_ports,
  tr_geo = tr_geo,
  buffers = buffers_canyon,
  crs_cible = target_crs,
  prefix = "port",
  raster_export_path = "C:/Users/miche/Desktop/09-marieke/res_canyon/06-export/raster_cost_port.tif"
)

# --- Appliquer pour mpa
buffers_final_bis <- traiter_distance(
  zone_path = chemin_mpa,
  tr_geo = tr_geo,
  buffers = buffers_final,
  crs_cible = target_crs,
  prefix = "mpa",
  raster_export_path = "C:/Users/miche/Desktop/09-marieke/res_canyon/06-export/raster_cost_mpa.tif"
)

# --- Export final
st_write(buffers_final_bis, "C:/Users/miche/Desktop/09-marieke/res_canyon/07-port/02-donnees_travaillees/buffer_with_dist_canyon_port_mpa_stats.gpkg", delete_dsn = TRUE)
