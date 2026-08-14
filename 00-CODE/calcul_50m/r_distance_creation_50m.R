library(terra)
library(dplyr)
library(gdistance)
library(raster)


#' 1. Buffer from known distance to win space for friction calcul
#'
#' Calcul de la distance max entre les buffers et la mer pour gagner du temps de calcul sur les zones de buffers 
#' à partir de la première inférence à 100m. Permet d'éviter de recalculer sur toute la zone à 100m les données 50m
#' Calculate distance max between the buffers and the sea to win time of calculation on the buffers
#' by the first inference we did at 100m. It avoid to recalculate on all the med for the 50m buffer
#' @param buffers transects avec buffer à 50 m
#' @param prefixes prefixe pour ajouter aux colonnes des indicateurs de distances
#' @param couche_mer polygone de la mer pour l'exclusion
#' @return zone totale de la mer que l'on veut calculer
#' @example 
#' path_mer <- "./01-DATA/inverse_polygon_union_port.gpkg"
#' couche_mer <- st_read(path_mer)
#'
#' prefixes <- c("canyon", "port", "mpa", "shore")
#' 
#' zone_totale_mer <- creer_zone_limitee_par_buffer(
#'   buffers = buffers,
#'   prefixes = prefixes,
#'   couche_mer = couche_mer
#' )
#' 
#' st_write(zone_totale_mer, "./02-MID/zone_limitee_union_par_buffer.gpkg", delete_dsn = TRUE)
#' @export
creer_zone_limitee_par_buffer <- function(buffers, prefixes, couche_mer) {
  
  # check columns for each variables
  buffers <- st_read(buffers)
  dist_cols <- paste0(prefixes, "_dist_m_max")
  dist_cols_exist <- dist_cols[dist_cols %in% names(buffers)]
  
  if (length(dist_cols_exist) == 0) {
    stop("No column max distance were find in the buffer")
  }
  
  # calcul of combine max distance for each buffer
  buffers$dist_max_all <- apply(
    buffers[, dist_cols_exist] |> st_drop_geometry(),
    1,
    function(x) max(x, na.rm = TRUE)
  )
  
  # replace -Inf or NA by 0 (no buffer)
  buffers$dist_max_all[!is.finite(buffers$dist_max_all)] <- 0
  message("Creation of individual buffer by the max distance for each line...")
  
  # Individual creation buffer for each line
  buffers_buff <- st_buffer(buffers, dist = buffers$dist_max_all)
  
  # Union of the max distance buffer
  zone_limitee_union <- st_union(buffers_buff)
  
  # Intersection between buffer to keep only the sea 
  message("Intersection with the sea layer to remove the ground...")
  zone_limitee_mer <- st_intersection(st_make_valid(zone_limitee_union), st_union(couche_mer))
  message("Minimum buffer to each maximum buffer variable created.")
  return(zone_limitee_mer)
}



#' 2. Creation of the friction raster
#'
#' Raster with a value of 1 on all the friction map of the sea and nodata elsewhere
#' @param vect_mer polygon of the sea 
#' @param resolution spatial resolution of the pixel in meters
#' @param seuil_couverture treshold of coverage 0 to 1
#' @param export_path path to export the friction raster
#' 
#' @return friction raster
#' 
#' @example 
#' # --- Paramètres ---
#' target_crs <- "EPSG:2154"
#' chemin_mer      <- "./01-DATA/zone_limitee_union_par_buffer.gpkg"
#' chemin_transects <- "./01-DATA/mtdt_6.gpkg"
#' chemin_canyons   <- "./01-DATA/canyon_med.geojson"
#' chemin_ports     <- "./01-DATA/ZonePortuaire_combine_2154_area.gpkg"
#' chemin_mpa <- "./01-DATA/protectionMed_fr_modif_fully.gpkg"
#' chemin_shore <- "./01-DATA/shore.gpkg"
#' chemin_zones <- "./02-MID/zone_limitee_union_par_buffer.gpkg"
#' resolution_fine <- 50  # exemple de résolution plus fine
#' tampon_sup <- 1000      # 1 km
#' chemin_mer <- chemin_zones
#' 
#' # --- Charger les couches ---
#' layers <- charger_vecteurs(chemin_mer, chemin_transects, chemin_canyons, chemin_shore, target_crs)
#' 
#' # --- Créer le raster de friction à partir de la mer ---
#' r_friction <- creer_raster_friction(
#'   vect_mer = layers$mer,
#'   resolution = resolution_fine,
#'   seuil_couverture = 0.25,
#'   export_path = "./02-MID/friction_50m_thresh025.tif"
#' )
#'  
#' # --- Calcul de la matrice de transition ---
#' tr_geo <- creer_matrice_transition(r_friction)
#'         
#' zone_totale_mer <- st_make_valid(zone_totale_mer)
#' 
#' st_write(zone_totale_mer, "./02-MID/zone_limitee_union_par_buffer.gpkg", delete_dsn = TRUE)
#' @export
creer_raster_friction <- function(vect_mer, resolution = 50, seuil_couverture = 0.3, export_path = NULL) {
  message("Création du raster de friction avec seuil de couverture = ", seuil_couverture * 100, "%")
  
  # friction = 1 for all marine distance
  vect_mer$friction <- 1
  r_template <- rast(ext(vect(vect_mer)), resolution = resolution, crs = crs(vect(vect_mer)))
  
  # rasterisation with sum of coverage (occupation fraction)
  r_frac <- terra::rasterize(vect(vect_mer), r_template, field = "friction", fun = "sum", background = 0, touches = TRUE)
  
  # normalization between 0 and 1
  r_frac <- r_frac / max(r_frac[], na.rm = TRUE)
  
  # application of the threshold 
  values(r_frac) <- ifelse(values(r_frac) >= seuil_couverture, 1, NA)
  
  message("Nombre de cellules marines retenues : ", sum(!is.na(values(r_frac))), " / ", ncell(r_frac))
  
  # optional export
  if (!is.null(export_path)) {
    message("Export du raster de friction vers : ", export_path)
    writeRaster(r_frac, export_path, overwrite = TRUE)
  }
  
  return(r_frac)
}



#' 3. Load the polygon vectors
#' 
#' Load the polygon vectors literally, in sf format
#' @param chemin_mer path to sea
#' @param chemin_transects path to transects
#' @param chemin_zones path to zone
#' @param chemin_shore path to shore
#' @param crs_cible choice of the crs EPSG
#' @return list of the vectors in sf format
#' @export
charger_vecteurs <- function(chemin_mer, chemin_transects, chemin_zones, chemin_shore, crs_cible) {
  vect_mer <- st_read(chemin_mer) |> st_transform(crs_cible)
  buffers  <- st_read(chemin_transects) |> st_transform(crs_cible)
  zones    <- st_read(chemin_zones) |> st_transform(crs_cible)
  shore    <- st_read(chemin_shore) |> st_transform(crs_cible)
  list(mer = vect_mer, buffers = buffers, zones = zones, shore = shore)
}



#' 4. transition matrix creation
#' 
#' Transition matrix creation with 8 directions and geocorrection of gdistance package
#' @param r_friction path of the friction raster
#' @return raster of transition matrix 
#' @example 
#' tr_geo <- creer_matrice_transition(r_friction)
#' @export
creer_matrice_transition <- function(r_friction) {

  message("Début création matrice bip bip")
  tr <- transition(r_friction, function(x) 1 / mean(x), directions = 8)
  message("Fin création matrice bip bip")
  
  message("Début géocorrection bip bip")
  tr_geo <- geoCorrection(tr, type = "c")
  message("Fin géocorrection bip bip")
  
  return(tr_geo)
}



#' 5. Cost raster creation
#' 
#' Creation of the cost raster by gdistance library
#' @param tr_geo path of the friction raster
#' @param coords_sources coords_sources
#' @return Cost raster 
#' @example 
#' raster_cout <- creer_raster_cout_depuis_tr_geo(tr_geo, zone_coords)
creer_raster_cout_depuis_tr_geo <- function(tr_geo, coords_sources) {
  accCost(tr_geo, SpatialPoints(coords_sources))
}



#' 6. Extract values
#' 
#' @param raster_cout cost raster
#' @param vecteur polygon to extract data from the cost raster
#' @return terra object with extracted values of the cost raster
#' @example 
#' valData <- extraire_valeurs(raster_cout_proj, buffers)
extraire_valeurs <- function(raster_cout, vecteur) {
  return(terra::extract(raster_cout, vect(vecteur), weights = TRUE))
  # exact_extract(raster_cout, vect(vecteur))
}



#' 7. Calcul of the distance stats
#' 
#' Calcul of the distances min, max, mean, range and weight from the cost raster
#' @param valData terra object with cost distance data extracted from buffer
#' @param buffers terra object of the buffers
#' @return list  with min, max, mean, range and weight distance by the variable and buffer ID
#' @example 
#' stats_df <- calculer_stats_distance(valData, buffers)
calculer_stats_distance <- function(valData, buffers) {
  val_col <- names(valData)[2]
  
  valData <- valData %>%
    mutate(value = .data[[val_col]]) %>%
    mutate(value = ifelse(value < 0 | is.infinite(value), NA, value)) %>%
    filter(!is.na(value))
  
  stopifnot("weight" %in% names(valData))
  
  summary_values <- valData %>%
    group_by(ID) %>%
    summarise(
      dist_m_min = min(value, na.rm = TRUE),
      dist_m_max = max(value, na.rm = TRUE),
      dist_m_mean = mean(value, na.rm = TRUE),
      dist_m_range = dist_m_max - dist_m_min,
      dist_m_weight = if (all(is.na(weight)) || sum(weight, na.rm = TRUE) == 0) {
        NA_real_
      } else {
        sum(value * weight, na.rm = TRUE) / sum(weight, na.rm = TRUE)
      },
      .groups = "drop"
    )
  
  # check if the IDS of the buffers are still there
  full_ids <- tibble(ID = seq_len(nrow(buffers)))
  summary_values <- full_ids %>%
    left_join(summary_values, by = "ID")
  
  summary_values
}



#' 8. Add stats to the buffer
#' 
#' Add the distances min, max, mean, range and weight in the buffers by ID
#' @param buffers terra object of the buffers
#' @param stats_df statistics from calculer_stats_distance function min, max, mean, range, weight
#' @param prefix char like "canyon", "port", "mpa"
#' @return 
#' @example 
#' ajouter_stats_a_buffers(buffers, stats_df, prefix)
ajouter_stats_a_buffers <- function(buffers, stats_df, prefix) {
  buffers$ID <- seq_len(nrow(buffers))
  buffers <- buffers[, c("ID", setdiff(names(buffers), "ID"))]
  
  colnames(stats_df)[-1] <- paste0(prefix, "_", colnames(stats_df)[-1])
  
  left_join(buffers, stats_df, by = "ID")
}



#' 9. Pipeline of raster distance for a variable + statistical buffer extraction
#' 
#' Calcul of the raster distance from a variable on a grid friction + extraction of the values by buffer
#' Possibility to serialize the addition of the data from different variables in the same buffer.
#' @param zone_path polygons of a variable (canyon, mpa)
#' @param tr_geo raster of the transition matrix
#' @param buffers polygons of the buffers to create the distances from
#' @param crs_cible crs in char (ie : "EPSG:2154")
#' @param prefix char like "canyon", "port", "mpa"
#' @param zone_limitee polygon of the sea
#' @param raster_export_path path for the transition matrix creation
#' @param resolution resolution of the transition matrix you want
#' @param seuil_couverture threshold for the extraction
#' @return 
#' @example 
#' # Example for 2 variables
#' 
#' buffers_canyon <- traiter_distance_zone(
#'   zone_path = st_read(chemin_canyons),
#'   crs_cible = "EPSG:2154",
#'   buffers = layers$buffers,
#'   prefix = "canyon",
#'   tr_geo = tr_geo,
#'   zone_limitee = zone_totale_mer,
#'   seuil_couverture = 0.5,
#'   raster_export_path = "./02-MID/debug_canyon_fraction.tif"
#' )
#' 
#' gc()
#' 
#' buffers_shore <- traiter_distance_zone(
#'   zone_path = st_read(chemin_shore),
#'   crs_cible = "EPSG:2154",
#'   buffers = buffers_canyon,
#'   prefix = "shore",
#'   tr_geo = tr_geo,
#'   zone_limitee = zone_totale_mer,
#'   seuil_couverture = 0.5,
#'   raster_export_path = "./02-MID/debug_shore_fraction.tif"
#' )
#' 
#' gc()
#' 
#' # Final result
#' st_write(buffers_shore, "./02-MID/buffer_dist_canyon_shore_50m_fraction.gpkg", delete_dsn = TRUE)
#' @export
traiter_distance_zone <- function(zone_path, tr_geo, buffers, crs_cible, prefix, zone_limitee,
                                  raster_export_path = NULL, resolution = 50, seuil_couverture = 0.1) {
  message("=== Traitement de la couche ", prefix, " ===")
  
  # --- Lecture et reprojection en crs_cible
  if (inherits(zone_path, "sf")) {
    zone <- st_transform(zone_path, crs_cible)
  } else {
    zone <- st_read(zone_path, quiet = TRUE) |> st_transform(crs_cible)
  }
  
  # --- Nettoyage géométrique
  zone <- suppressWarnings(st_make_valid(zone))
  zone <- zone |> st_collection_extract("POLYGON")  # évite MULTI ou GEOMETRYCOLLECTION
  
  # --- Restreindre la couche à la zone d’étude (EPSG:2154)
  zone_limitee <- st_transform(zone_limitee, crs_cible)
  zone <- st_intersection(zone, zone_limitee)
  
  # --- Créer un raster template basé sur la zone marine limitée
  r_template <- rast(ext(vect(zone_limitee)), resolution = resolution, crs = crs_cible)
  
  # --- Rasterisation
  zone_vect <- try(vect(zone), silent = TRUE)
  if (inherits(zone_vect, "try-error")) {
    warning("⚠️ Conversion sf -> SpatVector échouée pour ", prefix, ". Tentative via sp.")
    zone_sp <- as(zone, "Spatial")
    zone_vect <- vect(zone_sp)
  }
  
  # zone_raster_frac <- terra::rasterize(zone_vect, r_template, field = 1, fun = "sum", background = 0)
  # zone_raster_frac <- zone_raster_frac / max(zone_raster_frac[], na.rm = TRUE)
  
  zone_raster_frac <- terra::rasterize(
    zone_vect,
    r_template,
    field = 1,
    fun = "mean",       # moyenne de couverture
    background = 0,
    touches = TRUE      # inclut les pixels partiellement touchés
  )
  
  # --- Seuil
  zone_raster <- zone_raster_frac
  values(zone_raster) <- ifelse(values(zone_raster) >= seuil_couverture, 1, NA)
  
  n_sources <- sum(!is.na(values(zone_raster)))
  message("Pixels sources retenus (", prefix, ") : ", n_sources)
  if (n_sources == 0) {
    stop("Aucun pixel source trouvé pour ", prefix, " — réduire le seuil ou la résolution.")
  }
  
  # --- Extraction des coordonnées des pixels sources
  zone_cells  <- which(!is.na(values(zone_raster)))
  zone_coords <- xyFromCell(zone_raster, zone_cells)
  
  message("Calcul du raster de coût pour ", prefix)
  raster_cout <- creer_raster_cout_depuis_tr_geo(tr_geo, zone_coords)
  
  raster_cout_spat <- rast(raster_cout)
  crs(raster_cout_spat) <- crs_cible
  raster_cout_proj <- project(raster_cout_spat, crs_cible)
  
  if (!is.null(raster_export_path)) {
    writeRaster(raster_cout_proj, raster_export_path, overwrite = TRUE)
  }
  
  message("Extraction des valeurs pour ", prefix)
  valData <- extraire_valeurs(raster_cout_proj, buffers)
  stats_df <- calculer_stats_distance(valData, buffers)
  
  ajouter_stats_a_buffers(buffers, stats_df, prefix)
}





# # ------------------------------- 4 - Découpage corse et PACA/Occitanie -------------------------------
# 
# 
# # On part de ta couche finale zone_totale_mer
# zone_split <- st_cast(st_make_valid(zone_totale_mer), "MULTIPOLYGON")
# 
# # Sépare chaque polygone en géométrie individuelle
# zone_split <- st_cast(zone_split, "POLYGON")
# zone_split <- st_make_valid(zone_split)
# zone_split$ID_zone <- seq_len(nrow(zone_split))
# 
# # Vérifie combien tu en as (normalement 2 : Corse et continent)
# print(nrow(zone_split))
# 
# # Exporte pour traiter séparément
# st_write(zone_split[1, ], "./02-MID/zone_totale_mer_mainland.gpkg", delete_dsn = TRUE)
# st_write(zone_split[2, ], "./02-MID/zone_totale_mer_corse.gpkg", delete_dsn = TRUE)
# 
# mapview::mapview(zone_split)
# 
# # Charger les deux zones
# zone_mainland <- st_read("./02-MID/zone_totale_mer_mainland.gpkg")
# zone_corse    <- st_read("./02-MID/zone_totale_mer_corse.gpkg")
# 
# # Créer les rasters friction séparés
# r_friction_mainland <- crop(r_friction, vect(zone_mainland))
# r_friction_corse    <- crop(r_friction, vect(zone_corse))
# 
# # Transition et accCost indépendants
# tr_geo_mainland <- creer_matrice_transition(r_friction_mainland)
# tr_geo_corse    <- creer_matrice_transition(r_friction_corse)
# 
# # (Ensuite tes accCost, extractions, etc. par zone)
# 
# acc_mainland_r <- rast("./02-MID/raster_cost_mainland.tif")
# acc_corse_r    <- rast("./02-MID/raster_cost_corse.tif")
# 
# acc_total <- mosaic(acc_mainland_r, acc_corse_r, fun = "min")  # ou autre fonction selon ton besoin
# writeRaster(acc_total, "./02-MID/raster_cost_total.tif", overwrite = TRUE)

