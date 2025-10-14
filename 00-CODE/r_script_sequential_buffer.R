library(sf)
library(terra)
library(raster)
library(gdistance)
library(dplyr)

# --- Paramètres ---
chemin_buffers <- "C:/Users/miche/Desktop/09-marieke/res_canyon/07-port/02-donnees_travaillees/buffer_with_dist_canyon_port_mpa_stats_v2.gpkg"
chemin_mer <- "C:/Users/miche/Desktop/09-marieke/res_canyon/07-port/02-donnees_travaillees/inverse_poly.gpkg"
output_dir <- "C:/Users/miche/Desktop/09-marieke/res_canyon/07-port/01-DATA/05-test_parallel/output_cost_rasters/"

dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
buffer_margin <- 10000 # 10km

# --- Charger friction globale (valeurs = 1 partout) ---
vect_mer <- vect(chemin_mer)
r_friction_global <- rast(ext(vect_mer), resolution = 100, crs = crs(vect_mer))
r_friction_global <- rasterize(vect_mer, r_friction_global, field = 1, background = NA)

# Charger buffers
buffers <- st_read(chemin_buffers)
buffers$ID <- seq_len(nrow(buffers))

process_buffer_local <- function(buffer_row) {
  id_buf <- buffer_row$ID
  cat("➡️ Traitement buffer ID =", id_buf, "\n")
  
  # Calcul distance max + marge 10 km
  max_dist <- max(c(buffer_row$port_dist_m_max,
                    buffer_row$canyon_dist_m_max,
                    buffer_row$mpa_dist_m_max), na.rm = TRUE) + buffer_margin
  
  # Zone locale : buffer étendu autour de la géométrie originale
  zone_locale <- st_buffer(st_geometry(buffer_row), dist = max_dist)
  
  # Extraire friction locale (crop + mask sur friction globale)
  friction_crop <- crop(r_friction_global, vect(zone_locale))
  friction_local <- mask(friction_crop, vect(zone_locale))
  
  # Convertir en raster package (pour gdistance)
  friction_local_r <- raster(friction_local)
  
  # Construire matrice transition locale (par buffer)
  tr_local <- transition(friction_local_r, function(x) 1/mean(x), directions = 8)
  tr_geo_local <- geoCorrection(tr_local, type = "c")
  
  # Rasteriser buffer original dans zone locale pour extraire pixels sources
  buffer_sp <- as(buffer_row, "Spatial")
  buffer_rast <- rasterize(buffer_sp, friction_local_r)
  cells_buffer <- which(!is.na(values(buffer_rast)))
  
  if(length(cells_buffer) == 0) {
    stop("Pas de pixels dans buffer rasterisé")
  }
  
  coords_source <- xyFromCell(friction_local_r, cells_buffer)
  
  # Calcul accCost local depuis pixels buffer
  r_cost <- accCost(tr_geo_local, SpatialPoints(coords_source))
  
  # Sauvegarder raster accCost en SpatRaster (terra)
  r_cost_spat <- rast(r_cost)
  crs(r_cost_spat) <- crs(r_friction_global)
  
  out_file <- file.path(output_dir, sprintf("buffer_%04d_accCost.tif", id_buf))
  writeRaster(r_cost_spat, out_file, overwrite = TRUE)
  
  cat("✅ Exporté :", out_file, "\n")
}

# --- Exécution séquentielle ---
for(i in seq_len(nrow(buffers))) {
  tryCatch({
    process_buffer_local(buffers[i, ])
  }, error = function(e) {
    cat(sprintf("❌ Erreur buffer %d : %s\n", buffers$ID[i], e$message))
  })
}

cat("Traitement terminé.\n")
