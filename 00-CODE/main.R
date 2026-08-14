# Main
install.packages("roxygen2")
library(roxygen2)

setwd("D:/01-backup_data/1-github_repo/08-marineDistance/test_alone/00-CODE")

source("./calcul_portuaire/port_creation_data.R")
source("./calcul_50m/r_distance_creation_50m.R")

setwd("D:/01-backup_data/1-github_repo/08-marineDistance/test_alone")

# -------------------------------1. Creation port et couche Shore -------------------------------

# 1.1 r_paca_occ_port
gpkg1 <- "./01-DATA/ZonePortuaire_paca_occitanie.gpkg"
gpkg2 <- "./01-DATA/ZonePortuaire_corse.gpkg"
output_layer_2154 <- "./01-DATA/ZonePortuaire_combine_2154_area.gpkg"

r_paca_occ_port(gpkg1, gpkg2, output_layer_2154)


# 1.2 create_poly_sea
polygone_france <- "./01-DATA/01-DATA_SEA/med_regions.geojson"
polygone_italie <- "./01-DATA/01-DATA_SEA/georef-italy-regione/georef-italy-regione-millesime.shp"
polygone_out_combined <- "./01-DATA/01-DATA_SEA/france_italie_union.gpkg"

create_poly_sea(polygone_france, polygone_italie, polygone_out_combined)


# 1.3 r_bbox_inverse_poly
zone_portuaire_path <- "./01-DATA/ZonePortuaire_combine_2154_area.gpkg"
polygon_union_port_sea_path <- "./01-DATA/inverse_polygon_union_port.gpkg"
output_inverse_poly_path <- "./01-DATA/inverse_poly_test.gpkg"

r_bbox_inverse_poly(zone_portuaire_path, polygon_union_port_sea_path, output_inverse_poly_path)


# 1.4 r_merge_port_sea
zone_portuaire_path <- "./01-DATA/ZonePortuaire_combine_2154_area.gpkg"
inverse_poly_path <- "./01-DATA/inverse_poly.gpkg"
polygon_union_port_sea <- "./01-DATA/inverse_polygon_union_port.gpkg"

r_merge_port_sea(zone_portuaire_path, inverse_poly_path, polygon_union_port_sea)


# 1.5 r_create_shore
polygon_union_port_sea <- "./01-DATA/inverse_polygon_union_port.gpkg"
polygon_shore <- "./01-DATA/shore.gpkg"

r_create_shore(polygon_union_port_sea, polygon_shore)
  

# 1.6 r_touch_port
zone_portuaire_path <- "./01-DATA/ZonePortuaire_combine_2154_area.gpkg"
output_inverse_poly_path <- "./01-DATA/inverse_poly_test.gpkg"

r_touch_port(zone_portuaire_path, output_inverse_poly_path)

# ------------------------------- 2. Distance calcul 50 m -------------------------------

# 2.1 creer_zone_limitee_par_buffer

# option pour crop la zone de calcul sur une plus petite surface (100m à 50m) à partir de valeurs déjà calculées

path_mer <- "./01-DATA/inverse_polygon_union_port.gpkg"
couche_mer <- st_read(path_mer)

chemin_transects <- "./02-MID/buffer_with_dist_canyon_port_mpa_stats.gpkg"

prefixes <- c("canyon", "port", "mpa", "shore")

zone_totale_mer <- creer_zone_limitee_par_buffer(
  buffers = chemin_transects,
  prefixes = prefixes,
  couche_mer = couche_mer
)

st_write(zone_totale_mer, "./02-MID/zone_limitee_union_par_buffer.gpkg", delete_dsn = TRUE)

# 2.2 creer_raster_friction

target_crs <- "EPSG:2154"
chemin_mer      <- "./01-DATA/zone_limitee_union_par_buffer.gpkg"
chemin_transects <- "./01-DATA/mtdt_6.gpkg"
chemin_canyons   <- "./01-DATA/canyon_med.geojson"
chemin_ports     <- "./01-DATA/ZonePortuaire_combine_2154_area.gpkg"
chemin_mpa <- "./01-DATA/protectionMed_fr_modif_fully.gpkg"
chemin_shore <- "./01-DATA/shore.gpkg"
chemin_zones <- "./02-MID/zone_limitee_union_par_buffer.gpkg"
resolution <- 50
tampon_sup <- 1000 # 1 km
chemin_mer <- chemin_zones

# --- Charger les couches ---
layers <- charger_vecteurs(chemin_mer, chemin_transects, chemin_canyons, chemin_shore, target_crs)

# --- Créer le raster de friction à partir de la mer ---
r_friction <- creer_raster_friction(
  vect_mer = layers$mer,
  resolution = resolution,
  seuil_couverture = 1,
  export_path = "./02-MID/friction_50m.tif"
)

r_friction <- terra::rast("./02-MID/friction_50m.tif")

# --- Calcul de la matrice de transition ---
tr_geo <- creer_matrice_transition(r_friction)

saveRDS(tr_geo, file = "./02-MID/matrice_transition_geocorrected_50m.rds")
gc()

zone_totale_mer <- st_make_valid(zone_totale_mer)

# 2.3 traiter_distance_zone
# Raster distance d'une variable + extraction par buffer
buffers_canyon <- traiter_distance_zone(
  zone_path = st_read(chemin_canyons),
  crs_cible = "EPSG:2154",
  buffers = layers$buffers,
  prefix = "canyon",
  tr_geo = tr_geo,
  zone_limitee = zone_totale_mer,
  seuil_couverture = 1,
  raster_export_path = "./02-MID/raster_cost_port_fine_seuil0.tif"
)

gc()

buffers_port <- traiter_distance_zone(
  zone_path = st_read(chemin_ports),
  crs_cible = "EPSG:2154",
  buffers = buffers_canyon,
  prefix = "port",
  tr_geo = tr_geo,
  zone_limitee = zone_totale_mer,
  seuil_couverture = 1,
  raster_export_path = "./02-MID/debug_port_fraction.tif"
)

gc()

buffers_mpa <- traiter_distance_zone(
  zone_path = st_read(chemin_mpa),
  crs_cible = "EPSG:2154",
  buffers = buffers_port,
  prefix = "mpa",
  tr_geo = tr_geo,
  zone_limitee = zone_totale_mer,
  seuil_couverture = 1,
  raster_export_path = "./02-MID/debug_mpa_fraction.tif"
)

gc()

buffers_shore <- traiter_distance_zone(
  zone_path = st_read(chemin_shore),
  crs_cible = "EPSG:2154",
  buffers = buffers_mpa,
  prefix = "shore",
  tr_geo = tr_geo,
  zone_limitee = zone_totale_mer,
  seuil_couverture = 1,
  raster_export_path = "./02-MID/debug_shore_fraction.tif"
)

gc()

# --- Résultat final ---
st_write(buffers_shore, "./02-MID/buffer_dist_canyon_port_mpa_union_treshold_shore_50m_fraction.gpkg", delete_dsn = TRUE)







