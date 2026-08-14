library(sf)
library(dplyr)


#' 1. Combinaison PACA et Occitanie harbour
#'
#' Combinate all the harbour of mediterranean french coast
#' @param gpkg1 path gpkg occ/paca harbour
#' @param gpkg2 path gpkg corsica harbour
#' @param output_layer_2154 path to save
#' @return gpkg of all harbour of Med sea
#' @example 
#' setwd("M:/BiodivMed/Distance_to_port_MPA_canyon/")
#' gpkg1 <- "./01-DATA/ZonePortuaire_paca_occitanie.gpkg"
#' gpkg2 <- "./01-DATA/ZonePortuaire_corse.gpkg"
#' output_layer_2154 <- "./01-DATA/ZonePortuaire_combine_2154_area.gpkg"
#' @export
r_paca_occ_port <- function(gpkg1, gpkg2, output_layer_2154) {
  
  layer1 <- st_read(gpkg1)
  layer2 <- st_read(gpkg2)
  
  layer1_2154 <- st_transform(layer1, 2154)
  layer2_2154 <- st_transform(layer2, 2154)
  
  zone_portuaire_2154 <- rbind(layer1, layer2)
  
  zone_portuaire_2154 <- zone_portuaire_2154 %>%
    mutate(aire_m2 = st_area(.))
  
  st_write(zone_portuaire_2154, output_layer_2154)
  
}



#' 2. Create polygon of ground
#'
#' gpkg of all ground italia + france
#' @param polygone_france path french ground
#' @param polygone_italie path italian ground
#' @param polygone_out_combined path to save
#' @return gpkg of all ground italia + france
#' @example 
#' setwd("M:/BiodivMed/Distance_to_port_MPA_canyon/")
#' polygone_france <- "./01-DATA/01-DATA_SEA/med_regions.geojson"
#' polygone_italie <- "./01-DATA/01-DATA_SEA/georef-italy-regione/georef-italy-regione-millesime.shp"
#' polygone_out_combined <- "./01-DATA/01-DATA_SEA/france_italie_union.gpkg"
#' @export
create_poly_sea <- function(polygone_france, polygone_italie, polygone_out_combined) {
  
  polygones <- st_read(polygone_france)
  polygones_italie <- st_read(polygone_italie)
  
  if (st_crs(polygones) != st_crs(polygones_italie)) {
    polygones_italie <- st_transform(polygones_italie, st_crs(polygones))
  }
  
  polygones <- polygones %>% select(geometry)
  polygones_italie <- polygones_italie %>% select(geometry)
  
  polygones_combines <- rbind(polygones, polygones_italie)
  
  polygone_uni <- polygones_combines %>% 
    st_union() %>%
    st_make_valid()
  
  st_write(polygone_uni, polygone_out_combined)
  
}



#' 3. Invert polygon ground to a sea polygon
#'
#' harbour + ground to create a sea polygon
#' @param zone_portuaire_path path harbour 
#' @param polygon_union_port_sea_path path sea + harbour
#' @param output_inverse_poly_path path to save
#' @return gpkg of a sea polygon with ports in it
#' @example 
#' setwd("M:/BiodivMed/Distance_to_port_MPA_canyon/")
#' zone_portuaire_path <- "./01-DATA/ZonePortuaire_combine_2154_area.gpkg"
#' polygon_union_port_sea <- "./01-DATA/inverse_polygon_union_port.gpkg"
#' output_inverse_poly_path <- "./01-DATA/inverse_poly_test.gpkg"
#' @export
r_bbox_inverse_poly <- function(zone_portuaire_path, polygon_union_port_sea_path, output_inverse_poly_path) {
  zone_bbox_source <- st_read(zone_portuaire_path)
  polygon_union_port_sea <- st_read(polygon_union_port_sea_path)
  
  bbox <- st_bbox(zone_bbox_source)
  bbox_poly <- st_as_sfc(bbox)

  bbox_poly <- st_transform(bbox_poly, 2154)
  print("bbox_poly")
  polygone_uni <- st_transform(polygon_union_port_sea, 2154)
  print("polygone_uni")
  
  bbox_buffered <- st_buffer(bbox_poly, 100000)
  
  inverse_poly <- st_difference(bbox_buffered, polygone_uni)
  
  st_write(inverse_poly, output_inverse_poly_path, delete_dsn = TRUE)
}



#' 4. Add of a harbour polygon with ports in the sea to calculate the distances to the port
#'
#' Function to combinate ports with the sea, inverse_poly_path is the inverse of the land so the sea
#' @param zone_portuaire_path path harbour 
#' @param polygon_union_port_sea_path path sea + harbour
#' @param output_inverse_poly_path path to save
#' @return gpkg of a sea polygon with ports in it
#' @example 
#' zone_portuaire_path <- "D:/01-backup_data/1-github_repo/08-marineDistance/marineDistance/01-DATA/ZonePortuaire_combine_2154_area.gpkg"
#' inverse_poly_path <- "D:/01-backup_data/1-github_repo/08-marineDistance/marineDistance/01-DATA/inverse_poly.gpkg"
#' polygon_union_port_sea <- "D:/01-backup_data/1-github_repo/08-marineDistance/marineDistance/01-DATA/inverse_polygon_union_port.gpkg"
#' r_merge_port_sea <- r_merge_port_sea(zone_portuaire_path, inverse_poly_path, polygon_union_port_sea)
#' @export
r_merge_port_sea <- function(zone_portuaire_path, inverse_poly_path, polygon_union_port_sea) {
  
  zone_portuaire <- st_read(zone_portuaire_path)
  inverse_poly <- st_read(inverse_poly_path)
  zone_portuaire <- st_transform(zone_portuaire, st_crs(inverse_poly))
  
  union_geom <- st_union(st_make_valid(zone_portuaire), st_make_valid(inverse_poly))
  union_sf <- st_sf(geometry = union_geom)
  
  merged_result <- union_sf %>%
    st_union() %>%
    st_sf()
  
  st_write(merged_result, polygon_union_port_sea)
}



#' 5. create shore
#'
#' create the shore polygon
#' @param polygon_union_port_sea path of polygon sea + port 
#' @param polygon_shore path polygon base shore
#' @return gpkg shore polygon
#' @example 
#' setwd("M:/BiodivMed/Distance_to_port_MPA_canyon/")
#' polygon_union_port_sea <- "./01-DATA/inverse_polygon_union_port.gpkg"
#' polygon_shore <- "./01-DATA/shore.gpkg"
#' r_create_shore <- r_create_shore(polygon_union_port_sea, polygon_shore)
#' @export
r_create_shore <- function(polygon_union_port_sea, polygon_shore) {
  
  zone_bbox_source <- st_read(polygon_union_port_sea)
  
  bbox <- st_bbox(zone_bbox_source)
  bbox_poly <- st_as_sfc(bbox)
  
  bbox_poly <- st_transform(bbox_poly, 2154)
  
  bbox_buffered <- st_buffer(bbox_poly, 10000)
  
  inverse_poly <- st_difference(bbox_buffered, zone_bbox_source)
  
  st_write(inverse_poly, polygon_shore, delete_dsn = TRUE)
  
}



#' 6. Test if harbour are in or off the sea polygon
#'
#' presence absence harbours in sea
#' @param zone_portuaire_path path of the ports
#' @param output_inverse_poly_path path of the sea
#' @return print of the harbour out the shore entirely
#' @example 
#' setwd("M:/BiodivMed/Distance_to_port_MPA_canyon/")
#' zone_portuaire_path <- "./01-DATA/ZonePortuaire_combine_2154_area.gpkg"
#' output_inverse_poly_path <- "./01-DATA/inverse_poly_test.gpkg"
#' r_touch_port <- r_touch_port(zone_portuaire_path, output_inverse_poly_path) {
#' @export
r_touch_port <- function(zone_portuaire_path, output_inverse_poly_path) {
  zone_bbox_source <- st_read(zone_portuaire_path)
  
  inverse_poly <- st_read(output_inverse_poly_path)
  
  ports <- zone_bbox_source
  
  if (st_crs(ports) != st_crs(inverse_poly)) {
    inverse_poly <- st_transform(inverse_poly, st_crs(ports))
  }
  
  intersections <- st_intersects(ports, inverse_poly)
  ports$intersecte <- lengths(intersections) > 0
  
  ports_non_touches <- ports %>% 
    filter(!intersecte)
  
  ports$intersecte <- lengths(intersections) > 0
  nb_non_touches <- sum(!ports$intersecte)
  
  cat("Nombre de ports qui NE touchent PAS le polygone inversé :", nb_non_touches, "\n")
  
  cat("Ports qui NE touchent PAS le polygone inversé :\n")
  print(ports_non_touches$NomZonePor)
}


# on a fait un raccordement manuel du polygone de la mer pour qu'il soient en contact avec les ports proches de fos sur mer :
# "port Napoléon", "port de plaisance de Carteau", "port de plaisance de Port-Saint-Louis-du-Rhône", "port Abri"




# ------------------------------- 7 - calcul euclidien (non necessaire) -------------------------------



#' 7. Euclidian calcul (non necessarly)
#'
#' euclidian distance
#' @param chemin_ports path of the harbours
#' @param chemin_transects path of tbuffers
#' @param chemin_euclidian path of the euclidian layer to save
#' @return return euclidian distance
#' @example 
#' chemin_ports     <- "./01-DATA/ZonePortuaire_combine_2154_area.gpkg"
#' chemin_transects <- "./01-DATA/mtdt_6.gpkg"
#' chemin_euclidian <- "./02-MID/buffer_euclidian_port.gpkg"
#' r_euclidian_port(chemin_ports, chemin_transects, chemin_euclidian)
#' @export
r_euclidian_port <- function(chemin_ports, chemin_transects, chemin_euclidian) {
  
  # Function to calculate the euclidian distance beetween port and transects
  # chemin_ports
  # chemin_transects
  # chemin_euclidian
  
  buffers <- st_read(chemin_transects)|> st_transform(2154)
  # --- Charger les couches si pas déjà faites ---
  ports <- st_read(chemin_ports) |> st_transform(2154)
  
  # --- Calcul du port le plus proche et de la distance euclidienne ---
  calculer_dist_euclidienne_port <- function(buffers, ports) {
    message("Calcul des distances euclidiennes vers le port le plus proche...")
    
    # Trouver l’indice du port le plus proche pour chaque buffer
    idx_plus_proche <- st_nearest_feature(buffers, ports)
    
    # Calculer la distance euclidienne correspondante
    dist_min <- st_distance(buffers, ports[idx_plus_proche, ], by_element = TRUE)
    
    # Ajout dans un data.frame clair
    data.frame(
      ID = seq_len(nrow(buffers)),
      port_dist_euclid_m_min = as.numeric(dist_min)
    )
  }
  
  # --- Application ---
  dist_euclidienne_df <- calculer_dist_euclidienne_port(buffers_final, ports)
  
  # --- Jointure propre dans ton objet principal ---
  buffers_final <- buffers_final %>%
    left_join(dist_euclidienne_df, by = "ID")
  
  st_write(buffers_final, chemin_euclidian, delete_dsn = TRUE)
  
}

