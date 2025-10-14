library(sf)
library(dplyr)

setwd("M:/BiodivMed/Distance_to_port_MPA_canyon/")

polygones <- st_read("./01-DATA/01-DATA_SEA/med_regions.geojson")
polygones_italie <- st_read("./01-DATA/01-DATA_SEA/georef-italy-regione/georef-italy-regione-millesime.shp")

if (st_crs(polygones) != st_crs(polygones_italie)) {
  polygones_italie <- st_transform(polygones_italie, st_crs(polygones))
}

polygones <- polygones %>% select(geometry)
polygones_italie <- polygones_italie %>% select(geometry)

polygones_combines <- rbind(polygones, polygones_italie)

polygone_uni <- polygones_combines %>% 
  st_union() %>%
  st_make_valid()

st_write(polygone_uni, "./01-DATA/01-DATA_SEA/france_italie_union.gpkg")


zone_bbox_source <- st_read("./01-DATA/ZonePortuaire_combine_2154_area.gpkg")

bbox <- st_bbox(zone_bbox_source)
bbox_poly <- st_as_sfc(bbox)

bbox_poly <- st_transform(bbox_poly, 2154)
polygone_uni <- st_transform(polygone_uni, 2154)

bbox_buffered <- st_buffer(bbox_poly, 100000)

inverse_poly <- st_difference(bbox_buffered, polygone_uni)

output_inverse_poly <- "./01-DATA/inverse_poly_test.gpkg"

st_write(inverse_poly, output_inverse_poly, delete_dsn = TRUE)

# -------- Combien de ports pas dans la mer --------

inverse_poly <- st_read(output_inverse_poly)

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

# on a fait un raccordement manuel du polygone de la mer pour qu'il soient en contact avec les ports proches de fos sur mer :
# "port Napoléon", "port de plaisance de Carteau", "port de plaisance de Port-Saint-Louis-du-Rhône", "port Abri"                                                                