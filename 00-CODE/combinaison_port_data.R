library(sf)

setwd("M:/BiodivMed/Distance_to_port_MPA_canyon/")

gpkg1 <- "./01-DATA/ZonePortuaire_paca_occitanie.gpkg"
gpkg2 <- "./01-DATA/ZonePortuaire_corse.gpkg"

layer1 <- st_read(gpkg1)
layer2 <- st_read(gpkg2)

layer1_2154 <- st_transform(layer1, 2154)
layer2_2154 <- st_transform(layer2, 2154)

zone_portuaire_2154 <- rbind(layer1, layer2)

zone_portuaire_2154 <- zone_portuaire %>%
  mutate(aire_m2 = st_area(.))

output_layer_2154 <- "./01-DATA/ZonePortuaire_combine_2154_area.gpkg"

st_write(zone_portuaire_2154, output_layer_2154)