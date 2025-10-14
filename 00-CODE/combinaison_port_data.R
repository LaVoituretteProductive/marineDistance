library(sf)

# Chemins vers les fichiers
gpkg1 <- "C:/Users/miche/Desktop/09-marieke/res_canyon/07-port/02-donnees_travaillees/ZonePortuaire_paca_occitanie.gpkg"
gpkg2 <- "C:/Users/miche/Desktop/09-marieke/res_canyon/07-port/02-donnees_travaillees/ZonePortuaire_corse.gpkg"

# Lire les couches (en supposant qu'il n'y a qu'une seule couche dans chaque fichier)
layer1 <- st_read(gpkg1)
layer2 <- st_read(gpkg2)

# Fusionner les deux jeux de données
zone_portuaire <- rbind(layer1, layer2)

output_layer <- "C:/Users/miche/Desktop/09-marieke/res_canyon/07-port/02-donnees_travaillees/ZonePortuaire_combine.gpkg"

# Sauvegarder dans un nouveau GeoPackage (ou écraser un ancien)
st_write(zone_portuaire, output_layer)


layer1_2154 <- st_transform(layer1, 2154)
layer2_2154 <- st_transform(layer2, 2154)

zone_portuaire_2154 <- rbind(layer1, layer2)

zone_portuaire_2154 <- zone_portuaire %>%
  mutate(aire_m2 = st_area(.))

output_layer_2154 <- "C:/Users/miche/Desktop/09-marieke/res_canyon/07-port/02-donnees_travaillees/ZonePortuaire_combine_2154_area.gpkg"

st_write(zone_portuaire_2154, output_layer_2154)

