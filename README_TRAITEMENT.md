# Création et extraction des données Portuaires, Canyons et AMP par transects ADNe - Martin PAQUET - août 2025

## Sommaire

- [Présentation](#présentation)
- [Sources de données](#sources-de-données)
  - [Ports (SANDRE et autres)](#ports-sandre-et-autres)
  - [Canyons Marins](#canyons-marins)
  - [Aires Marines Protégées (MPA)](#aires-marines-protégées-mpa)
  - [Données de la mer (France / Italie)](#données-de-la-mer-france--italie)
- [Traitement des données](#traitement-des-données)
- [Résultat](#résultat)
- [À faire / Points en attente](#à-faire--points-en-attente)

---

## Présentation

Ce projet vise à extraire les données géospatiales de différentes variables maritimes géoréférencées (port, canyons, amp) en Méditerranée en vue de calculer la distance aux transects ADNe et récupérer différents champs pour analyse. Il s’appuie sur des sources officielles (SANDRE, Data.gouv, DataSud, etc.) ainsi que sur des contributions locales (New York State Department of State, PACA, etc) pour constituer un jeu de données fiable et homogène.

---

## Sources de données

### Ports (SANDRE et autres)

- **Polygones des zones portuaires (SANDRE) retravaillée voir (README_DATA.md) - ZonePortuaire_combine_2154_area.gpkg**  
  [Consulter la ressource](https://www.sandre.eaufrance.fr/atlas/srv/fre/catalog.search#/metadata/5498d7d1-1fee-4bcf-a0aa-9c57fd29d667/formatters/xsl-view?root=div&view=advanced)

### Canyons Marins

- **Open Data Portal - NY - canyon_med.gpkg** :  
  [Données Canyons](https://opdgig.dos.ny.gov/datasets/250f2b9496854bd098be8154bab04a3a/explore?location=19.426255%2C45.648608%2C3.72)

### Aires Marines Protégées (MPA)

- **Fichier transmis par Marieke - protectionMed_fr_modif_fully.gpkg**  
  > Champs à mettre à jour manuellement.

### Données de la mer (France / Italie)

- **Polygone inversé de la France et de l'Italie - inverse_poly.gpkg**

---

## Traitement des données

Objectif : Récupération des champs des géométries des variables les plus proches aux transects ADNe.

Méthode en 3 étapes :

- Récupérer la distance max de chaque transect ADNe à une variable.
- Générer un raster de distance, par transect ADNe, par rapport à un buffer de la dist max + 10km pour optimiser la mémoire.
- Extraction des champs par jointure des géométries des variables les plus proches des transects ADNe.

### 1. Pipeline du script r_factorisation_variable_distance_creation.R

- Récupérer les valeurs maximum de distance des variables au buffer pour ensuite optimiser le buffer pour la génération de accCost par buffer.

1. Rasteriser Mer + Buffer à 100m
2. Calcul de 3 accCost par variable AMP/Canyon/Port
3. Extraction des distances min/max/mean/weighted/range

### 2. Pipeline du script r_script_sequential_buffer.R

- Générer un raster de distance par buffer pour extraire ensuite les champs de chaque géométrie la plus proche.

1. Rasterisation mer 100m résolution sur la med
2. Extraction par transect ADNe dist var max parmi les 3 + 10km
3. Calcul accCost et enregistrement raster de distance par transect

### 3. Pipeline du script r_extract_field_var.R

- Extraire pour chaque champs, les distance min + champs associés par transects.

1. Itération par buffer des 3 couches et on récupère la distance minimum par variable pour comparer avec la distance initiale.
2. création du dataframe et export

---

## Résultat

- Couche des buffers avec 78 champs, les champs importants sont :
  - distance min/max/mean/range/max, de la forme canyon_dist_m_min, calculer au départ sur accCost par variable
  - distance min pour comparer, de la forme canyon_dist_min_from_buff_m, calculer à la fin sur accCost par transect pour comparer avec le min précédent
  - canyon_area_km2, canyon_Mean_Depth, canyon_Length, canyon_Width, canyon_Shape_Area


---

## À faire

- Validation de Marieke
- mpa_fully et la distance à la fully pour créer un binaire présence/absence
- Refaire les scripts pour qu'ils fonctionnent dans le dossier de BIODIVMED

---

