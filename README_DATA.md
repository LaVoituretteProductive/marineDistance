# Préparation des données Portuaires, Canyons et AMP - Martin PAQUET - août 2025

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

Ce projet vise à compiler, corriger et enrichir les données géospatiales portuaires et récupérer des variables maritimes en Méditerranée en vue de calculer la distance aux transects ADNe et récupérer différents champs pour analyse. Il s’appuie sur des sources officielles (SANDRE, Data.gouv, DataSud, etc.) ainsi que sur des contributions locales (New York State Department of State, PACA, etc) pour constituer un jeu de données fiable et homogène.

La création d'un polygone de port se base sur la définition donnée par le Sandre : 

*Un port est un abri naturel ou artificiel pour les navires et les bateaux, situé sur la côte, un cours d’eau ou un lac ouvert à la navigation, muni des ouvrages et installations permanentes nécessaires à l'embarquement et au débarquement des passagers, des marchandises, ou aux activités de pêche, de plaisance, et le cas échéant d'autres installations associées au trafic maritime ou fluvial (stockage, entretien, réparations, etc.), faisant l’objet d’un arrêté ou autre acte réglementaire en déterminant la nature, les limites, précisant les modalités et l’autorité de gestion et, suivant les cas, qui en désigne l’autorité de police.*

---

## Sources de données

### Ports (SANDRE et autres)

- **Polygones des zones portuaires (SANDRE)**  
  [Consulter la ressource](https://www.sandre.eaufrance.fr/atlas/srv/fre/catalog.search#/metadata/5498d7d1-1fee-4bcf-a0aa-9c57fd29d667/formatters/xsl-view?root=div&view=advanced)

- **Informations portuaires locales** :  
  - [Base nationale des installations portuaires - Data.gouv](https://www.data.gouv.fr/datasets/informations-portuaires-1/)
  - [Ressources complémentaires (plaisance, stations de sauvetage, fonctions portuaires)](https://www.data.gouv.fr/datasets/667220b96b366362f1c4be2d/?resource_id=f3d81845-bd5e-47af-a1da-954154340bf3)
  - [Ports propres en Provence-Alpes-Côte d’Azur - DataSud](https://www.datasud.fr/explorer/fr/jeux-de-donnees/ports-propres-provence-alpes-cote-dazur/telechargements)

### Canyons Marins

- **Open Data Portal - NY** :  
  [Données Canyons](https://opdgig.dos.ny.gov/datasets/250f2b9496854bd098be8154bab04a3a/explore?location=19.426255%2C45.648608%2C3.72)

### Aires Marines Protégées (MPA)

- **Fichier transmis par Marieke**  
  > Champs à mettre à jour manuellement.

### Données de la mer (France / Italie)

- **France** : Redemander la source à Marieke de Med_Regions.

- **Italie** :  
  [Régions italiennes - OpenDataSoft](https://public.opendatasoft.com/explore/dataset/georef-italy-regione-millesime/table/?flg=fr-fr&disjunctive.rip_code&disjunctive.rip_name&disjunctive.reg_code&disjunctive.reg_name&sort=year)

---

## Traitement des données

### Données Portuaires

- Utilisation du **polygone des zones portuaires (SANDRE)**.
- **Modifications manuelles** :
  - Ajustement des zones débordant des limites portuaires.
  - Création de nouvelles zones à partir des points où les polygones étaient absents.
- **Croisement avec d'autres couches** pour identifier les ports oubliés :
  - Ports de plaisance (SMCFAC)
  - Stations de sauvetage (RSCSTA)
  - Fonctions portuaires (HRBFAC)

### Données Canyons

- Sélection des **canyons situés en Méditerranée** via le champ `"Mediterranean Sea"`.
- Export des entités filtrées.

### Données MPA

- Intégration du fichier envoyé par Marieke.
- Champs à harmoniser et compléter.

### Données de la mer

- **Inversion géographique** des couches pour récupérer avec précision la forme de la Méditerranée de notre zone d'étude :
  - Côtes italiennes
  - Couches `"med_regions"` côté français (redemander à Marieke la source)

- Inversion des Polygones de la Mer fait avec le script **r_inversion_polygone_france.R**.

---

## Résultat

- Ports :
  - **49 polygones de ports** générés après croisement et nettoyage + modification des formes non conforme à la définition d'un port.
  - 3 couches générées : ZonePortuaire_combine_2154_area.gpkg, ZonePortuaire_paca_occitanie.gpkg, ZonePortuaire_corse.gpkg
- Données des canyons méditerranéens isolées et exportées.
- Données MPA des zones fully protected.
- Couches maritimes rasterisable des côtes française et italienne.

---

## À faire / Points en attente

- Ajouter les sources de la donnée MPA.
- Redemander les **données françaises** de la mer à Marieke ou prendre un intégral sur opendata.

---

