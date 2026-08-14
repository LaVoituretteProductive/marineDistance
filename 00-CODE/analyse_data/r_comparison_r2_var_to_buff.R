library(sf)
library(dplyr)
library(ggplot2)
library(ggpubr)

setwd("D:/01-backup_data/1-github_repo/08-marineDistance/marineDistance/")

# --- Lecture et reprojection ---
path_buffer <- "./03-RESULTS/buffer_with_closest_feats_50m.gpkg"
# buffers <- dplyr::bind_rows(resultats_list)
# buffers <- buffers_df <- resultats_df
buffers <- st_read(path_buffer)
buffers <- st_transform(buffers, 2154)

# --- Fonction de détection stricte des outliers ---
detect_outliers_strict <- function(data, xvar, yvar, id_col = "ID", quantile_thresh = 0.99) {
  formula <- as.formula(paste(yvar, "~", xvar))
  model <- lm(formula, data = data)
  resids <- resid(model)
  
  threshold <- quantile(abs(resids), quantile_thresh, na.rm = TRUE)
  
  data_with_flag <- data %>%
    mutate(
      resid = resids,
      abs_resid = abs(resids),
      outlier = abs_resid > threshold
    )
  
  outlier_ids <- data_with_flag %>%
    filter(outlier) %>%
    pull(!!sym(id_col))
  
  list(
    data = data_with_flag,
    ids = outlier_ids,
    r2 = summary(model)$r.squared
  )
}

# --- Fonction de plot avec outliers surlignés ---
plot_relation_with_outliers <- function(result, xvar, yvar, label) {
  ggplot(result$data, aes_string(x = xvar, y = yvar)) +
    geom_point(aes(color = outlier), alpha = 0.7) +
    geom_smooth(method = "lm", se = FALSE, color = "blue") +
    scale_color_manual(values = c("FALSE" = "black", "TRUE" = "red")) +
    theme_minimal(base_size = 13) +
    labs(
      title = paste("Relation entre", label),
      subtitle = paste0("R² = ", round(result$r2, 4), " — outliers en rouge"),
      x = xvar,
      y = yvar,
      color = "Outlier"
    )
}

# --- Application aux trois types ---
res_canyon <- detect_outliers_strict(buffers, "canyon_dist_m_min", "canyon_dist_min_from_buff_m")
res_port   <- detect_outliers_strict(buffers, "port_dist_m_min", "port_dist_min_from_buff_m")
res_mpa    <- detect_outliers_strict(buffers, "mpa_dist_m_min", "mpa_dist_min_from_buff_m")
res_shore  <- detect_outliers_strict(buffers, "shore_dist_m_min", "shore_dist_min_from_buff_m")

# --- Affichage des IDs outliers ---
cat("IDs outliers (canyon):", res_canyon$ids, "\n")
cat("IDs outliers (port):", res_port$ids, "\n")
cat("IDs outliers (mpa):", res_mpa$ids, "\n")
# cat("IDs outliers (shore):", res_shore$ids, "\n")

# --- Création des plots ---
p_canyon <- plot_relation_with_outliers(res_canyon, "canyon_dist_m_min", "canyon_dist_min_from_buff_m", "les distances aux canyons")
p_port   <- plot_relation_with_outliers(res_port, "port_dist_m_min", "port_dist_min_from_buff_m", "les distances aux ports")
p_mpa    <- plot_relation_with_outliers(res_mpa, "mpa_dist_m_min", "mpa_dist_min_from_buff_m", "les distances aux aires marines protégées")
# p_shore  <- plot_relation_with_outliers(res_shore, "shore_dist_m_min", "shore_dist_min_from_buff_m", "les distances à la côte")

# --- Affichage combiné ---
ggarrange(p_canyon, p_port, p_mpa, ncol = 3)

# ggarrange(p_canyon, p_port, p_mpa, p_shore, ncol = 4)
