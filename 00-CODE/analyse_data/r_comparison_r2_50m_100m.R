library(sf)
library(dplyr)
library(ggplot2)
library(ggpubr)

setwd("D:/01-backup_data/1-github_repo/08-marineDistance/marineDistance/")

# --- Lecture des deux couches ---
path_v25 <- "./03-RESULTS/buffer_with_closest_feats_search_outlier_treshold_shore_50m_fraction.gpkg"
path_v6  <- "./03-RESULTS/buffer_with_closest_feats_search_outlier_treshold_shore_v6.gpkg"
path_v6  <- "D:/01-backup_data/1-github_repo/08-marineDistance/test_alone/03-RESULTS/buffer_with_closest_feats_search_outlier_treshold_shore_v6.gpkg"

v25 <- st_read(path_v25) %>% st_transform(2154)
v6  <- st_read(path_v6)  %>% st_transform(2154)

# --- Jointure sur l'ID (on garde les valeurs numériques à comparer) ---
comp <- v25 %>%
  st_drop_geometry() %>%
  dplyr::select(ID,
                port_dist_m_min_25 = port_dist_m_min,
                canyon_dist_m_min_25 = canyon_dist_m_min,
                shore_dist_m_min_25 = shore_dist_m_min,
                mpa_dist_m_min_25 = mpa_dist_m_min) %>%
  inner_join(
    v6 %>%
      st_drop_geometry() %>%
      dplyr::select(ID,
                    port_dist_m_min_v6 = port_dist_m_min,
                    canyon_dist_m_min_v6 = canyon_dist_m_min,
                    shore_dist_m_min_v6 = shore_dist_m_min,
                    mpa_dist_m_min_v6 = mpa_dist_m_min),
    by = "ID"
  )


# --- Fonction de détection des outliers et stats ---
detect_outliers_and_plot <- function(df, var_25, var_v6, label) {
  formula <- as.formula(paste(var_v6, "~", var_25))
  model <- lm(formula, data = df)
  df$resid <- resid(model)
  
  threshold <- quantile(abs(df$resid), 0.99, na.rm = TRUE)
  df$outlier <- abs(df$resid) > threshold
  
  # Stats
  r2 <- summary(model)$r.squared
  pearson <- cor(df[[var_25]], df[[var_v6]], use = "complete.obs", method = "pearson")
  
  # Plot
  p <- ggplot(df, aes_string(x = var_25, y = var_v6)) +
    geom_point(aes(color = outlier), alpha = 0.7) +
    geom_smooth(method = "lm", color = "blue", se = FALSE) +
    scale_color_manual(values = c("FALSE" = "black", "TRUE" = "red")) +
    theme_minimal(base_size = 13) +
    labs(
      title = paste("Comparaison des distances —", label),
      subtitle = paste0("R² = ", round(r2, 4), " | Pearson = ", round(pearson, 4)),
      x = "Version 50m (25%)",
      y = "Version v6",
      color = "Outlier"
    )
  
  list(
    plot = p,
    r2 = r2,
    pearson = pearson,
    outliers = df %>% filter(outlier) %>% pull(ID)
  )
}

# --- Application sur les 4 variables ---
res_port   <- detect_outliers_and_plot(comp, "port_dist_m_min_25",   "port_dist_m_min_v6",   "Ports")
res_canyon <- detect_outliers_and_plot(comp, "canyon_dist_m_min_25", "canyon_dist_m_min_v6", "Canyons")
res_shore  <- detect_outliers_and_plot(comp, "shore_dist_m_min_25",  "shore_dist_m_min_v6",  "Côtes")
res_mpa    <- detect_outliers_and_plot(comp, "mpa_dist_m_min_25",    "mpa_dist_m_min_v6",    "AMP")

# --- Affichage combiné ---
ggarrange(res_port$plot, res_canyon$plot, res_shore$plot, res_mpa$plot, ncol = 2, nrow = 2)

# --- Résumé console ---
cat("\n==== Résumé des corrélations ====\n")
cat("Ports   — R²:", round(res_port$r2, 4), " | Pearson:", round(res_port$pearson, 4), "\n")
cat("Canyons — R²:", round(res_canyon$r2, 4), " | Pearson:", round(res_canyon$pearson, 4), "\n")
cat("Côtes   — R²:", round(res_shore$r2, 4), " | Pearson:", round(res_shore$pearson, 4), "\n")
cat("AMP     — R²:", round(res_mpa$r2, 4), " | Pearson:", round(res_mpa$pearson, 4), "\n")

cat("\n==== IDs outliers détectés ====\n")
cat("Ports:",   paste(res_port$outliers, collapse = ", "), "\n")
cat("Canyons:", paste(res_canyon$outliers, collapse = ", "), "\n")
cat("Côtes:",   paste(res_shore$outliers, collapse = ", "), "\n")
cat("AMP:",     paste(res_mpa$outliers, collapse = ", "), "\n")
