#------------------------------
# Non-parametric trend analysis
#------------------------------
library(forestTrends) #remotes::install_github("katemmiller/forestTrends")
library(tidyverse)
#library(lme4) # useful for troubleshooting forestTrends models

library(plantcomNGPN)
library(dplyr)
library(tidyr)
library(ggplot2)

importViews(import_path = "./data_final/NGPN_FFI_views_20250825.zip")

#---- Within plot jaccard similarity using point intercepts ----
point_jac <- read.csv("./data_final/NGPN_point-level_jaccard_similarity.csv")
park_list <- sort(unique(point_jac$Unit_Name))

jac_boot_point <-
  purrr::map(park_list, function(p){
    df <- point_jac |> filter(Unit_Name %in% p)
    mod <- case_boot_lmer(df, x = 'year_std', y = 'jac_mean', group = 'Unit_Name',
                          ID = "MacroPlot_Name", num_reps = 250,
                          random_type = "custom",
                          random_formula = "(1+year_std|MacroPlot_Name) + (1|year_fac)",
                          random_cols = "year_fac")
    mod$Unit_Name <- p
    return(mod)
  }, .progress = T) |>
  list_rbind()

write.csv(jac_boot_point, "./R/results/NGPN_point_level_similarity_trends.csv", row.names = F)

plot_trend_response(jac_boot_point, xlab = 'Year', ylab = "Within Plot Jaccard Similarity",
                    model_type = 'lmer', ribbon = T, group = "Unit_Name",
                    facet_cols = 3) +
  scale_x_continuous(breaks = c(0, 3, 6, 9, 12),
                     labels = c(2011, 2014, 2017, 2020, 2023)) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 0.5))+
  scale_y_continuous(breaks = c(0, 0.2, 0.4, 0.6, 0.8, 1),
                     limits = c(-0.1, 1.1))
ggsave("./R/results/NGPN_within_plot_jaccard_similarity.png", height = 6, width = 8)

#---- Within park similarity, using plot-level species percent cover ----
plot_jac <- read.csv("./data_final/NGPN_plot-level_jaccard_similarity.csv")
plot_jac$year_std <- plot_jac$year - min(plot_jac$year)
plot_jac$year_fac <- as.factor(plot_jac$year)
plot_jac$plotid <- paste0(plot_jac$plot_col1, ".", plot_jac$plot_col2)
head(plot_jac)
tail(plot_jac)
park_list <- sort(unique(plot_jac$Unit_Name))

head(plot_jac)
badl <- plot_jac |> filter(Unit_Name == "BADL")
table(complete.cases(badl))
table(badl$year)
all_tally <- data.frame(table(plot_jac$plotid, plot_jac$Unit_Name)) |> filter(Freq>0)
badl_tally <- data.frame(table(badl$plotid)) |> filter(Freq > 0)

case_boot_sample(badl, x = "year_std", y = "jaccard", group = "Unit_Name", ID = "plotid",
                 model_type = "lmer", random_type = "custom", sample = F,
                 random_cols = "year_fac", random_formula = "(1|plotid) + (1|year_fac)")

# troubleshooting via
#   https://rstudio-pubs-static.s3.amazonaws.com/33653_57fc7b8e5d484c909b615d8633c01d51.html

jac_boot_plot <-
  purrr::map(park_list, function(p){
    df <- plot_jac |> filter(Unit_Name %in% p)
    mod <- case_boot_lmer(df, x = 'year_std', y = 'jaccard', group = 'Unit_Name',
                          ID = "plotid", num_reps = 250,
                          random_type = "custom",
                          random_cols = "year_fac",
                          optimizer = "bobyqa",
                          random_formula = "(1|plotid) + (1|year_fac)") # the plotids are different pairs
    mod$Unit_Name <- p
    return(mod)
  }, .progress = T) |>
  list_rbind()

write.csv(jac_boot_plot, "./R/results/NGPN_plot_level_similarity_trends.csv", row.names = F)

plot_trend_response(jac_boot_plot, xlab = 'Year', ylab = "Within Park Jaccard Similarity",
                    model_type = 'lmer', ribbon = T, group = "Unit_Name",
                    facet_cols = 3) +
  scale_x_continuous(breaks = c(0, 3, 6, 9, 12),
                     limits = c(-1, 14),
                     labels = c(2011, 2014, 2017, 2020, 2023)) +
  scale_y_continuous(breaks = c(0, 0.2, 0.4, 0.6, 0.8, 1),
                     limits = c(0, 1)) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 0.5))

ggsave("./R/results/NGPN_within_park_jaccard_similarity.png", height = 6, width = 8)

#---- plot-level species richness ----
spprich <- read.csv("./data_final/NGPN_plot_level_spprich_by_lifeform.csv")

spprich_tot <- spprich |> select(MacroPlot_Name, Unit_Name, year, month, doy, total_spp) |> distinct() |>
  mutate(year_std = year - min(spprich$year),
         year_fac = as.factor(year))

sppricht_boot <-
  purrr::map(park_list, function(p){
    df <- spprich_tot |> filter(Unit_Name %in% p)
    mod <- case_boot_lmer(df, x = 'year_std', y = 'total_spp', group = 'Unit_Name',
                          ID = "MacroPlot_Name", num_reps = 250,
                          random_type = "custom",
                          random_cols = "year_fac",
                          optimizer = "bobyqa",
                          random_formula = "(1|MacroPlot_Name) + (1|year_fac)")
    mod$Unit_Name <- p
    return(mod)
  }, .progress = T) |>
  list_rbind()

write.csv(sppricht_boot, "./R/results/NGPN_plot_level_spprich_trends.csv", row.names = F)

plot_trend_response(sppricht_boot, xlab = 'Year', ylab = "Plot-level Spp. Rich.",
                    model_type = 'lmer', ribbon = T, group = "Unit_Name",
                    facet_cols = 3) +
  scale_x_continuous(breaks = c(0, 3, 6, 9, 12),
                     limits = c(-1, 14),
                     labels = c(2011, 2014, 2017, 2020, 2023)) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 0.5))

ggsave("./R/results/NGPN_plot_level_species_richness.png", height = 6, width = 8)

#---- plot-level species richness - by life form ----
lifeforms = sort(unique(spprich$LifeForm))

park_lf <- expand.grid(park = park_list, lifeform = lifeforms) |> arrange(park, lifeform)

head(park_lf)

spprich_lf_boot <-
  purrr::map2(park_lf$park, park_lf$lifeform,
              function(p, lf){
    df <- spprich |> filter(Unit_Name %in% p) |> filter(LifeForm %in% lf) |>
      distinct() |>
      mutate(year_std = year - min(spprich$year),
             year_fac = as.factor(year))
    mod <- case_boot_lmer(df, x = 'year_std', y = 'numspp', group = 'Unit_Name',
                          ID = "MacroPlot_Name", num_reps = 250,
                          random_type = "custom",
                          random_cols = "year_fac",
                          optimizer = "bobyqa",
                          random_formula = "(1|MacroPlot_Name) + (1|year_fac)")
    mod$Unit_Name <- p
    mod$LifeForm <- lf
    return(mod)
  }, .progress = T) |>
  list_rbind()

write.csv(spprich_lf_boot, "./R/results/NGPN_plot_level_spprich_lifeform_trends.csv", row.names = F)

plot_trend_response(spprich_lf_boot |> filter(LifeForm == "Forb/herb"),
                    xlab = 'Year', ylab = "Plot-level Spp. Rich.- Forbs",
                    model_type = 'lmer', ribbon = T, group = "Unit_Name",
                    facet_cols = 3) +
  scale_x_continuous(breaks = c(0, 3, 6, 9, 12),
                     limits = c(-1, 14),
                     labels = c(2011, 2014, 2017, 2020, 2023)) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 0.5))

ggsave("./R/results/NGPN_plot_level_species_richness_Forbs.png", height = 6, width = 8)

plot_trend_response(spprich_lf_boot |> filter(LifeForm == "Graminoid"),
                    xlab = 'Year', ylab = "Plot-level Spp. Rich.- Nat. Graminoid",
                    model_type = 'lmer', ribbon = T, group = "Unit_Name",
                    facet_cols = 3) +
  scale_x_continuous(breaks = c(0, 3, 6, 9, 12),
                     limits = c(-1, 14),
                     labels = c(2011, 2014, 2017, 2020, 2023)) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 0.5))

ggsave("./R/results/NGPN_plot_level_species_richness_Graminoid.png", height = 6, width = 8)

plot_trend_response(spprich_lf_boot |> filter(LifeForm == "Graminoid - Inv."),
                    xlab = 'Year', ylab = "Plot-level Spp. Rich.- Inv. Graminoid",
                    model_type = 'lmer', ribbon = T, group = "Unit_Name",
                    facet_cols = 3) +
  scale_x_continuous(breaks = c(0, 3, 6, 9, 12),
                     limits = c(-1, 14),
                     labels = c(2011, 2014, 2017, 2020, 2023)) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 0.5))

ggsave("./R/results/NGPN_plot_level_species_richness_Graminoid-Inv.png", height = 6, width = 8)

plot_trend_response(spprich_lf_boot |> filter(LifeForm == "Shrub"),
                    xlab = 'Year', ylab = "Plot-level Spp. Rich.- Shrub",
                    model_type = 'lmer', ribbon = T, group = "Unit_Name",
                    facet_cols = 3) +
  scale_x_continuous(breaks = c(0, 3, 6, 9, 12),
                     limits = c(-1, 14),
                     labels = c(2011, 2014, 2017, 2020, 2023)) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 0.5))

ggsave("./R/results/NGPN_plot_level_species_richness_Shrub.png", height = 6, width = 8)

plot_trend_response(spprich_lf_boot |> filter(LifeForm == "Subshrub"),
                    xlab = 'Year', ylab = "Plot-level Spp. Rich.- Subshrub",
                    model_type = 'lmer', ribbon = T, group = "Unit_Name",
                    facet_cols = 3) +
  scale_x_continuous(breaks = c(0, 3, 6, 9, 12),
                     limits = c(-1, 14),
                     labels = c(2011, 2014, 2017, 2020, 2023)) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 0.5))

ggsave("./R/results/NGPN_plot_level_species_richness_subshrub.png", height = 6, width = 8)

plot_trend_response(spprich_lf_boot |> filter(LifeForm == "Tree"),
                    xlab = 'Year', ylab = "Plot-level Spp. Rich.- Tree",
                    model_type = 'lmer', ribbon = T, group = "Unit_Name",
                    facet_cols = 3) +
  scale_x_continuous(breaks = c(0, 3, 6, 9, 12),
                     limits = c(-1, 14),
                     labels = c(2011, 2014, 2017, 2020, 2023)) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 0.5))

ggsave("./R/results/NGPN_plot_level_species_richness_Tree.png", height = 6, width = 8)

#---- plot-level species richness - by life form simplified ----
spprichs <- read.csv("./data_final/NGPN_plot_level_spprich_by_lifeform_simp.csv")
lifeformsr = sort(unique(spprichs$LifeForm))

park_lfsr <- expand.grid(park = park_list, lifeform = lifeformsr) |> arrange(park, lifeform)

head(park_lfsr)

spprich_lf_simp_boot <-
  purrr::map2(park_lfsr$park, park_lfsr$lifeform,
              function(p, lf){
                df <- spprichs |> filter(Unit_Name %in% p) |> filter(LifeForm %in% lf) |>
                  distinct() |>
                  mutate(year_std = year - min(spprichs$year),
                         year_fac = as.factor(year))
                mod <- case_boot_lmer(df, x = 'year_std', y = 'numspp', group = 'Unit_Name',
                                      ID = "MacroPlot_Name", num_reps = 250,
                                      random_type = "custom",
                                      random_cols = "year_fac",
                                      optimizer = "bobyqa",
                                      random_formula = "(1|MacroPlot_Name) + (1|year_fac)")
                mod$Unit_Name <- p
                mod$LifeForm <- lf
                return(mod)
              }, .progress = T) |>
  list_rbind()

write.csv(spprich_lf_simp_boot, "./R/results/NGPN_plot_level_spprich_lifeform_simp_trends.csv", row.names = F)

# plotting lifeform trend results
col_pal <- c("Forb/herb" = "#36C736", "Graminoid" = "#efdf00", "Graminoid - Inv." = "#C7381C",
             "Woody" = "#279CF5")

fill_pal <- c("Forb/herb" = "#36C736", "Graminoid" = "#efdf00", "Graminoid - Inv." = "#C7381C",
              "Woody" = "#279CF5")

label_leg <- c("Forb/herb" = "Forb/herb", "Graminoid" = "Graminoid",
               "Graminoid - Inv." = "Graminoid - Inv.", "Woody" = "Woody")

shps <- c("Forb/herb" = 21, "Graminoid" = 24, "Graminoid - Inv." = 22,
          "Woody" = 23)

shpsz <- c("Forb/herb" = 3, "Graminoid" = 2.5, "Graminoid - Inv." = 2,
           "Woody" = 3)

spprich_lf_simp_boot$LifeForm_fac <-
  factor(spprich_lf_simp_boot$LifeForm,
         levels = c("Forb/herb", "Graminoid", "Graminoid - Inv.", "Woody"))

minyr <- min(pctcov$year)

spprich_lf_simp_boot2 <-
  left_join(spprich_lf_simp_boot, spprich_lf_simp_boot |>  filter(term == "Slope") |>
              mutate(sign = case_when(lower95 > 0 ~ "sign",
                                      upper95 < 0 ~ "sign",
                                      is.na(lower95) ~ "notmod",
                                      TRUE ~ "nonsign")) |>
              select(Unit_Name, LifeForm, sign),
            by = c("Unit_Name", "LifeForm")) |>
  filter(!term %in% c("Intercept", "Slope"))

spprich_lf_simp_res <- spprich_lf_simp_boot2 |> filter(!term %in% c("Intercept", "Slope")) |>
  mutate(year = as.numeric(gsub("\\D", "", term)) + minyr)
head(spprich_lf_simp_res)

ggplot(spprich_lf_simp_res,
       aes(x = year, y = estimate,
           group = LifeForm_fac, color = LifeForm_fac,
           fill = LifeForm_fac, shape = LifeForm_fac)) +
  geom_ribbon(aes(ymin = lower95, ymax = upper95,
                  fill = LifeForm_fac, color = LifeForm_fac),
              linewidth = 0.5, alpha = 0.2) +
  geom_point(aes(size = LifeForm_fac), show.legend = T) +
  facet_wrap(~Unit_Name) +
  scale_color_manual(guide = "legend", values = col_pal, name = "Life Form", labels = label_leg, drop = F) +
  scale_fill_manual(guide = "legend", values = col_pal, name = "Life Form", labels = label_leg, drop = F) +
  scale_shape_manual(guide = "legend", values = shps, name = "Life Form", labels = label_leg) +
  scale_size_manual(values = shpsz, name = "Life Form", labels = label_leg) +
  theme_bw() + labs(x = NULL, y = "Mean Species Richness") +
  theme(legend.position = "right") +
  scale_x_continuous(breaks = c(2011, 2014, 2017, 2020, 2023),
                     limits = c(2010, 2024)) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 0.5)) +
  geom_ribbon(data = spprich_lf_simp_res |> filter(!sign %in% "sign"),
              aes(ymin = lower95, ymax = upper95), fill = "white",
              linewidth = 0.5, show.legend = F) +
  geom_point(data = spprich_lf_simp_res |> filter(!sign %in% "sign"),
             aes(shape = LifeForm_fac, size = LifeForm_fac), fill = "white",
             #color = NULL,
             show.legend = F) +
  geom_line(linewidth = 0.6) +
  geom_ribbon(data = spprich_lf_simp_res |> filter(sign == "sign"),
              aes(ymin = lower95, ymax = upper95, fill = LifeForm_fac, color = LifeForm_fac),
              linewidth = 0.5, alpha = 0.2) +
  geom_point(data = spprich_lf_simp_res |> filter(sign == "sign"),
             aes(size = LifeForm_fac), show.legend = T)


ggsave("./R/results/NGPN_plot_level_spprich_Life_Form_simp.png", height = 8, width = 10)

#---- Trends in abundance by lifeform ----
pctcov1 <- read.csv("./data_final/Point_Intercept_LifeForm_2011-2024.csv")
pctcov <- pctcov1 |>
  distinct() |>
  mutate(year_std = year - min(pctcov1$year),
         year_fac = as.factor(year))

lifeforms = sort(unique(pctcov$LifeForm))

park_lf <- expand.grid(park = park_list, lifeform = lifeforms) |> arrange(park, lifeform)

pctcov_lf_boot <-
  purrr::map2(park_lf$park, park_lf$lifeform,
              function(p, lf){
                df <- pctcov |> filter(Unit_Name %in% p) |> filter(LifeForm %in% lf) |>
                  distinct()
                mod <- case_boot_lmer(df, x = 'year_std', y = 'pct_cov', group = 'Unit_Name',
                                      ID = "MacroPlot_Name", num_reps = 250,
                                      random_type = "custom",
                                      random_cols = "year_fac",
                                      optimizer = "bobyqa",
                                      random_formula = "(1|MacroPlot_Name) + (1|year_fac)")
                mod$Unit_Name <- p
                mod$LifeForm <- lf
                return(mod)
              }, .progress = T) |>
  list_rbind()

write.csv(pctcov_lf_boot, "./R/results/NGPN_plot_level_pctcov_lifeform_trends.csv", row.names = F)

plot_trend_response(pctcov_lf_boot |> filter(LifeForm == "Bare"),
                    xlab = 'Year', ylab = "Pct. Cover - Bare",
                    model_type = 'lmer', ribbon = T, group = "Unit_Name",
                    facet_cols = 3) +
  scale_x_continuous(breaks = c(0, 3, 6, 9, 12),
                     limits = c(-1, 14),
                     labels = c(2011, 2014, 2017, 2020, 2023)) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 0.5))

ggsave("./R/results/NGPN_plot_level_pctcov_Bare.png", height = 6, width = 8)

plot_trend_response(pctcov_lf_boot |> filter(LifeForm == "Forb/herb"),
                    xlab = 'Year', ylab = "Pct. Cover - Forb",
                    model_type = 'lmer', ribbon = T, group = "Unit_Name",
                    facet_cols = 3) +
  scale_x_continuous(breaks = c(0, 3, 6, 9, 12),
                     limits = c(-1, 14),
                     labels = c(2011, 2014, 2017, 2020, 2023)) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 0.5))

ggsave("./R/results/NGPN_plot_level_pctcov_Forb.png", height = 6, width = 8)

plot_trend_response(pctcov_lf_boot |> filter(LifeForm == "Graminoid"),
                    xlab = 'Year', ylab = "Pct. Cover - Graminoid",
                    model_type = 'lmer', ribbon = T, group = "Unit_Name",
                    facet_cols = 3) +
  scale_x_continuous(breaks = c(0, 3, 6, 9, 12),
                     limits = c(-1, 14),
                     labels = c(2011, 2014, 2017, 2020, 2023)) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 0.5))

ggsave("./R/results/NGPN_plot_level_pctcov_Graminoid.png", height = 6, width = 8)

plot_trend_response(pctcov_lf_boot |> filter(LifeForm == "Graminoid - Inv."),
                    xlab = 'Year', ylab = "Pct. Cover - Inv. Graminoid",
                    model_type = 'lmer', ribbon = T, group = "Unit_Name",
                    facet_cols = 3) +
  scale_x_continuous(breaks = c(0, 3, 6, 9, 12),
                     limits = c(-1, 14),
                     labels = c(2011, 2014, 2017, 2020, 2023)) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 0.5))

ggsave("./R/results/NGPN_plot_level_pctcov_Graminoid_Inv.png", height = 6, width = 8)

plot_trend_response(pctcov_lf_boot |> filter(LifeForm == "Litter"),
                    xlab = 'Year', ylab = "Pct. Cover - Litter",
                    model_type = 'lmer', ribbon = T, group = "Unit_Name",
                    facet_cols = 3) +
  scale_x_continuous(breaks = c(0, 3, 6, 9, 12),
                     limits = c(-1, 14),
                     labels = c(2011, 2014, 2017, 2020, 2023)) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 0.5))

ggsave("./R/results/NGPN_plot_level_pctcov_Litter.png", height = 6, width = 8)

plot_trend_response(pctcov_lf_boot |> filter(LifeForm == "Other Non-Bio"),
                    xlab = 'Year', ylab = "Pct. Cover - Other Non-Bio",
                    model_type = 'lmer', ribbon = T, group = "Unit_Name",
                    facet_cols = 3) +
  scale_x_continuous(breaks = c(0, 3, 6, 9, 12),
                     limits = c(-1, 14),
                     labels = c(2011, 2014, 2017, 2020, 2023)) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 0.5))

ggsave("./R/results/NGPN_plot_level_pctcov_Other Non-Bio.png", height = 6, width = 8)

plot_trend_response(pctcov_lf_boot |> filter(LifeForm == "Shrub"),
                    xlab = 'Year', ylab = "Pct. Cover - Shrub",
                    model_type = 'lmer', ribbon = T, group = "Unit_Name",
                    facet_cols = 3) +
  scale_x_continuous(breaks = c(0, 3, 6, 9, 12),
                     limits = c(-1, 14),
                     labels = c(2011, 2014, 2017, 2020, 2023)) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 0.5))

ggsave("./R/results/NGPN_plot_level_pctcov_Shrub.png", height = 6, width = 8)

plot_trend_response(pctcov_lf_boot |> filter(LifeForm == "Subshrub"),
                    xlab = 'Year', ylab = "Pct. Cover - Subshrub",
                    model_type = 'lmer', ribbon = T, group = "Unit_Name",
                    facet_cols = 3) +
  scale_x_continuous(breaks = c(0, 3, 6, 9, 12),
                     limits = c(-1, 14),
                     labels = c(2011, 2014, 2017, 2020, 2023)) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 0.5))

ggsave("./R/results/NGPN_plot_level_pctcov_Subshrub.png", height = 6, width = 8)

plot_trend_response(pctcov_lf_boot |> filter(LifeForm == "Tree"),
                    xlab = 'Year', ylab = "Pct. Cover - Tree",
                    model_type = 'lmer', ribbon = T, group = "Unit_Name",
                    facet_cols = 3) +
  scale_x_continuous(breaks = c(0, 3, 6, 9, 12),
                     limits = c(-1, 14),
                     labels = c(2011, 2014, 2017, 2020, 2023)) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 0.5))

ggsave("./R/results/NGPN_plot_level_pctcov_Tree.png", height = 6, width = 8)

plot_trend_response(pctcov_lf_boot |> filter(LifeForm == "Vine"),
                    xlab = 'Year', ylab = "Pct. Cover - Vine",
                    model_type = 'lmer', ribbon = T, group = "Unit_Name",
                    facet_cols = 3) +
  scale_x_continuous(breaks = c(0, 3, 6, 9, 12),
                     limits = c(-1, 14),
                     labels = c(2011, 2014, 2017, 2020, 2023)) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 0.5))

ggsave("./R/results/NGPN_plot_level_pctcov_Vine.png", height = 6, width = 8)

# plotting lifeform trend results
col_pal <- c("Forb/herb" = "#05e689", "Graminoid" = "#efdf00", "Graminoid - Inv." = "#C7381C",
             "Subshrub" = "#9371B9",
             "Shrub" = "#386EC7", "Tree" = "#8A20A1", "Vine" = "#ff7f00",
             "Bare" = "#735E47", "Litter" = "#C2AF80", "Nonvascular" = '#DFACFC',
             "Other Non-Bio" = "#575757", "Undefined" = "grey")

labels <- c("Forb/herb" = "Forb/herb", "Graminoid" = "Graminoid",
            "Graminoid - Inv." = "Graminoid - Inv.", "Subshrub" = "Subshrub",
            "Shrub" = "Shrub", "Tree" = "Tree", "Vine" = "Woody Vine",
            "Bare" = "Bare Ground", "Litter" = "Dead Litter", "Nonvascular" = "Nonvascular",
            "Other Non-Bio" = "Other Non-Bio", "Undefined" = "Undefined")

shps <- c("Forb/herb" = 21, "Graminoid" = 24, "Graminoid - Inv." = 22,
          "Subshrub" = 18, "Shrub" = 23, "Tree" = 24,
          "Vine" = 22, "Bare" = 23, "Litter" = 13, "Nonvascular" = 22,
          "Other Non-Bio" = 24, "Undefined" = 21)

shpsz <- c("Forb/herb" = 3, "Graminoid" = 2.5, "Graminoid - Inv." = 2,
           "Subshrub" = 3, "Shrub" = 2, "Tree" = 2.5,
           "Vine" = 2, "Bare" = 2, "Litter" = 3,
           "Nonvascular" = 2, "Other Non-Bio" = 2, "Undefined" = 3)

pctcov_lf_boot$LifeForm_fac <-
  factor(pctcov_lf_boot$LifeForm,
         levels = c("Forb/herb", "Graminoid", "Graminoid - Inv.", "Subshrub", "Shrub", "Tree", "Vine",
                    "Bare", "Litter", "Nonvascular", "Other Non-Bio", "Undefined"))
minyr <- min(pctcov$year)

pctcov_lf_boot2 <-
  left_join(pctcov_lf_boot, pctcov_lf_boot  |>  filter(term == "Slope") |>
              mutate(sign = case_when(lower95 > 0 ~ "signinc",
                                      upper95 < 0 ~ "signdec",
                                      is.na(lower95) ~ "notmod",
                                      TRUE ~ "nonsign")) |>
              select(Unit_Name, LifeForm, sign),
            by = c("Unit_Name", "LifeForm")) |>
  filter(!term %in% c("Intercept", "Slope"))

pctcov_lf_res <- pctcov_lf_boot2 |> filter(!term %in% c("Intercept", "Slope")) |>
  mutate(year = as.numeric(gsub("\\D", "", term)) + minyr)
head(pctcov_lf_res)

pctcov_lf_res_com <- pctcov_lf_res |> filter(LifeForm %in% c("Forb/herb", "Graminoid", "Graminoid - Inv.", "Subshrub", "Shrub", "Tree",
                                                             "Bare", "Litter", "Nonvascular", "Other Non-Bio", "Undefined"))

ggplot(pctcov_lf_res,
       aes(x = year, y = estimate,
           group = LifeForm_fac, color = LifeForm_fac,
           shape = LifeForm_fac, size = LifeForm_fac)) +
  geom_ribbon(aes(ymin = lower95, ymax = upper95, fill = LifeForm_fac),
              width = 0.2, linewidth = 0.5, alpha = 0.2) +
  geom_point() +
  facet_wrap(~Unit_Name) +
  scale_shape_manual(values = shps, name = "Life Form", labels = labels) +
  scale_size_manual(values = shpsz,
                    name = "Life Form", labels = labels) +
  scale_color_manual(values = col_pal, name = "Life Form", labels = labels) +
  scale_fill_manual(values = col_pal, name = "Life Form", labels = labels) +
  geom_line(aes(linetype = sign), linewidth = 0.5) +
  scale_linetype_manual(values = c("notmod" = 'dotted', "nonsign" = 'dashed',
                                   "signinc" = 'solid', "signdec" = 'solid'),
                        name = "Trend") +
  theme_bw() + labs(x = NULL, y = "Mean % Cover") +
  theme(legend.position = "bottom") +
  scale_x_continuous(breaks = c(2011, 2014, 2017, 2020, 2023),
                     limits = c(2010, 2024)) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 0.5))


#---- Trends in abundance by simplified lifeform ----
pctcov1 <- read.csv("./data_final/Point_Intercept_LifeForm_simp_2011-2024.csv")
pctcov <- pctcov1 |>
  distinct() |>
  mutate(year_std = year - min(pctcov1$year),
         year_fac = as.factor(year))

lifeforms = sort(unique(pctcov$LifeForm))

park_lf <- expand.grid(park = park_list, lifeform = lifeforms) |> arrange(park, lifeform)

pctcov_lf_simp_boot <-
  purrr::map2(park_lf$park, park_lf$lifeform,
              function(p, lf){
                df <- pctcov |> filter(Unit_Name %in% p) |> filter(LifeForm_simp %in% lf) |>
                  distinct()
                mod <- case_boot_lmer(df, x = 'year_std', y = 'pct_cov', group = 'Unit_Name',
                                      ID = "MacroPlot_Name", num_reps = 250,
                                      random_type = "custom",
                                      random_cols = "year_fac",
                                      optimizer = "bobyqa",
                                      random_formula = "(1|MacroPlot_Name) + (1|year_fac)")
                mod$Unit_Name <- p
                mod$LifeForm_simp <- lf
                return(mod)
              }, .progress = T) |>
  list_rbind()

write.csv(pctcov_lf_simp_boot, "./R/results/NGPN_plot_level_pctcov_lifeform_simp_trends.csv", row.names = F)

plot_trend_response(pctcov_lf_simp_boot |> filter(LifeForm_simp == "Bare"),
                    xlab = 'Year', ylab = "Pct. Cover - Bare",
                    model_type = 'lmer', ribbon = T, group = "Unit_Name",
                    facet_cols = 3) +
  scale_x_continuous(breaks = c(0, 3, 6, 9, 12),
                     limits = c(-1, 14),
                     labels = c(2011, 2014, 2017, 2020, 2023)) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 0.5))

ggsave("./R/results/NGPN_plot_level_pctcov_Bare_simp.png", height = 6, width = 8)

plot_trend_response(pctcov_lf_simp_boot |> filter(LifeForm_simp == "Forb/herb"),
                    xlab = 'Year', ylab = "Pct. Cover - Forb",
                    model_type = 'lmer', ribbon = T, group = "Unit_Name",
                    facet_cols = 3) +
  scale_x_continuous(breaks = c(0, 3, 6, 9, 12),
                     limits = c(-1, 14),
                     labels = c(2011, 2014, 2017, 2020, 2023)) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 0.5))

ggsave("./R/results/NGPN_plot_level_pctcov_Forb_simp.png", height = 6, width = 8)

plot_trend_response(pctcov_lf_simp_boot |> filter(LifeForm_simp == "Graminoid"),
                    xlab = 'Year', ylab = "Pct. Cover - Graminoid",
                    model_type = 'lmer', ribbon = T, group = "Unit_Name",
                    facet_cols = 3) +
  scale_x_continuous(breaks = c(0, 3, 6, 9, 12),
                     limits = c(-1, 14),
                     labels = c(2011, 2014, 2017, 2020, 2023)) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 0.5))

ggsave("./R/results/NGPN_plot_level_pctcov_Graminoid_simp.png", height = 6, width = 8)

plot_trend_response(pctcov_lf_simp_boot |> filter(LifeForm_simp == "Graminoid - Inv."),
                    xlab = 'Year', ylab = "Pct. Cover - Inv. Graminoid",
                    model_type = 'lmer', ribbon = T, group = "Unit_Name",
                    facet_cols = 3) +
  scale_x_continuous(breaks = c(0, 3, 6, 9, 12),
                     limits = c(-1, 14),
                     labels = c(2011, 2014, 2017, 2020, 2023)) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 0.5))

ggsave("./R/results/NGPN_plot_level_pctcov_Graminoid_Inv_simp.png", height = 6, width = 8)

plot_trend_response(pctcov_lf_simp_boot |> filter(LifeForm_simp == "Litter"),
                    xlab = 'Year', ylab = "Pct. Cover - Litter",
                    model_type = 'lmer', ribbon = T, group = "Unit_Name",
                    facet_cols = 3) +
  scale_x_continuous(breaks = c(0, 3, 6, 9, 12),
                     limits = c(-1, 14),
                     labels = c(2011, 2014, 2017, 2020, 2023)) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 0.5))

ggsave("./R/results/NGPN_plot_level_pctcov_Litter_simp.png", height = 6, width = 8)

plot_trend_response(pctcov_lf_simp_boot |> filter(LifeForm_simp == "Other Non-Vasc"),
                    xlab = 'Year', ylab = "Pct. Cover - Other Non-Vasc",
                    model_type = 'lmer', ribbon = T, group = "Unit_Name",
                    facet_cols = 3) +
  scale_x_continuous(breaks = c(0, 3, 6, 9, 12),
                     limits = c(-1, 14),
                     labels = c(2011, 2014, 2017, 2020, 2023)) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 0.5))

ggsave("./R/results/NGPN_plot_level_pctcov_Other Non-Vasc_simp.png", height = 6, width = 8)

plot_trend_response(pctcov_lf_simp_boot |> filter(LifeForm_simp == "Woody"),
                    xlab = 'Year', ylab = "Pct. Cover - Woody",
                    model_type = 'lmer', ribbon = T, group = "Unit_Name",
                    facet_cols = 3) +
  scale_x_continuous(breaks = c(0, 3, 6, 9, 12),
                     limits = c(-1, 14),
                     labels = c(2011, 2014, 2017, 2020, 2023)) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 0.5))

ggsave("./R/results/NGPN_plot_level_pctcov_Woody_simp.png", height = 6, width = 8)


# plotting lifeform trend results
col_pal <- c("Forb/herb" = "#36C736", "Graminoid" = "#efdf00", "Graminoid - Inv." = "#C7381C",
             "Woody" = "#279CF5",
             "Bare" = "#735E47", "Litter" = "#C2AF80")#,
             #"Other Non-Vasc" = "#CCCCCC")#,
             #"Not Sign." = "white")

fill_pal <- c("Forb/herb" = "#36C736", "Graminoid" = "#efdf00", "Graminoid - Inv." = "#C7381C",
             "Woody" = "#279CF5",
             "Bare" = "#735E47", "Litter" = "#C2AF80")#,
             #"Other Non-Vasc" = "#CCCCCC")#,
             #"Not Sign." = "white")

label_leg <- c("Forb/herb" = "Forb/herb", "Graminoid" = "Graminoid",
            "Graminoid - Inv." = "Graminoid - Inv.", "Woody" = "Woody",
            "Bare" = "Bare Ground", "Litter" = "Dead Litter")#,
            #"Other Non-Vasc" = "Other Non-Vasc")#,
            #"Not Sign." = "Not Sign.")

shps <- c("Forb/herb" = 21, "Graminoid" = 24, "Graminoid - Inv." = 22,
          "Woody" = 23, "Bare" = 25, "Litter" = 23)#, "Other Non-Vasc" = 24)

shpsz <- c("Forb/herb" = 3, "Graminoid" = 2.5, "Graminoid - Inv." = 2,
           "Woody" = 3, "Bare" = 2.5, "Litter" = 3)#,
#           "Other Non-Vasc" = 2.5)

pctcov_lf_simp_boot2 <- pctcov_lf_simp_boot |> filter(!LifeForm_simp %in% "Other Non-Vasc")
pctcov_lf_simp_boot2$LifeForm_fac <-
  factor(pctcov_lf_simp_boot2$LifeForm_simp,
         levels = c("Forb/herb", "Graminoid", "Graminoid - Inv.", "Woody",
                    "Bare", "Litter"))#, "Other Non-Vasc"))
minyr <- min(pctcov$year)

pctcov_lf_simp_boot3 <-
  left_join(pctcov_lf_simp_boot2, pctcov_lf_simp_boot2  |>  filter(term == "Slope") |>
              mutate(sign = case_when(lower95 > 0 ~ "sign",
                                      upper95 < 0 ~ "sign",
                                      is.na(lower95) ~ "notmod",
                                      TRUE ~ "nonsign")) |>
              select(Unit_Name, LifeForm_simp, sign),
            by = c("Unit_Name", "LifeForm_simp")) |>
  filter(!term %in% c("Intercept", "Slope"))

pctcov_lf_simp_res <- pctcov_lf_simp_boot3 |> filter(!term %in% c("Intercept", "Slope")) |>
  mutate(year = as.numeric(gsub("\\D", "", term)) + minyr)
head(pctcov_lf_simp_res)

col_df <- data.frame(LifeForm_simp = names(col_pal), shpcolor = factor(col_pal))
shp_df <- data.frame(LifeForm_simp = names(shps), shpsymb = shps)
sz_df <- data.frame(LifeForm_simp = names(shpsz), shpsz = shpsz)
lab_df <- data.frame(LifeForm_simp = names(label_leg), lab_leg = factor(label_leg))

pctcov_lf_simp_res2 <- left_join(pctcov_lf_simp_res, col_df, by = "LifeForm_simp")

pctcov_lf_simp_res2$shpfill <- ifelse(pctcov_lf_simp_res2$sign %in% c("nonsign", "notmod"), "white",
                                      paste0(pctcov_lf_simp_res2$shpcolor))

pctcov_lf_simp_res3 <- left_join(pctcov_lf_simp_res2, shp_df, by = "LifeForm_simp")
pctcov_lf_simp_res4 <- left_join(pctcov_lf_simp_res3, sz_df, by = "LifeForm_simp")
pctcov_lf_simp_res5 <- left_join(pctcov_lf_simp_res4, lab_df, by = "LifeForm_simp")

head(pctcov_lf_simp_res5)
str(pctcov_lf_simp_res5)

col_labs <- c("#36C736" = "Forb/herb",
              "#efdf00" = "Graminoid",
              "#C7381C" = "Graminoid - Inv.",
              "#279CF5" = "Woody",
              "#735E47" = "Bare Ground",
              "#C2AF80" = "Dead Litter",
              #"#CCCCCC" = "Other Non-Vasc",
              "white" = "Not Sign.")

fill_labs <- c("#36C736" = "Forb/herb",
              "#efdf00" = "Graminoid",
              "#C7381C" = "Graminoid - Inv.",
              "#279CF5" = "Woody",
              "#735E47" = "Bare Ground",
              "#C2AF80" = "Dead Litter",
              #"#CCCCCC" = "Other Non-Vasc",
              "white" = "Not Sign.")

ggplot(pctcov_lf_simp_res5,
       aes(x = year, y = estimate,
           group = LifeForm_fac, color = LifeForm_fac,
           fill = LifeForm_fac, shape = LifeForm_fac)) +
  geom_ribbon(aes(ymin = lower95, ymax = upper95, fill = LifeForm_fac, color = LifeForm_fac),
              linewidth = 0.5, alpha = 0.2) +
  geom_point(aes(size = LifeForm_fac), show.legend = T) +
  facet_wrap(~Unit_Name) +
  scale_color_manual(guide = "legend", values = col_pal, name = "Life Form", labels = label_leg, drop = F) +
  scale_fill_manual(guide = "legend", values = col_pal, name = "Life Form", labels = label_leg, drop = F) +
  scale_shape_manual(guide = "legend", values = shps, name = "Life Form", labels = label_leg) +
  scale_size_manual(values = shpsz, name = "Life Form", labels = label_leg) +
  theme_bw() + labs(x = NULL, y = "Mean % Cover") +
  theme(legend.position = "right") +
  scale_x_continuous(breaks = c(2011, 2014, 2017, 2020, 2023),
                     limits = c(2010, 2024)) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 0.5)) +
  geom_ribbon(data = pctcov_lf_simp_res5 |> filter(!sign %in% "sign"),
              aes(ymin = lower95, ymax = upper95), fill = "white",
              linewidth = 0.5, show.legend = F) +
  geom_point(data = pctcov_lf_simp_res5 |> filter(!sign %in% "sign"),
             aes(shape = LifeForm_fac, size = LifeForm_fac), fill = "white",
             #color = NULL,
             show.legend = F) +
  geom_line(linewidth = 0.6) +
  geom_ribbon(data = pctcov_lf_simp_res5 |> filter(sign == "sign"),
              aes(ymin = lower95, ymax = upper95, fill = LifeForm_fac, color = LifeForm_fac),
              linewidth = 0.5, alpha = 0.2) +
  geom_point(data = pctcov_lf_simp_res5 |> filter(sign == "sign"),
             aes(size = LifeForm_fac), show.legend = T)


ggsave("./R/results/NGPN_plot_level_pctcov_Life_Form_simp.png", height = 8, width = 10)
