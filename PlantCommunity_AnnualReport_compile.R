# Libraries ----
library(readr) # tidyverse data import
library(dplyr) # data wrangling
library(ggplot2) # plotting
library(knitr) # for kable
library(kableExtra) # custom kable features
library(htmltools) # loading html files
library(plantcomNGPN) # getting plant comm data
library(tidyr) # data wrangling
library(ggbasemap) # for mapping
library(stringr) # data wrangling

# Functions ----
## map
create_map <- function(df, park){
 ggplot() +
    geom_point(data = df,
               aes(x = UTM_X,
                   y = UTM_Y))
}

## species table
species_table <- function(df, park){

}

# Loading data ----

# path to data
data_path <- file.path("./data",
                       format(Sys.Date(), '%Y'),
                       "NGPN_FFI_views_20260616.zip")

importViews(import_path = data_path)

# panel schedules
panel_sch_wide <- read.csv("./data/panel_schedule.csv", na.strings = "")

# pivot to longer
panel_sch <- panel_sch_wide |>
  pivot_longer(!Year,
               names_to = "Panel") |>
  drop_na() |>
  # filtering to current date (will update every year)
  filter(Year <= as.integer(format(Sys.Date(), "%Y"))) |>
  select(Year,
         Panel)

# THRO
panel_sch_wide_thro <- read.csv("./data/THRO_panel_schedule.csv", na.strings = "")

# pivot to longer
panel_sch_thro <- panel_sch_wide_thro |>
  pivot_longer(!Year,
               names_to = "Panel") |>
  drop_na() |>
  # filtering to current date (will update every year)
  filter(Year <= as.integer(format(Sys.Date(), "%Y"))) |>
  select(Year,
         Panel)

# Data wrangling ----
## macro plots
macro <- getMacroPlot() |>
  select(MacroPlot_Name,
         vegtype = MacroPlot_UV4)

## cover point
covpts1 <- getCoverPoints()

## taxonomy
taxon <- VIEWS_NGPN$Taxa_Table |>
  dplyr::select(Symbol,
                ScientificName,
                # CommonName,
                Family,
                Genus,
                Nativity) |>
  distinct()

# cleaning up taxon
taxon_clean <- taxon |>
  dplyr::mutate(
    # cleaning up white space and converting blanks to NA
    across(where(is.character),
                 ~ na_if(trimws(.),
                         "")),
    # ScientificName fixes
    ScientificName = case_when(
      Symbol == "ACAM99" ~ "Acmispon americanus",
      Symbol == "BARE" ~ "bare",
      Symbol == "BOECH99" ~ "Boechera",
      Symbol == "BOLE" ~ "bole",
      Symbol == "CANOPY" ~ "Canopy",
      Symbol == "CRUST" ~ "Crust",
      Symbol == "DUFF" ~ "Duff",
      Symbol == "FORB" ~ "Forb",
      Symbol == "GRAMINOID" ~ "Graminoid",
      Symbol == "LITT" ~ "Litter",
      Symbol == "MOSS" ~ "Moss",
      Symbol == "NOSP" ~ "No species",
      Symbol == "PIMI7" ~ "Piptatherum micranthum", # says unknown?
      Symbol == "ROCK" ~ "Rock",
      Symbol == "ROOT" ~ "Root",
      Symbol == "SCAT" ~ "Scat",
      Symbol == "SHRUB" ~ "Shrub",
      Symbol == "SYWY99" ~ "Synthyris wyomingensis",
      Symbol == "TRADE" ~ "Tradescantia",
      Symbol == "UNKGRAM" ~ "Unknown graminoid",
      Symbol == "VEG" ~ "Unknown veg",
      Symbol == "WATER" ~ "Water",
      Symbol == "WOOD" ~ "Wood",
      Symbol == "XASP99" ~ "Xanthium spinulosum",
      TRUE ~ ScientificName),
    # Family name fixes
    Family = case_when(
      Symbol == "ARPY4" ~ "Brassicaceae",
      Symbol == "BARE" ~ "bare",
      Symbol == "BRJA" ~ "Poaceae",
      Symbol == "BOCO4" ~ "Brassicaceae",
      Symbol == "BEDROCK" ~ "BEDROCK",
      Symbol == "BOLE" ~ "bole",
      Symbol == "CAFO3" ~ "Cyperaceae",
      Symbol == "CANOPY" ~ "Canopy",
      Symbol == "CRUST" ~ "Crust",
      Symbol == "DICA18" ~ "Asteraceae",
      Symbol == "DRFI3" ~ "Rosaceae",
      Symbol == "DUFF" ~ "Duff",
      Symbol == "ERAS2" ~ "Brassicaceae",
      Symbol == "ERHO13" ~ "Caryophyllaceae",
      Genus == "Euphorbia" ~ "Euphorbiaceae",
      Symbol == "FACO" ~ "Polygonaceae",
      Symbol == "FORB" ~ "Forb",
      Symbol == "GRAMINOID" ~ "Graminoid",
      Symbol == "LITT" ~ "Litter",
      Symbol == "MOSS" ~ "Moss",
      Symbol == "MUSIN" ~ "Apiaceae",
      Symbol == "NOSP" ~ "No species",
      Genus == "Oenothera" ~ "Onagraceae",
      Symbol == "PIMI7" ~ NA,
      Symbol == "POIN" ~ "Poaceae",
      Symbol == "PRTR4" ~ "Liliaceae",
      Symbol == "ROCK" ~ "Rock",
      Symbol == "ROOT" ~ "Root",
      Symbol == "SCAT" ~ "Scat",
      Symbol == "SHRUB" ~ "Shrub",
      Symbol == "SOPT3" ~ "Solanaceae",
      Symbol == "SORI2" ~ "Asteraceae",
      Symbol == "SYWY99" ~ "Plantaginaceae",
      Symbol == "TOVE2" ~ "Melanthiaceae",
      Symbol == "TUGL" ~ "Brassicaceae",
      Symbol == "UNKFORB" ~ "Unknown family",
      Symbol == "UNKFORBANN" ~ "Unknown family",
      Symbol == "UNKFORBPER" ~ "Unknown family",
      Symbol == "UNKGRAM" ~ "Unknown family",
      Symbol == "UNKGRAMANN" ~ "Unknown family",
      Symbol == "UNKGRAMPER" ~ "Unknown family",
      Symbol == "VEG" ~ "Unknown family",
      Symbol == "WATER" ~ "Water",
      Symbol == "WOOD" ~ "Wood",
      TRUE ~ Family),
    # Genus fixes
    Genus = case_when(
      Symbol == "BARE" ~ "bare",
      Symbol == "BEDROCK" ~ "BEDROCK",
      Symbol == "BOLE" ~ "bole",
      Symbol == "BRJA" ~ "Bromus",
      Symbol == "CAFO3" ~ "Carex",
      Symbol == "CANOPY" ~ "Canopy",
      Symbol == "CRUST" ~ "Crust",
      Symbol == "DUFF" ~ "Duff",
      Symbol == "EUSE5" ~ "Euphorbia",
      Symbol == "FORB" ~ "Forb",
      Symbol == "GRAMINOID" ~ "Graminoid",
      Symbol == "LITT" ~ "Litter",
      Symbol == "MOSS" ~ "Moss",
      Symbol == "NOSP" ~ "No species",
      Symbol == "OESE3" ~ "Oenothera",
      Symbol == "PIMI7" ~ "Piptatherum",
      Symbol == "ROCK" ~ "Rock",
      Symbol == "ROOT" ~ "Root",
      Symbol == "SCAT" ~ "Scat",
      Symbol == "SHRUB" ~ "Shrub",
      Symbol == "SYWY99" ~ "Synthyris",
      Symbol == "UNKFORB" ~ "Unknown genus",
      Symbol == "UNKFORBANN" ~ "Unknown genus",
      Symbol == "UNKFORBPER" ~ "Unknown genus",
      Symbol == "UNKGRAM" ~ "Unknown genus",
      Symbol == "UNKGRAMANN" ~ "Unknown genus",
      Symbol == "UNKGRAMPER" ~ "Unknown genus",
      Symbol == "VEG" ~ "Unknown genus",
      Symbol == "WATER" ~ "Water",
      Symbol == "WOOD" ~ "Wood",
      Symbol == "XASP99" ~ "Xanthium",
      TRUE ~ Genus),
    # Nativity fixes
    Nativity = case_when(
      ScientificName == "Amaranthus retroflexus" ~ FALSE,
      Symbol == "BOST4" ~ TRUE,
      Symbol == "DALEA" ~ TRUE,
      Symbol == "ERIGE2" ~ TRUE,
      Symbol == "FEOV" ~ FALSE,
      Symbol == "HIERA" ~ TRUE,
      Symbol == "JUNCU" ~ TRUE,
      Symbol == "MOSS" ~ FALSE,
      Symbol == "PHYSA" ~ TRUE,
      Symbol == "PIMI7" ~ TRUE,
      Symbol == "POLYG4" ~ TRUE,
      Symbol == "RIBES" ~ TRUE,
      Symbol == "SOPT3" ~ TRUE,
      Symbol == "SYMPH4" ~ TRUE,
      Symbol == "TRADE" ~ TRUE,
      Symbol == "UNKSHRUB" ~ FALSE,
      TRUE ~ Nativity)) |>
  distinct() |>
  # removing XXXX data
  dplyr::filter(!Symbol == "XXXX")

taxon_check <- taxon_clean |>
  group_by(Symbol) |>
  filter(n() > 1)

## merging and filtering panel schedule
covpt_filter <- bind_rows(covpts1 |>
                            filter(!Unit_Name == "THRO") |>
                            semi_join(panel_sch,
                                      by = c("MacroPlot_Purpose" = "Panel",
                                             "year" = "Year")),
                          covpts1 |>
                            filter(Unit_Name == "THRO") |>
                            semi_join(panel_sch_thro,
                                      by = c("MacroPlot_Purpose" = "Panel",
                                             "year" = "Year")))

# removing symbol NA and selecting columns
covpts <- covpt_filter |>
  select(MacroPlot_Name,
         Unit_Name,
         UTM_X,
         UTM_Y,
         UTMzone,
         year,
         month,
         doy,
         Symbol,
         NumTran,
         TranLen,
         NumPtsTran,
         Transect,
         Point,
         Tape,
         Order,
         Height) |>
  filter(!is.na(Symbol)) |>
  distinct() |>
  filter(Transect %in% 1:2)

covpts$year <- as.numeric(covpts$year)

# joining
macro_covpts <- left_join(covpts,
                          macro,
                          by = join_by(MacroPlot_Name)) |>
  # keep plots with grassland or badlands sparse
  filter(grepl("UG|BS", vegtype)) |>
  # drop plots with Ponderosa pine also in vegtype
  filter(!grepl("PP", vegtype))|>
  # only has 2 UG plots
  filter(!Unit_Name %in% "JECA") |>
  # joining taxon table
  left_join(taxon) |>
  distinct()

# Current year samples
covpts_current <- macro_covpts |>
  dplyr::filter(year == 2025)
  # dplyr::filter(year == as.integer(format(Sys.Date(), "%Y")))
