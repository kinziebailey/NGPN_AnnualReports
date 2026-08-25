# This is the file that runs all the data wrangling for the
# PlantComunity_AnnualReport.Rmd. If the Rmd does not knit, this is
# where the error will come from. The portion of the code that is
# commented out will need to be run for any troubleshooting.

# Libraries ----

# getting newest version of NGPN package
if (!requireNamespace("plantcomNGPN", quietly = TRUE)) {
  pak::pak("KateMMiller/plantcomNGPN")
}

# libraries ----
library(plantcomNGPN) # getting plant comm data
library(dplyr) # data wrangling
library(tidyr) # data wrangling
library(stringr) # data wrangling
library(ggplot2) # plotting
library(purrr) # list options
library(knitr) # for kable
library(kableExtra) # custom kable features
library(htmltools) # loading html files
library(sf) # converting UTM to WGS84 lat lon
library(DT) # for data table

# loading data ----
# to catch if it is already loaded in loop
if(!exists("VIEWS_NGPN")){
  importViews(import_path = "./data/NGPN_FFI_views_20260616.zip",
            new_env = TRUE)
}

# Start of Source Code ----

## Data wrangling ----
### macro plots
macro <- getMacroPlot() |>
  select(MacroPlot_Name,
         vegtype = MacroPlot_UV4,
         Datum,
         MacroPlot_GUID)

### cover point
covpts1 <- getCoverPoints()

### taxonomy
taxon <- VIEWS_NGPN$Taxa_Table |>
  dplyr::select(Symbol,
                ScientificName,
                CommonName,
                Family,
                Genus,
                Nativity,
                LifeForm_Name,
                Spp_GUID) |>
  # changing grass-like to graminoid
  mutate(LifeForm_Name = ifelse(LifeForm_Name == "Grass-like",
                                "Graminoid",
                                LifeForm_Name)) |>
  # changing undefined to NA
  mutate(LifeForm_Name = ifelse(LifeForm_Name == "Undefined",
                                NA_character_,
                                LifeForm_Name)) |>
  # filling in lifeform by species code
  fill(LifeForm_Name,
       .by = Symbol) |>
  mutate(LifeForm_Name = ifelse(is.na(LifeForm_Name),
                                "Not Defined",
                                LifeForm_Name)) |>
  distinct()

# wrangling cover points
covpts2 <- covpts1 |>
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
         Height,
         MacroPlot_GUID,
         Spp_GUID) |>
  # nremoving NA symbol
  filter(!is.na(Symbol)) |>
  # getting distinct values
  distinct() |>
  # filtering weird transects
  filter(Transect %in% 1:2) |>
  # fixing year
  mutate(year = as.integer(year))

# joining
covpts3 <- left_join(covpts2,
                     macro) |>
  # keep plots with grassland or badlands sparse
  filter(grepl("UG|BS", vegtype)) |>
  # drop plots with Ponderosa pine also in vegtype
  filter(!grepl("PP", vegtype)) |>
  # joining taxon table
  left_join(taxon) |>
  distinct() |>
  select(-MacroPlot_GUID,
         -Spp_GUID)

# correcting lifeform
covpts4 <- covpts3 |>
  # changing LifeForm names
  mutate(LifeForm_simp = case_when(Symbol == "BARE" ~ "Bare",
                                   Symbol == "LITT" ~ "Litter",
                                   # non-vascular
                                   !Symbol %in% c("BARE", "LITT") &
                                     LifeForm_Name == "Nonvascular" ~ "Other Non-Vasc",
                                   # graminoids
                                   LifeForm_Name == "Graminoid" & Nativity == FALSE ~ "Graminoid - Non Native",
                                   LifeForm_Name == "Graminoid" & Nativity == TRUE ~ "Graminoid - Native",
                                   # woody
                                   LifeForm_Name %in% c("Shrub", "Subshrub", "Tree", "Vine") &
                                     Nativity == TRUE ~ "Woody - Native",
                                   LifeForm_Name %in% c("Shrub", "Subshrub", "Tree", "Vine") &
                                     Nativity == FALSE ~ "Woody - Non Native",
                                   # forb/herb
                                   LifeForm_Name == "Forb/herb" & Nativity == FALSE ~ "Forb/herb - Non Native",
                                   LifeForm_Name == "Forb/herb" & Nativity == TRUE ~ "Forb/herb - Native",
                                   # other
                                   TRUE ~ LifeForm_Name)) |>
  # removing undefined (e.g. rock, crust, moss, etc.)
  filter(!LifeForm_simp %in% "Undefined")

### Calculating relative abundance ----
# Absolute cover values (including bare ground): cover values that reflect whole plot
rel_abund <- covpts4 |>
  # getting total number of points
  mutate(total_points = n_distinct(Point),
         .by = c("Unit_Name",
                 "MacroPlot_Name",
                 "year",
                 "Transect",
                 "UTM_X",
                 "UTM_Y",
                 "UTMzone")) |>
  # getting the distinct number of points for each lifeform per grouping
  summarise(lifeform_hits = n_distinct(Point),
            .by = c("Unit_Name",
                    "MacroPlot_Name",
                    "year",
                    "Transect",
                    "UTM_X",
                    "UTM_Y",
                    "UTMzone",
                    "LifeForm_simp",
                    "total_points")) |>
  # calculating relative abundance
  mutate(rel_abundance = (lifeform_hits / total_points) * 100) |>
  # plot level abundance
  summarise(rel_abundance_mean = mean(rel_abundance),
            rel_abundance_sd = sd(rel_abundance),
            .by = c("Unit_Name",
                    "MacroPlot_Name",
                    "year",
                    "UTM_X",
                    "UTM_Y",
                    "UTMzone",
                    "LifeForm_simp"))

## Current year samples ----
abundance_current <- rel_abund |>
  dplyr::filter(year == 2025)
# dplyr::filter(year == as.integer(format(Sys.Date(), "%Y")))

# Historic Data
abundance_historic <- rel_abund |>
  dplyr::filter(year < 2025)
# dplyr::filter(year < as.integer(format(Sys.Date(), "%Y")))

## Data Prep Functions ----

### map
prep_map <- function(df){
  df |>
    # selecting coord data
    select(MacroPlot_Name,
           UTM_X,
           UTM_Y,
           UTMzone,
           Datum) |>
    # getting distinct values
    distinct() |>
    # removing NA
    filter(!is.na(UTM_X),
           !is.na(UTM_Y),
           !is.na(UTMzone)) |>
    # UTM information
    mutate(zone = as.integer(gsub("[^0-9]", "", UTMzone)),
           hemi = ifelse(grepl("[sS]", UTMzone), "S", "N"),
           # correcting for now
           Datum = case_when(grepl("NAD83", Datum, ignore.case = TRUE) ~ "NAD83",
                             grepl("WGS84", Datum, ignore.case = TRUE) ~ "WGS84",
                             grepl("ITRF", Datum, ignore.case = TRUE) ~ "WGS84",   # treat as WGS84
                             TRUE ~ NA_character_),
           epsg = case_when(Datum == "NAD83" & hemi == "N" ~ 26900 + zone,
                            Datum == "WGS84" & hemi == "N" ~ 32600 + zone,
                            TRUE ~ NA_real_)) |>
    # removing NA
    filter(!is.na(epsg)) |>
    # getting one sf per EPSG
    group_split(epsg) |>
    # creating sf per group
    map(~st_as_sf(.x, coords = c("UTM_X",
                                 "UTM_Y"),
                  crs = unique(.x$epsg))) |>
    # transforming to same CRS
    map(~st_transform(.x, 4326)) |>
    # binding rows
    bind_rows() |>
    # getting lat lon
    mutate(lon = st_coordinates(geometry)[,1],
           lat = st_coordinates(geometry)[,2])

}

### species list
prep_species <- function(df){
  df |>
    # selecting needed columns
    select(Family,
           Symbol,
           ScientificName,
           CommonName,
           Nativity) |>
    # getting unique rows
    distinct() |>
    # sorting by Family, SciName, Nativity
    arrange(Family,
            ScientificName,
            Nativity) |>
    # renaming symbol
    rename(`Scientific Name` = ScientificName,
           Code = Symbol)
}

## Functions ----
### map
create_map <- function(df){

  ggplot() +
  geom_sf(data = df,
          color = "#2c7fb8",
          size = 2) +
    ggrepel::geom_text_repel(data = df,
                             aes(x = lon,
                                 y = lat,
                                 label = MacroPlot_Name),
                             size = 3) +
    theme_minimal()
}

### species table
species_table <- function(df){
  datatable(df,
            class = 'cell-border stripe',
            rownames = FALSE,
            extensions = c("FixedColumns", "Buttons"),
            options = list(
              pageLength = nrow(df),
              autoWidth = FALSE,
              scrollX = '850px',
              scrollY = '600px',
              scrollCollapse = TRUE,
              fixedColumns = list(leftColumns = 1),
              dom = "Blfrtip",
              buttons = c('copy', 'csv', 'print')
            ),
            filter = list(position = c('top'),
                          clear = FALSE))

}


