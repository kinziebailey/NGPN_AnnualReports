library(rmarkdown)

park <- c("AGFO", "AGFO", "BADL", "DETO", "BADL", "DETO")
cover <- c("12", "11", "16", "13", "12", "17")
species <- c("brja", "brja", "brja", "brdi", "brdi","brdi")

park_data <- data.frame(park = park,
                        cover = cover,
                        species = species)



parks <- unique(park_data$park)

for (p in parks) {

  # Create a folder for park (it if doesn't already exist)

  message("Creating folder for: ", p)
  dir.create(p, showWarnings = FALSE)

  # Render the report into park folder

  message("Rendering report for: ", p)
  render(
    "docs/Annual_Report.Rmd",
    params = list(park = p),
    output_file = paste0(p, "report.html"),
    output_dir = p,
    knit_root_dir = getwd(),
    envir = new.env()
  )
}
