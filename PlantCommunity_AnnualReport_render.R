# This is the document that renders the park specific report for:
# PlantCommunity_AnnualReport.Rmd
# PlantCommunity_AnnualReport_compile.r

# libraries
library(rmarkdown)

# parks
parks <- c("AGFO",
           "BADL",
           "DETO",
           # "JECA",
           "KNRI",
           # "MORU",
           "SCBL",
           "THRO",
           "WICA")

current_year <- 2025
  # format(Sys.Date(), "%Y")

# creating year file
if(!file.exists(file.path("./reports",
                          current_year))) {
  dir.create(file.path("./reports",
                       current_year))
  message("Folder created: ",
          file.path("./reports",
                    current_year))
} else{
  message("Folder already exists: ",
          normalizePath(file.path("./reports",
                                  current_year)))
}

for(p in parks){

  message(paste0("Rendering report for ",
                 p))

  # render the correct Rmd file
  render(input = "PlantCommunity_AnnualReport.Rmd",
         # Name output
         output_file = paste0("PlantCommunity_AnnualReport_",
                              p,
                              format(Sys.Date(), "%Y"),
                              ".html"),
         #output director
         output_dir = file.path("./reports",
                                current_year),
         # start each render with clean environment
         envir = new.env(),
         quiet = TRUE)
}
cat(sprintf("All reports written to: %s\n", normalizePath(out_dir)))
