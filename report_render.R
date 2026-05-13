# driver_render.R
# Render parameterized reports for each park into HTML or PDF.

library(rmarkdown)
# Choose output format: "html" or "pdf"
OUTPUT_FORMAT <- "html"  # change to "pdf" if you want PDF (requires a LaTeX installation)

parks <- c("AGFO","BADL","DETO","FOLA","FOUS","JECA","KNRI","MORU","SCBL","THRO","WICA")

# Ensure output directory exists
out_dir <- "./reports"

# Map format string to rmarkdown output_format + file extension
fmt_map <- list(
  html = list(format = "html_document", ext = "html"),
  pdf  = list(format = "pdf_document",  ext = "pdf")
)
fmt <- fmt_map[[tolower(OUTPUT_FORMAT)]]
if (is.null(fmt)) stop("OUTPUT_FORMAT must be 'html' or 'pdf'.")

for (pk in parks) {
  cat(sprintf("Rendering %s report for park %s...\n", toupper(OUTPUT_FORMAT), pk))
  render(
    input = "annual_report.Rmd",
    params = list(park = pk),
    # output_format = fmt$format,         # pick HTML or PDF
    output_file = sprintf("PCM_Report_%s.%s", pk, fmt$ext),
    output_dir = out_dir,
    clean = TRUE,
    quiet = TRUE
  )
}
cat(sprintf("All reports written to: %s\n", normalizePath(out_dir)))
