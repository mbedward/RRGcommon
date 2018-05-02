# List of page sizes.
PAGESIZE <- list(
  A3 = list(width = 29.7, height = 42.0, units = "cm"),
  A4 = list(width = 21.0, height = 29.7, units = "cm")
)
devtools::use_data(PAGESIZE, overwrite = TRUE)


#####################
# Site information
SITES <- read.csv("data-raw/site_info.csv", stringsAsFactors = FALSE)

SITES$site.quality <- factor(SITES$site.quality, levels = c("SQ1", "SQ2"))

SITES$year.thinned <- factor(SITES$year.thinned, levels = c(2016, 2017))

SITES$treat <- factor(SITES$treat, levels = c("control", "moderate", "heavy"))

devtools::use_data(SITES, overwrite = TRUE)
