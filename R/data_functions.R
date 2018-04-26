#' Calculate mid-point stem area from tree size class
#'
#' This function uses representative mid-point diameters for tree size classes
#' (as defined in the \code{\link{SIZES}} table) to calculate stem areas.
#'
#' @param sizes A factor or character vector of tree size classes as per the
#'   'sizeclass' column in the \code{\link{SIZES}} table (e.g. 'from20to30cm').
#'
#' @param counts An option vector of tree counts. If provided it must be either
#'   the same length as 'sizes' or a single value. If not provided, counts of
#'   1 are assumed.
#'
#' @param units Area units: one of 'm2' (default) or 'cm2'.
#'
#' @return A vector of stem area values.
#'
#' @importFrom dplyr %>% left_join
#'
#' @export
#'
calculate_stem_area <- function(sizes, counts = NULL, units = c("m2", "cm2")) {

  units <- match.arg(units)

  nas <- is.na(sizes)

  if (!all(sizes[!nas] %in% RRGcommon::SIZES$sizeclass))
    stop("All values in sizes must be size class levels (e.g. 'from10to20cm')")

  if (is.null(counts)) counts <- 1

  if (length(counts) == 1) counts <- rep(counts, length(sizes))
  else if (length(counts) != length(sizes))
    stop("counts vector has a different length to the sizes vector")

  suppressWarnings(
    x <- left_join(
      data.frame(sizes, counts),
      RRGcommon::SIZES, by = c("sizes" = "sizeclass")
    )
  )

  area <- x$counts * pi * x$midpoint.cm^2 / 4
  if (units == "m2") area <- area / 1e4

  area
}
