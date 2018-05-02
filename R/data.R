#' RRG site information
#'
#' A data frame of meta-data for sites and plots consisting of:
#' \describe{
#' \item{site}{Site identifier (int)}
#' \item{plot}{Plot identifier (int)}
#' \item{site.quality}{Mapped site quality class (factor: SQ1; SQ2)}
#' \item{year.thinned}{Year of thinning for treatment plots (factor: 2016; 2017).
#'   Control plots have 'NA' values. Plots thinned in 2017 were affected by flooding
#'   just prior to thinning.}
#' \item{treat}{Plot treatment (factor: control; moderate; heavy)}
#' }
"SITES"


#' RRG tree diameter size classes
#'
#' A data frame of size class definitions and identifiers consisting of:
#' \describe{
#' \item{sizeclass}{Column names in tree count data sets
#'   (factor with 11 levels)}
#' \item{index}{Integer size class indices}
#' \item{label}{Alternative short labels for use in graphs etc.
#'   (factor with 11 levels)}
#' }
"SIZES"


#' Page size information for use with the \code{\link{save_plot}} function.
#'
#' A named list with elements 'A3' and 'A4'. Each element is a list with
#' elements 'width', 'height' and 'units'.
"PAGESIZE"

