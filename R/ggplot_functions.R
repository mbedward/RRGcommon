### Functions and objects to use with ggplot.

#' Save a ggplot object to file.
#'
#' @param ggobj The ggplot object to save.
#'
#' @param path The path and name of the destination file. This should include the extension
#'   (e.g. 'foo.pdf') as this is used to determine the file format.
#'
#' @param pagesize A list of page size information with elements 'width', 'height' and
#'   'units'. Use the \code{\link{PAGESIZE}} object for A3 and A4 information.
#'
#' @param orientation One of 'portrait' or 'landscape' (may be abbreviated).
#'
#' @examples
#' \dontrun{
#' save_plot(last_plot(), "treedata.pdf", PAGESIZE$A4, orientation = "land")
#' }
#'
#' @importFrom ggplot2 ggsave
#'
#' @export
#'
save_plot <- function(ggobj, path, pagesize, orientation = c("portrait", "landscape")) {
  orientation <- match.arg(orientation)
  if (orientation == "portrait") {
    w <- pagesize$width
    h <- pagesize$height
  }
  if (orientation == "landscape") {
    w <- pagesize$height
    h <- pagesize$width
  }

  ggsave(filename = path,
         plot = ggobj,
         width = w,
         height = h,
         units = pagesize$units)
}
