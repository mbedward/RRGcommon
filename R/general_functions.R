#' Load a saved R object to a chosen name
#'
#' This function allows you to load a saved object from a file (e.g. .RData file)
#' with control over the name of the loaded object. The base R function \code{load}
#' returns the object under the name that it was saved but this is often
#' inconvenient.
#'
#' @param .path The path to a file previously created with the \code{save} function.
#'
#' @return If the data file contains a single object it is returned. If there are
#'   multiple objects these are returned as a list.
#'
#' @examples
#' \dontrun{
#' # We have a file 'foo.RData' that contains an object saved as 'foo' but we
#' # want to load this object under a more informative name.
#' DAT.newname <- load_from("c:/somedir/foo.RData")
#' }
#'
#' @export
#'
load_from <- function(.path) {
  load(.path)
  obs <- ls(all.names = FALSE)
  if (length(obs) == 1) get(obs[1], inherits = FALSE)
  else if (length(obs) > 1) mget(obs, inherits = FALSE)
  else NULL
}
