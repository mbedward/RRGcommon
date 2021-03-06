% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/general_functions.R
\name{load_from}
\alias{load_from}
\title{Load a saved R object to a chosen name}
\usage{
load_from(.path)
}
\arguments{
\item{.path}{The path to a file previously created with the \code{save} function.}
}
\value{
If the data file contains a single object it is returned. If there are
  multiple objects these are returned as a list.
}
\description{
This function allows you to load a saved object from a file (e.g. .RData file)
with control over the name of the loaded object. The base R function \code{load}
returns the object under the name that it was saved but this is often
inconvenient.
}
\examples{
\dontrun{
# We have a file 'foo.RData' that contains an object saved as 'foo' but we
# want to load this object under a more informative name.
DAT.newname <- load_from("c:/somedir/foo.RData")
}

}
