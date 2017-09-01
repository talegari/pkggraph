#' @title relies
#' @author Srikanth KS
#' @description Captures recursive dependencies of these types: "Depends",
#'   "Imports", "LinkingTo"
#' @param packages (non-empty character vector) Package names
#' @return (Named list) A name is the package name from 'packages'. A Value is a
#'   character vector of all packages which the package 'relies' (Captures
#'   recursive dependencies of these types: "Depends", "Imports", "LinkingTo")
#' @seealso \code{\link{reverse_relies}}
#' @examples
#' pkggraph::init(local = TRUE)
#' pkggraph::relies("tidytext")
#' @export
relies <- function(packages){

  # assertions ----
  stopifnot(is.character(packages) && length(packages) > 0)
  if(!exists("packmeta")){
    stop("Unable to find `packmeta`. Please run `pkggraph::init(local = TRUE)`.")
  }

  # call with recursive ----
  tools::package_dependencies(packages    = packages
                              , db        = packmeta
                              , which     = c("Depends", "Imports", "LinkingTo")
                              , recursive = TRUE
                              , reverse   = FALSE
                              , verbose   = FALSE)
}

#' @title reverse_relies
#' @author Srikanth KS
#' @description Captures reverse recursive dependencies of these types:
#'   "Depends", "Imports", "LinkingTo"
#' @param packages (non-empty character vector) Package names
#' @return (Named list) A name is the package name from 'packages'. A Value is a
#'   character vector of all packages which the package 'relies' (Captures
#'   reverse recursive dependencies of these types: "Depends", "Imports",
#'   "LinkingTo")
#' @seealso \code{\link{relies}}
#' @examples
#' pkggraph::init(local = TRUE)
#' pkggraph::reverse_relies("data.table")
#' @export
reverse_relies <- function(packages){

  # assertions ----
  stopifnot(is.character(packages) && length(packages) > 0)
  if(!exists("packmeta")){
    stop("Unable to find `packmeta`. Please run `pkggraph::init(local = TRUE)`.")
  }

  # call with recursive ----
  tools::package_dependencies(packages    = packages
                              , db        = packmeta
                              , which     = c("Depends", "Imports", "LinkingTo")
                              , recursive = TRUE
                              , reverse   = TRUE
                              , verbose   = FALSE)
}
