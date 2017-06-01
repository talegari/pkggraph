#' @title Check depends
#' @author Srikanth KS
#' @description Check whether pkg_1 has a dependency on pkg_2
#' @param pkg_1 a package name
#' @param pkg_2 a package name
#' @return TRUE or FALSE
#' @examples
#' pkggraph::init()
#' "dplyr" %depends% "tibble"
#' @export

`%depends%` <- function(pkg_1, pkg_2){
  stopifnot(length(pkg_1) == 1 && is.character(pkg_1))
  stopifnot(length(pkg_2) == 1 && is.character(pkg_2))
  stopifnot(exists("packmeta"))

  pkg_2 %in% tools::package_dependencies(packages = pkg_1
                                         , which  = "Depends"
                                         , db     = packmeta)[[1]]
}

#' @title Check imports
#' @author Srikanth KS
#' @description Check whether pkg_1 has a dependency on pkg_2
#' @param pkg_1 a package name
#' @param pkg_2 a package name
#' @return TRUE or FALSE
#' @examples
#' pkggraph::init()
#' "dplyr" %imports% "tibble"
#' @export

`%imports%` <- function(pkg_1, pkg_2){
  stopifnot(length(pkg_1) == 1 && is.character(pkg_1))
  stopifnot(length(pkg_2) == 1 && is.character(pkg_2))
  stopifnot(exists("packmeta"))

  pkg_2 %in% tools::package_dependencies(packages = pkg_1
                                         , which  = "Imports"
                                         , db     = packmeta)[[1]]
}

#' @title Check linkingto
#' @author Srikanth KS
#' @description Check whether pkg_1 has a dependency on pkg_2
#' @param pkg_1 a package name
#' @param pkg_2 a package name
#' @return TRUE or FALSE
#' @examples
#' pkggraph::init()
#' "dplyr" %linkingto% "tibble"
#' @export

`%linkingto%` <- function(pkg_1, pkg_2){
  stopifnot(length(pkg_1) == 1 && is.character(pkg_1))
  stopifnot(length(pkg_2) == 1 && is.character(pkg_2))
  stopifnot(exists("packmeta"))

  pkg_2 %in% tools::package_dependencies(packages = pkg_1
                                         , which  = "LinkingTo"
                                         , db     = packmeta)[[1]]
}

#' @title Check suggests
#' @author Srikanth KS
#' @description Check whether pkg_1 has a dependency on pkg_2
#' @param pkg_1 a package name
#' @param pkg_2 a package name
#' @return TRUE or FALSE
#' @examples
#' pkggraph::init()
#' "dplyr" %suggests% "tibble"
#' @export

`%suggests%` <- function(pkg_1, pkg_2){
  stopifnot(length(pkg_1) == 1 && is.character(pkg_1))
  stopifnot(length(pkg_2) == 1 && is.character(pkg_2))
  stopifnot(exists("packmeta"))

  pkg_2 %in% tools::package_dependencies(packages = pkg_1
                                         , which  = "Suggests"
                                         , db     = packmeta)[[1]]
}

#' @title Check enhances
#' @author Srikanth KS
#' @description Check whether pkg_1 has a dependency on pkg_2
#' @param pkg_1 a package name
#' @param pkg_2 a package name
#' @return TRUE or FALSE
#' @examples
#' pkggraph::init()
#' "dplyr" %enhances% "tibble"
#' @export

`%enhances%` <- function(pkg_1, pkg_2){
  stopifnot(length(pkg_1) == 1 && is.character(pkg_1))
  stopifnot(length(pkg_2) == 1 && is.character(pkg_2))
  stopifnot(exists("packmeta"))

  pkg_2 %in% tools::package_dependencies(packages = pkg_1
                                         , which  = "Enhances"
                                         , db     = packmeta)[[1]]
}

#' @title Check relies
#' @author Srikanth KS
#' @description Check whether a package has a recursive dependency on the other
#' @param pkg_1 (string) A package name
#' @param pkg_2 (string) A package name
#' @return (flag) TRUE, if 'pkg_1' `relies` on 'pkg_2'
#' @seealso \code{\link{relies}}, \code{\link{reverse_relies}}
#' @examples
#' pkggraph::init()
#' "dplyr" %relies% "tibble"
#' @export
`%relies%` <- function(pkg_1, pkg_2){
  stopifnot(is.character(pkg_1) && length(pkg_1) == 1)
  stopifnot(is.character(pkg_2) && length(pkg_2) == 1)

  pkg_2 %in% relies(pkg_1)[[1]]
}
