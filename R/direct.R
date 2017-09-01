#' @title get_depends
#' @author Srikanth KS
#' @description Get dependencies
#' @param packages (non-empty character vector) Package names
#' @param level (positive integer) Depth of recursive dependency
#' @return A tibble with three columns: `pkg_1`, `relation` and `pkg_2`
#' @seealso \code{\link{get_depends}}, \code{\link{get_imports}},
#'   \code{\link{get_linkingto}}, \code{\link{get_suggests}},
#'   \code{\link{get_enhances}}, \code{\link{get_all_dependencies}},
#'   \code{\link{get_reverse_depends}}
#' @examples
#' pkggraph::init(local = TRUE)
#' pkggraph::get_depends("glmnet")
#' @export
get_depends <- function(packages, level = 1L){
    get_dependencies(packages    = packages
                     , type      = "Depends"
                     , level     = level
                     , reverse   = FALSE)
}

#' @title get_imports
#' @author Srikanth KS
#' @description Get dependencies
#' @param packages (non-empty character vector) Package names
#' @param level (positive integer) Depth of recursive dependency
#' @return A tibble with three columns: `pkg_1`, `relation` and `pkg_2`
#' @seealso \code{\link{get_depends}}, \code{\link{get_imports}},
#'   \code{\link{get_linkingto}}, \code{\link{get_suggests}},
#'   \code{\link{get_enhances}}, \code{\link{get_all_dependencies}},
#'   \code{\link{get_reverse_imports}}
#' @examples
#' pkggraph::init(local = TRUE)
#' pkggraph::get_imports("dplyr")
#' @export
get_imports <- function(packages, level = 1L){
  get_dependencies(packages    = packages
                   , type      = "Imports"
                   , level     = level
                   , reverse   = FALSE)
}

#' @title get_linkingto
#' @author Srikanth KS
#' @description Get dependencies
#' @param packages (non-empty character vector) Package names
#' @param level (positive integer) Depth of recursive dependency
#' @return A tibble with three columns: `pkg_1`, `relation` and `pkg_2`
#' @seealso \code{\link{get_depends}}, \code{\link{get_imports}},
#'   \code{\link{get_linkingto}}, \code{\link{get_suggests}},
#'   \code{\link{get_enhances}}, \code{\link{get_all_dependencies}},
#'   \code{\link{get_reverse_linkingto}}
#' @examples
#' pkggraph::init(local = TRUE)
#' pkggraph::get_linkingto("tibble")
#' @export
get_linkingto <- function(packages, level = 1L){
  get_dependencies(packages    = packages
                   , type      = "LinkingTo"
                   , level     = level
                   , reverse   = FALSE)
}

#' @title get_suggests
#' @author Srikanth KS
#' @description Get dependencies
#' @param packages (non-empty character vector) Package names
#' @param level (positive integer) Depth of recursive dependency
#' @return A tibble with three columns: `pkg_1`, `relation` and `pkg_2`
#' @seealso \code{\link{get_depends}}, \code{\link{get_imports}},
#'   \code{\link{get_linkingto}}, \code{\link{get_suggests}},
#'   \code{\link{get_enhances}}, \code{\link{get_all_dependencies}},
#'   \code{\link{get_reverse_suggests}}
#' @examples
#' pkggraph::init(local = TRUE)
#' pkggraph::get_suggests("knitr")
#' @export
get_suggests <- function(packages, level = 1L){
  get_dependencies(packages    = packages
                   , type      = "Suggests"
                   , level     = level
                   , reverse   = FALSE)
}

#' @title get_enhances
#' @author Srikanth KS
#' @description Get dependencies
#' @param packages (non-empty character vector) Package names
#' @param level (positive integer) Depth of recursive dependency
#' @return A tibble with three columns: `pkg_1`, `relation` and `pkg_2`
#' @seealso \code{\link{get_depends}}, \code{\link{get_imports}},
#'   \code{\link{get_linkingto}}, \code{\link{get_suggests}},
#'   \code{\link{get_enhances}}, \code{\link{get_all_dependencies}},
#'   \code{\link{get_reverse_enhances}}
#' @examples
#' pkggraph::init(local = TRUE)
#' pkggraph::get_enhances("bigmemory")
#' @export
get_enhances <- function(packages, level = 1L){
  get_dependencies(packages    = packages
                   , type      = "Enhances"
                   , level     = level
                   , reverse   = FALSE)
}

#' @title get_all_dependencies
#' @author Srikanth KS
#' @description Get all dependencies
#' @param packages (non-empty character vector) Package names
#' @param level (positive integer, Default = 1L) Depth of recursive dependency
#' @param relation (character vector) Types of relations. Must be a subset of
#'   c("Depends", "Imports", "LinkingTo", "Suggests", "Enhances")
#' @param strict (logical, Default: TRUE) Whether to consider all packages
#'   (alternately only 'relation' specific packages) when computing dependencies
#'   for the next level
#' @param ignore package names to ignore
#' @return A tibble with three columns: `pkg_1`, `relation` and `pkg_2`
#' @seealso \code{\link{get_all_reverse_dependencies}}
#' @examples
#' pkggraph::init(local = TRUE)
#' # general use
#' pkggraph::get_all_dependencies("mlr")
#' # specify two levels
#' pkggraph::get_all_dependencies("mlr", level = 2)
#' # specify relation(s)
#' pkggraph::get_all_dependencies("mlr", level = 2, relation = "Imports")
#' # setting strict to TRUE to only consider 'Imports' of the previous level
#' pkggraph::get_all_dependencies("mlr"
#'                                , level    = 2
#'                                , relation = "Imports"
#'                                , strict   = TRUE)
#' @export
get_all_dependencies <- function(packages
                                 , level = 1L
                                 , relation = c("Depends"
                                                , "Imports"
                                                , "LinkingTo"
                                                , "Suggests"
                                                , "Enhances"
                                                )
                                 , strict = FALSE
                                 , ignore = c("datasets"
                                              , "utils"
                                              , "grDevices"
                                              , "graphics"
                                              , "stats"
                                              , "methods"
                                              )
                                 ){
  # assertions ----
  if(!exists("deptable")){
    stop("Unable to find `deptable`. Please run `pkggraph::init(local = TRUE)`.")
  }
  stopifnot(is.character(packages) && length(packages) > 0)
  stopifnot(length(level) == 1 &&
              level == as.integer(level) &&
              level >= 1L)
  stopifnot(length(relation) > 0 && is.character(relation))
  stopifnot(relation %in% c("Depends"
                            , "Imports"
                            , "LinkingTo"
                            , "Suggests"
                            , "Enhances"))
  stopifnot(is.logical(strict) && length(strict) == 1)
  stopifnot(is.character(packages))
  stopifnot(is.character(ignore))

  # consider 'strict' ----
  if(!strict){
    relation2 <- c("Depends"
                   , "Imports"
                   , "LinkingTo"
                   , "Suggests"
                   , "Enhances")
  } else {
    relation2 <- relation
  }

  # list of dependency frames ----
  ell <- length(relation2)
  for(it in seq_along(1:level)){

    direct_list <- vector("list", length = ell)
    for(it2 in seq_along(1:ell)){
      direct_list[[it2]] <- get_dependencies(packages = packages
                                             , type = relation2[it2]
                                             , assert = FALSE
                                             )
    }
    res      <- dplyr::bind_rows(direct_list)
    packages <- union(res[["pkg_1"]], res[["pkg_2"]])
  }

  # filter depending on 'strict' ----
  if(!strict){
    res <- res[res$relation %in% relation,]
  }

  # handle ignore
  if(length(ignore) > 0){
    res <- res[!(res$pkg_1 %in% ignore) & !(res$pkg_2 %in% ignore),]
  }

  # arrange by relation and return ----
  return(dplyr::arrange_(res, "relation"))
}
