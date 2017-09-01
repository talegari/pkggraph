#' @title get_neighborhood
#' @author Srikanth KS
#' @description Obtain dependencies and reverse dependencies of packages at a
#'   given depth of recursion
#' @param packages (non-empty character vector) Package names
#' @param level (positive integer, Default: 1L) Depth of recursive dependency
#' @param relation (character vector) Types of relations. Must be a subset of
#'   c("Depends", "Imports", "LinkingTo", "Suggests", "Enhances")
#' @param strict (logical, Default: TRUE) Whether to consider all packages
#'   (alternately only 'relation' specific packages) when computing dependencies
#'   for the next level
#' @param interconnect (flag, Default: TRUE) Whether to capture dependency among
#'   packages (of a given level) of the next level (See examples)
#' @param ignore package names to ignore
#' @return A tibble with three columns: `pkg_1`, `relation` and `pkg_2`
#' @seealso \code{\link{neighborhood_graph}},
#'   \code{\link{make_neighborhood_graph}}
#' @examples
#' # explore first level dependencies
#' pkggraph::init(local = TRUE)
#' pkggraph::get_neighborhood("caret")
#'
#' # explore second level dependencies
#' pkggraph::get_neighborhood("caret", level = 2)
#'
#' # explore second level dependencies without
#' # considering dependencies from third level
#' pkggraph::get_neighborhood("caret", level = 2, interconnect = FALSE)
#'
#' # explore first level dependencies of multiple packages
#' # and consider second level dependencies
#' get_neighborhood(c("caret", "mlr"))
#'
#' # get 'imports' specific neighborhood of 'mlr' package with strict = TRUE
#' get_neighborhood("mlr"
#'                  , level        = 2
#'                  , strict       = TRUE
#'                  , interconnect = FALSE
#'                  , relation     = "Imports")
#'
#' # get 'imports' specific neighborhood of 'mlr' package with strict = FALSE
#' get_neighborhood("mlr"
#'                  , level        = 2
#'                  , strict       = FALSE
#'                  , interconnect = FALSE
#'                  , relation     = "Imports")
#' @export
get_neighborhood <- function(packages
                             , level        = 1L
                             , relation     = c("Depends"
                                                , "Imports"
                                                , "LinkingTo"
                                                , "Suggests"
                                                , "Enhances")
                             , strict       = FALSE
                             , interconnect = TRUE
                             , ignore = c("datasets"
                                          , "utils"
                                          , "grDevices"
                                          , "graphics"
                                          , "stats"
                                          , "methods"
                             )){

  stopifnot(is.logical(interconnect) && length(interconnect) == 1)

  res <- dplyr::bind_rows(get_all_dependencies(packages   = packages
                                               , level    = level
                                               , relation = relation
                                               , strict   = strict
                                               , ignore   = ignore
                                               )
                          , get_all_reverse_dependencies(packages   = packages
                                                         , level    = level
                                                         , relation = relation
                                                         , strict   = strict
                                                         , ignore   = ignore
                                                         )
                          )

  if(!interconnect){
    return(res)
  } else {
    packages   <- union(res[["pkg_1"]], res[["pkg_2"]])
    ell        <- length(relation)
    res_direct <- vector("list", length = ell)

    for(it in seq_along(1:ell)){
      res_direct[[it]] <- get_dependencies(packages = packages
                                           , type   = relation[it]
                                           , assert = FALSE
                                           )
    }

    res_direct <- dplyr::bind_rows(res_direct)

    res_indirect <- vector("list", length = ell)

    for(it in seq_along(1:ell)){
      res_indirect[[it]] <- get_dependencies(packages  = packages
                                           , type    = relation[it]
                                           , assert  = FALSE
                                           , reverse = TRUE)
    }

    res_indirect <- dplyr::bind_rows(res_indirect)

    rf <- dplyr::bind_rows(res_direct, res_indirect)

    return(unique(rf[rf$pkg_1 %in% packages & rf$pkg_2 %in% packages, ]))
  }
}

#' @title neighborhood_graph
#' @author Srikanth KS
#' @description Obtain a network or igraph graph object of dependencies and
#'   reverse dependencies of packages at a given depth of recursion
#' @param packages (non-empty character vector) Package names
#' @param level (positive integer, Default: 1L) Depth of recursive dependency
#' @param type (string, Default: "igraph") Graph object type. Either "network"
#'   or "igraph"
#' @param relation (character vector) Types of graph edges. Must be a subset of
#'   c("Depends", "Imports", "LinkingTo", "Suggests", "Enhances")
#' @param strict (logical, Default: TRUE) Whether to consider all packages
#'   (alternately only 'relation' specific packages) when computing dependencies
#'   for the next level
#' @param interconnect (flag, Default: TRUE) Whether to capture dependency among
#'   packages (of a given level) of the next level (See examples)
#' @param ignore package names to ignore
#' @return A network or igraph graph object
#' @seealso \code{\link{get_neighborhood}},
#'   \code{\link{make_neighborhood_graph}}
#' @examples
#' # explore first level dependencies
#' pkggraph::init(local = TRUE)
#' pkggraph::neighborhood_graph("caret")
#'
#' # explore second level dependencies of class network
#' pkggraph::neighborhood_graph("caret", level = 2, type = "network")
#'
#' # get 'imports' specific neighborhood of 'mlr' package with strict = TRUE
#' neighborhood_graph("mlr"
#'                    , level        = 2
#'                    , strict       = TRUE
#'                    , interconnect = FALSE
#'                    , relation     = "Imports")
#'
#' # get 'imports' specific neighborhood of 'mlr' package with strict = FALSE
#' neighborhood_graph("mlr"
#'                    , level        = 2
#'                    , strict       = FALSE
#'                    , interconnect = FALSE
#'                    , relation     = "Imports")
#' @export
neighborhood_graph <- function(packages
                               , level        = 1L
                               , type         = "igraph"
                               , relation     = c("Depends"
                                                  , "Imports"
                                                  , "LinkingTo"
                                                  , "Suggests"
                                                  , "Enhances")
                               , strict       = FALSE
                               , interconnect = TRUE
                               , ignore = c("datasets"
                                            , "utils"
                                            , "grDevices"
                                            , "graphics"
                                            , "stats"
                                            , "methods"
                               )
                               ){

  stopifnot(type %in% c("igraph", "network") && length(type) == 1)

  # get neighborhood data
  ndf <- get_neighborhood(packages       = packages
                          , level        = level
                          , relation     = relation
                          , strict       = strict
                          , interconnect = interconnect
                          , ignore       = ignore
                          )

  # create an igraph object
  if(type == "igraph"){
    ng <- igraph::graph_from_data_frame(ndf[,c("pkg_1","pkg_2")]
                                        , directed = TRUE)
    ng <- igraph::set_edge_attr(graph   = ng
                                , name  = "relation"
                                , value = as.character(ndf[["relation"]]))
  }

  # create network object
  if(type == "network"){
    ng <- network::as.network(as.matrix(ndf[, c("pkg_1", "pkg_2")]))
    network::set.edge.attribute(ng
                                , "relation"
                                , as.character(ndf[["relation"]]))
  }

  go        <- list(graph_object = ng)
  class(go) <- c("pkggraph")
  return(go)
}

#' @title make_neighborhood_graph
#' @author Srikanth KS
#' @description Make a network or igraph graph object of dependencies and
#'   reverse dependencies from tibble output by functions like
#'   `get_neighborhood`, `get_all_dependents`etc
#' @param ndf (tibble) Output by functions like `get_neighborhood`,
#'   `get_all_dependents` etc
#' @param type (string, Default: "igraph") Graph object type. Either "network"
#'   or "igraph"
#' @return A network or igraph graph object
#' @seealso \code{\link{neighborhood_graph}},
#'   \code{\link{get_neighborhood}}
#' @examples
#' pkggraph::init(local = TRUE)
#' graph_object <- pkggraph::get_neighborhood("caret")
#' pkggraph::make_neighborhood_graph(graph_object)
#' @export
make_neighborhood_graph <- function(ndf, type = "igraph"){

  stopifnot(inherits(ndf, "tbl_df"))
  stopifnot(colnames(ndf) == c("pkg_1", "relation", "pkg_2"))
  stopifnot(type %in% c("igraph", "network") && length(type) == 1)

  # create an igraph object
  if(type == "igraph"){
    ng <- igraph::graph_from_data_frame(ndf[ , c("pkg_1","pkg_2")]
                                        , directed = TRUE)
    ng <- igraph::set_edge_attr(graph   = ng
                                , name  = "relation"
                                , value = as.character(ndf[["relation"]]))
  }

  # create network object
  if(type == "network"){
    ng <- network::as.network(as.matrix(ndf[, c("pkg_1", "pkg_2")]))
    network::set.edge.attribute(ng
                                , "relation"
                                , as.character(ndf[["relation"]]))
  }

  go        <- list(graph_object = ng)
  class(go) <- c("pkggraph")
  return(go)
}
