#' @title pkggraph
#'
#' @details See the vignette for further details
#'
#' @importFrom utils available.packages
#' @importFrom utils packageVersion
#' @importFrom tools package_dependencies
#' @importFrom tibble tibble
#' @importFrom dplyr bind_rows
#' @importFrom dplyr arrange_
#' @importFrom igraph graph_from_data_frame
#' @importFrom igraph set_edge_attr
#' @importFrom igraph get.edgelist
#' @importFrom igraph gorder
#' @importFrom igraph V
#' @importFrom igraph get.edge.attribute
#' @importFrom igraph get.adjacency
#' @importFrom network as.network
#' @importFrom network set.edge.attribute
#' @importFrom RColorBrewer brewer.pal
#' @importFrom curl has_internet
#' @importFrom intergraph asNetwork
#' @importFrom htmltools browsable
#' @importFrom htmltools tagList
#' @importFrom htmltools tags
#' @importFrom htmltools HTML
#' @importFrom networkD3 forceNetwork
#' @importFrom Matrix colSums
#' @importFrom Matrix rowSums
#' @importFrom plyr ldply
#' @import ggplot2
#' @import ggnetwork
#' @import data.table
"_PACKAGE"
