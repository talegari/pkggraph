% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/get_neighborhood.R
\name{make_neighborhood_graph}
\alias{make_neighborhood_graph}
\title{make_neighborhood_graph}
\usage{
make_neighborhood_graph(ndf, type = "igraph")
}
\arguments{
\item{ndf}{(tibble) Output by functions like `get_neighborhood`,
`get_all_dependents` etc}

\item{type}{(string, Default: "igraph") Graph object type. Either "network"
or "igraph"}
}
\value{
A network or igraph graph object
}
\description{
Make a network or igraph graph object of dependencies and
  reverse dependencies from tibble output by functions like
  `get_neighborhood`, `get_all_dependents`etc
}
\examples{
pkggraph::init(local = TRUE)
graph_object <- pkggraph::get_neighborhood("caret")
pkggraph::make_neighborhood_graph(graph_object)
}
\seealso{
\code{\link{neighborhood_graph}},
  \code{\link{get_neighborhood}}
}
\author{
Srikanth KS
}
