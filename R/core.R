#' @name get_dependencies
#' @title Get (reverse) dependencies of a set of packages
#' @description
#'  Get (reverse) dependencies of a set of packages till a certain
#'   depth(`level`) for a set of dependency types (`relation`).
#' @param packages (`chr`) Package names
#' @param level (`count(1)`) Depth of recursive dependency
#' @param relation (`chr`) Types of relations. Must be a subset of
#'   c("Depends", "Imports", "LinkingTo", "Suggests", "Enhances")
#' @param strict (`flag[T]`) Whether to consider all packages
#'   (alternately only 'relation' specific packages) when computing dependencies
#'   for the next level.
#' @param ignore (`chr`) package names to ignore.
#' @param reverse (`flag[F]`) Whether to get reverse dependencies.
#' @return (`dataframe`) with three columns: `pkg_1`, `relation` and `pkg_2`
#' @examples
#' \dontrun{
#' init()
#' get_dependencies("mlr3") |>
#' get_dependencies("mlr3", level = 2)
#' get_dependencies("mlr3", level = 2, reverse = TRUE)
#' get_dependencies("mlr3", level = 2, relation = "Imports")
#' # setting strict to TRUE to only consider 'Imports' of the previous level
#' get_dependencies("mlr3",
#'                  level    = 2,
#'                  relation = "Imports",
#'                  strict   = TRUE
#'                  )
#' }
#' @seealso [get_neighborhood]
#' @export

get_dependencies = function(packages,
                            level = 1L,
                            relation = c("Depends",
                                         "Imports",
                                         "LinkingTo",
                                         "Suggests",
                                         "Enhances"
                                          ),
                            strict = FALSE,
                            ignore = c("datasets",
                                       "utils",
                                       "grDevices",
                                       "graphics",
                                       "stats",
                                       "methods"
                                       ),
                            reverse = FALSE
                            ){
  # assertions
  if (!exists("deptable")){
    abort("Unable to find `deptable`. Did you run `init`?")
  }

  assert_character(packages, min.len = 1, unique = TRUE)
  assert_count(level)
  assert_character(relation, min.len = 1)
  relation_chr = c("Depends", "Imports", "LinkingTo", "Suggests", "Enhances")
  assert_subset(relation, relation_chr)
  assert_flag(strict)
  assert_character(ignore)
  assert_flag(reverse)

  relation2 = relation_chr
  if (strict) relation2 = relation

  # loop over 'level'
  ell = length(relation2)
  for (it in seq_along(1:level)){

    if (reverse){
      res = filter(deptable, pkg_2 %in% packages)
    } else {
      res = filter(deptable, pkg_1 %in% packages)
    }

    # filter depending on 'strict'
    if (strict) res = filter(res, relation %in% relation2)

    # handle ignore
    if (length(ignore) > 0){
      res = filter(res, !(pkg_1 %in% ignore) & !(pkg_2 %in% ignore))
    }

    packages = base::union(res[["pkg_1"]], res[["pkg_2"]])
  }

  res =
    res |>
    # always filter for relation (strict only affects inside the loop)
    filter(relation %in% !!relation) |>
    arrange(relation, pkg_1, pkg_2)

  return(res)
}

#' @name get_neighborhood
#' @title Obtain dependencies and reverse dependencies of a set of packages
#' @description Obtain dependencies and reverse dependencies of packages till a
#'   given depth (`level`) for a set of dependency types (`relation`).
#' @param packages (`chr`) Package names
#' @param level (`count[1]`) Depth of recursive dependency
#' @param relation (`chr`) Types of relations. Must be a subset of
#'   c("Depends", "Imports", "LinkingTo", "Suggests", "Enhances")
#' @param strict (`flag[T]`) Whether to consider all packages
#'   (alternately only 'relation' specific packages) when computing dependencies
#'   for the next level
#' @param ignore (`chr`) Package names to ignore.
#' @return (`dataframe`) three columns: `pkg_1`, `relation` and `pkg_2`
#' @examples
#' \dontrun{
#' init()
#' # explore first level dependencies
#' get_neighborhood("mlr3")
#'
#' # explore second level dependencies
#' get_neighborhood("caret", level = 2)
#'
#' # explore first level dependencies of multiple packages
#' # and consider second level dependencies
#' get_neighborhood(c("caret", "mlr"))
#'
#' # get 'imports' specific neighborhood of 'mlr'
#' get_neighborhood("mlr", relation = "Imports")
#' }
#' @seealso [get_dependencies]
#' @export

get_neighborhood = function(packages,
                            level = 1L,
                            relation = c("Depends",
                                         "Imports",
                                         "LinkingTo",
                                         "Suggests",
                                         "Enhances"
                                          ),
                            strict = FALSE,
                            ignore = c("datasets",
                                       "utils",
                                       "grDevices",
                                       "graphics",
                                       "stats",
                                       "methods"
                                       )
                            ){
  # get direct dependencies
  dep_df = get_dependencies(packages,
                            level = level,
                            relation = relation,
                            strict = strict,
                            ignore = ignore,
                            reverse = FALSE
                            )
  # get reverse dependencies
  rev_df = get_dependencies(packages,
                            level = level,
                            relation = relation,
                            strict = strict,
                            ignore = ignore,
                            reverse = TRUE
                            )

  # get interconnects
  pkgs_chr = Reduce(base::union,
                    list(dep_df[["pkg_1"]],
                         dep_df[["pkg_2"]],
                         rev_df[["pkg_1"]],
                         rev_df[["pkg_2"]]
                         )
                    )

  # filter with AND
  interconnect_df =
    deptable |>
    filter(pkg_1 %in% pkgs_chr,
           pkg_2 %in% pkgs_chr,
           relation %in% !!relation
           )

  res =
    bind_rows(dep_df, rev_df, interconnect_df) |>
    distinct() |>
    arrange(relation, pkg_1, pkg_2)

  return(res)
}

#' @name as_graph
#' @title Get igraph object from a dataframe representing package dependencies
#' @description Packages dependencies (typically obtained from
#' [get_dependencies()] or [get_neighborhood()]) is converted into a
#' [igraph](https://cran.r-project.org/package=igraph) object.
#' @param dependency_df (`dataframe`) Representing package dependencies
#' @returns igraph object
#' @examples
#' \dontrun{
#' init()
#' as_graph(get_neighborhood("mlr3"))
#' }
#' @export

as_graph = function(dependency_df){

  assert_data_frame(dependency_df)
  assert_subset(c("pkg_1", "relation", "pkg_2"), colnames(dependency_df))

  # create
  ng = igraph::graph_from_data_frame(dependency_df[ , c("pkg_1","pkg_2")],
                                     directed = TRUE
                                     )

  # add attr
  ng = igraph::set_edge_attr(graph   = ng,
                             name  = "relation",
                             value = as.character(dependency_df[["relation"]])
                             )

  package_title_df =
    tibble("Package" = union(dependency_df$pkg_1, dependency_df$pkg_2)) |>
    left_join(select(packmeta, Package, Title), by = "Package") |>
    select(Package, Title)

  ng = igraph::set_vertex_attr(graph   = ng,
                               name  = "title",
                               value = package_title_df$Title,
                               index = package_title_df$Package
                               )

  class(ng) = c("neighborhood_graph", class(ng))
  return(ng)
}

#' @name plot.neighborhood_graph
#' @title Static plot of package dependencies
#' @description Plot of packages dependencies or neighborhood with edges colored
#' by `relation` and node sized by 'centrality' from a `neighborhood_graph`
#' obtained from [as_graph].
#' @param x neighborhood_graph
#' @param layout layout is passed to [ggraph::ggraph]
#' @param ... Not used
#' @returns ggraph plot
#' @examples
#' \dontrun{
#' init()
#' get_neighborhood("mboost") |>
#'   as_graph() |>
#'   plot()
#' }
#' @export
plot.neighborhood_graph = function(x, layout = "sugiyama", ...){

  colors = c(
    "Imports" = "darkblue",
    "Depends" = "darkgreen",
    "LinkingTo" = "grey",
    "Suggests" = "orange",
    "Enhances" = "pink"
  )


  po =
    tidygraph::as_tbl_graph(x) |>
    mutate(cen = tidygraph::centrality_eigen()) |>
    ggraph(layout = layout) +
    geom_edge_link(aes(edge_color = relation),
                       arrow = arrow(type = "closed", )
                       ) +
    geom_node_point(aes(size = cen)) +
    geom_node_label(aes(label = name), repel = TRUE) +
    scale_edge_color_manual(values = colors)

  return(po)
}
