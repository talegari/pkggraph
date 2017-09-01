# get_dependencies
# not exported worker function
get_dependencies <- function(packages
                             , type
                             , level   = 1L
                             , reverse = FALSE
                             , assert  = TRUE){
  # assertions ----
  stopifnot(is.logical(assert) && length(assert) == 1)
  if(assert){
    stopifnot(is.character(packages) && length(packages) > 0)
    stopifnot(type %in% c("Depends"
                          , "Imports"
                          , "LinkingTo"
                          , "Suggests"
                          , "Enhances"))
    stopifnot(length(type) == 1)
    stopifnot(length(level) == 1 &&
              level == as.integer(level) &&
              level >= 1L
              )
    stopifnot(is.logical(reverse) && length(reverse) == 1)
    if(!exists("deptable")){
      stop("Unable to find `deptable`. Please run `pkggraph::init(local = TRUE)`.")
    }
  }

  # subset     ----
  if(reverse){

    res <- deptable[deptable$pkg_2 %in% packages & deptable$relation == type, ]

    if(level > 1){
      for(it in seq_along(2:level)){
        packages <- union(res[["pkg_1"]], res[["pkg_2"]])
        res      <- deptable[deptable$pkg_2 %in% packages & deptable$relation == type, ]
      }
    }
  } else {

    res <- deptable[deptable$pkg_1 %in% packages & deptable$relation == type, ]

    if(level > 1){
      for(it in seq_along(2:level)){
        packages <- union(res[["pkg_1"]], res[["pkg_2"]])
        res      <- deptable[deptable$pkg_1 %in% packages & deptable$relation == type, ]
      }
    }
  }

  return(res)
}
