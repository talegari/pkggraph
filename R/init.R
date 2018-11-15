#' @title init
#' @author Srikanth KS
#' @description Initiate the package by loading the data into parent frame. This
#'   should be done as soon as the package is loaded or attached. This
#'   creates(rewrites) new variables 'deptable' and 'packmeta' to the
#'   environment where it is run from.
#' @param local (flag, default: FALSE) If \itemize{ \item FALSE: Tries to to
#'   download package data from CRAN over internet and compute dependencies
#'   \item TRUE: Loads data that comes with the package corresponding to 2nd
#'   September 2017 02:04 IST}
#' @param repository (character vector, Default: "CRAN") One among c("CRAN",
#'   "BioCsoft", "BioCann", "BioCexp", "BioCextra", "omegahat"). To use a
#'   repository not in this list, set 'repository' to NULL and pass named
#'   argument called 'repos' with a valid repository address. This will be
#'   passed as is to `utils::available.packages()`.
#' @param ... Additional parameters to be passed to `available.packages()`
#' @return An invisible TRUE
#' @export

init <- function(local        = FALSE
                 , repository = "CRAN"
                 , ...){
  # assertions               ----
  stopifnot(is.logical(local) && length(local) == 1)
  # if internet connection is not available
  if(!local && !curl::has_internet()){
    message("No internet connection. Switching to 'local' mode.")
  }

  if(!local){
    stopifnot(is.null(repository) ||
                repository %in% c("CRAN"
                                  , "BioCsoft"
                                  , "BioCann"
                                  , "BioCexp"
                                  , "BioCextra"
                                  , "omegahat")
    )
    extra_args <- list(...)
    # cannot specify both repository and repos
    if(!is.null(repository) && ("repos" %in% names(extra_args))){
      stop("Set 'repository' to NULL if you intend to specify 'repos' in '...'")
    }
  }

  # set up repository ----
  if(!local){
    if(!is.null(repository))
    repos_lookup <- list()
    repos_lookup[["CRAN"]]     <- "https://cran.rstudio.com/"
    repos_lookup[["omegahat"]] <- "http://www.omegahat.net/R"
    if(repository %in% c("BioCsoft"
                         , "BioCann"
                         , "BioCexp"
                         , "BioCextra")){
      if(!requireNamespace("BiocManager")){
        stop("Missing 'BiocManager' package")
      } else {
        repos_lookup <- c(repos_lookup, BiocManager::repositories())
      }
    }
  }

  # obtain and set data      ----
  if(!local){
    message("Fetching package metadata from repositories ...")
    # get package metadata
    if(!is.null(repository)){
      packmeta  <- lapply(repos_lookup[repository]
                          , function(x) utils::available.packages(repos = x)
                          )
      packmeta  <- do.call(rbind, packmeta)
    } else {
      packmeta <- utils::available.packages(repos = extra_args[["repos"]])
    }
  # assign to parent frame
  assign("packmeta", packmeta, envir = parent.frame())
  }

  # fall-back
  if(local){
    message("Using 'local' data ...")
    message("To fetch data from CRAN over internet, run:", appendLF = FALSE)
    message("`pkggraph::init(local = FALSE)`")
    utils::data(sysdata
                , package = "pkggraph"
                , envir = parent.frame()
                )
  }

  # compute dependency table ----
  if(!local){
    message("Computing package dependencies ...")
    types <- c("Depends"
               , "Imports"
               , "LinkingTo"
               , "Suggests"
               , "Enhances")

    # dependency DT for each relation
    typed_df = function(string){

      df = strsplit(packmeta[,string], split = ",")
      df = lapply(df, function(y) gsub("\\([^()]*\\)", "", y))
      df = lapply(df, trimws)
      df = plyr::ldply(df, data.table)

      setDT(df)
      setnames(df, colnames(df), c("pkg_1", "pkg_2"))
      rfree = df$pkg_2 != "R" & !is.na(df$pkg_2)
      df = df[rfree,][, ("relation") := string]
      setcolorder(df, c("pkg_1", "relation", "pkg_2"))

      return(df)
    }


    # create a long tibble
    deptable <- rbindlist(lapply(types, typed_df))

    # set factor levels for relation
    relation = NULL
    deptable[, ("relation") := factor(relation, levels = types)]

    # convert to tibble
    deptable = tibble::as_tibble(deptable)

    # assign to parent frame
    assign("deptable"
           , deptable
           , envir = parent.frame())
  }

  message("Done!")
  return(invisible(TRUE))
}
