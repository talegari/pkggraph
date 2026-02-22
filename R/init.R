#' @name init
#' @title Create package metadata and dependency dataframes
#' @description Initiate the package by loading the data into parent frame. This
#'   should be done as soon as the package is loaded or attached. This
#'   creates(rewrites) new variables `deptable` and `packmeta` to the
#'   environment where it is run from.
#' @param repository (`chr[1]`, Default: "CRAN") One among c("CRAN",
#'   "BioCsoft", "BioCann", "BioCexp", "BioCextra", "omegahat"). To use a
#'   repository not in this list, set 'repository' to NULL and pass named
#'   argument called 'repos' with a valid repository address. This will be
#'   passed as is to [utils::available.packages()].
#' @param ... Parameters to be passed to [utils::available.packages()]
#' @return TRUE (invisibly)
#' @details Format of `packmeta`: A dataframe with one row per package with some
#'   metadata.
#'
#'   Format of `deptable`: Dataframe with three columns pkg_1 (chr), relation
#'   (factor, levels = `Depends`, `Imports`, `Suggests`, `LinkingTo`,
#'   `Enhances`), pkg_2 (chr)
#' @export

init = function(repository = "CRAN", ...){

  assert_string(repository, null.ok = TRUE)
  known_repositories_chr = c("CRAN", "BioCsoft", "BioCann",
                             "BioCexp", "BioCextra", "omegahat"
                             )
  if (!is.null(repository)) assert_subset(repository, known_repositories_chr)
  extra_args = list(...)
  get_ap = utils::available.packages
  ts_value = Sys.time()

  # set up repository ----
  if(!is.null(repository)){
    repos_lookup               = list()
    repos_lookup[["CRAN"]]     = "https://cran.rstudio.com/"
    repos_lookup[["omegahat"]] = "http://www.omegahat.net/R"

    bioc_packages_chr = c("BioCsoft", "BioCann", "BioCexp", "BioCextra")
    if (repository %in% bioc_packages_chr){
      check_installed("BiocManager")
      if (!is_installed("BiocManager")){
        abort("Missing 'BiocManager' package")
      } else {
        repos_lookup = c(repos_lookup, BiocManager::repositories())
      }
    }
  } else {
    if (is.null(extra_args[["repos"]])){
      abort("When repository is NULL, repos needs to be specified")
    }
    repos_lookup = extra_args[["repos"]]
  }

  # obtain and set data ----
  cli_alert_info("Fetching package metadata from repositories ...")
  # get package metadata
  if (!is.null(repository)){
    packmeta = lapply(repos_lookup[repository],
                      \(.x) as_tibble(get_ap(repos = .x))
                      )
    packmeta = bind_rows(packmeta)
  } else {
    packmeta = get_ap(repos = extra_args[["repos"]])
  }

  if (repository == "CRAN"){
    cran_df = tools::CRAN_package_db()

    packmeta =
      packmeta |>
      left_join(select(cran_df, Package, Title, Description), by = "Package")
  }

  # assign to parent frame
  attr(packmeta, "timestamp") = ts_value
  assign("packmeta", packmeta, envir = parent.frame())

  # compute dependency table ----
  cli_alert_info("Computing package dependencies ...")

  deptable =
    packmeta |>
    select(pkg_1 = Package, Depends, Imports, Suggests, LinkingTo, Enhances) |>
    pivot_longer(cols = -pkg_1,
                 values_to = "pkg_2",
                 names_to = "relation",
                 values_drop_na = TRUE
                 ) |>
    mutate(pkg_2 = lapply(pkg_2, \(.x) str_split_1(.x, ","))) |>
    unnest(pkg_2) |>
    mutate(pkg_2 = str_trim(str_replace_all(pkg_2, "\\([^()]*\\)", "")),
           relation = factor(relation)
           ) |>
    filter(pkg_2 != "R", pkg_1 != "", pkg_2 != "")

  # assign to parent frame
  attr(deptable, "timestamp") = ts_value
  assign("deptable", deptable, envir = parent.frame())

  cli_alert_success("Done!")
  return(invisible(TRUE))
}
