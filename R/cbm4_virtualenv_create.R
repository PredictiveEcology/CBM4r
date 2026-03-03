
#' CBM4 create virtualenv
#'
#' Create a Python virtual environment to run CBM4.
#'
#' @param virtualenv character. TODO
#' @param version character. TODO
#' @param upgrade logical. TODO
#' @param quiet logical. TODO
#' @param ... TODO
#'
#' @export
cbm4_virtualenv_create <- function(virtualenv, version = NULL, upgrade = FALSE, quiet = FALSE, ...){

  # Using gert to clone repos due to issues connecting with reticulate
  # This may be due to recent VPN connection issues to Github (2025-03)
  # This may make it faster to update packages
  if (!requireNamespace("gert", quietly = TRUE)) stop(
    "The package \"gert\" is required to create the CBM4 Python virtual environment")

  if (!reticulate::virtualenv_exists(virtualenv)){
    reticulate::virtualenv_create(
      virtualenv,
      version  = ">=3.12,<3.13",
      packages = "https://github.com/cgohlke/geospatial-wheels/releases/download/v2025.7.4/gdal-3.11.1-cp312-cp312-win_amd64.whl",
      pip_options = c("--upgrade"[upgrade], "-q"[quiet]),
      ...
    )
  }

  packages <- c(
    "arrow_space"  = "https://github.com/cat-cfs/arrow_space.git",
    "cbm4"         = "https://github.com/cat-cfs/cbm4.git",
    "cbmspec_cbm3" = "https://github.com/cat-cfs/cbmspec.cbm3.python.git"
  )

  envPackages <- trimws(reticulate::py_list_packages(virtualenv)$package)

  for (package in names(packages)){

    pkg_path <- file.path(tools::R_user_dir("CBM4r"), package)

    clone <- !file.exists(pkg_path)
    if (clone){
      gert::git_clone(packages[[package]], path = pkg_path)
    }

    refID <- gert::git_commit_info(repo = pkg_path)$id

    if (package == "cbm4" & !is.null(version)){
      gert::git_reset_hard(version, repo = pkg_path)
    }else{
      gert::git_reset_hard(repo = pkg_path)
      gert::git_pull(repo = pkg_path, verbose = FALSE)
    }

    install <- !package %in% envPackages ||
      clone || upgrade || !identical(refID, gert::git_commit_info(repo = pkg_path)$id)

    if (install){
      reticulate::virtualenv_install(
        virtualenv,
        packages = pkg_path,
        pip_options = c("--upgrade"[upgrade], "-q"[quiet])
      )
    }
  }
}


