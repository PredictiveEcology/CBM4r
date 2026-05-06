
#' CBM4 create virtualenv
#'
#' Create a Python virtual environment to run CBM4.
#'
#' @param envname character. Virtual environment name.
#' @param version character. CBM4 version.
#' @param upgrade logical. Upgrade Python packages.
#' @param quiet logical. Silence pip output.
#' @param ... arguments to \code{\link[reticulate]{virtualenv_create}}
#'
#' @export
cbm4_virtualenv_create <- function(envname = "r-CBM4", version = NULL, upgrade = FALSE, quiet = FALSE, ...){

  # Using gert to clone repos due to issues connecting with reticulate
  # This may be due to recent VPN connection issues to Github (2025-03)
  # This may make it faster to update packages
  if (!requireNamespace("gert", quietly = TRUE)) stop(
    "The package \"gert\" is required to create the CBM4 Python virtual environment")

  # Get version requirements
  vers <- cbm4_versions(version)

  # Initiate virtual environment with GDAL
  if (!reticulate::virtualenv_exists(envname)){

    reticulate::virtualenv_create(envname, version = vers$python, ...)

    if (identical(.Platform$OS.type, "windows")){

      reticulate::virtualenv_install(
        envname,
        packages = vers$gdal_win,
        pip_options = c("--upgrade"[upgrade], "-q"[quiet])
      )

    }else{

      gdalVers <- system("gdal-config --version", intern = TRUE)

      if (!is.null(vers[["gdal"]]) && (
        utils::compareVersion(gdalVers, vers[["gdal"]][["min"]]) == -1 |
        utils::compareVersion(gdalVers, vers[["gdal"]][["max"]]) ==  1
      )) stop("gdal >=", vers[["gdal"]][["min"]], ",<=", vers[["gdal"]][["max"]], " not found")

      reticulate::virtualenv_install(
        envname,
        packages = paste0("gdal[numpy]==", gdalVers),
        pip_options = c("--no-cache-dir", "--no-build-isolation", "--upgrade"[upgrade], "-q"[quiet])
      )
    }
  }

  # Install CBM4 packages
  packages <- c(
    "arrow_space"  = "https://github.com/cat-cfs/arrow_space.git",
    "cbm4"         = "https://github.com/cat-cfs/cbm4.git",
    "cbmspec_cbm3" = "https://github.com/cat-cfs/cbmspec.cbm3.python.git"
  )

  envPackages <- trimws(reticulate::py_list_packages(envname)$package)

  for (package in names(packages)){

    pkg_path <- file.path(tools::R_user_dir("CBM4r"), package)

    clone <- !file.exists(pkg_path)
    if (clone){
      gert::git_clone(packages[[package]], path = pkg_path)
    }

    refID <- gert::git_commit_info(repo = pkg_path)$id

    if (package %in% names(vers)){
      gert::git_pull(repo = pkg_path, verbose = FALSE)
      gert::git_reset_hard(vers[[package]], repo = pkg_path)
    }else{
      gert::git_reset_hard(repo = pkg_path)
      gert::git_pull(repo = pkg_path, verbose = FALSE)
    }

    install <- !package %in% envPackages ||
      clone || upgrade || !identical(refID, gert::git_commit_info(repo = pkg_path)$id)

    if (install){
      reticulate::virtualenv_install(
        envname,
        packages = pkg_path,
        pip_options = c("--upgrade"[upgrade], "-q"[quiet])
      )
    }
  }
}


# CBM4 version requirements
cbm4_versions <- function(version = NULL){

  vers <- list(
    "2.17.10" = list(
      cbm4     = "2.17.10",
      python   = ">=3.12",
      gdal_win = "https://github.com/cgohlke/geospatial-wheels/releases/download/v2025.10.25/gdal-3.11.4-cp312-cp312-win_amd64.whl"
    ),
    "2.17.9" = list(
      cbm4     = "2.17.9",
      python   = ">=3.12,<3.13",
      gdal_win = "https://github.com/cgohlke/geospatial-wheels/releases/download/v2025.7.4/gdal-3.11.1-cp312-cp312-win_amd64.whl",
      gdal     = c(min = "0", max = "3.11.3")
    )
  )

  if (!is.null(version) && !version %in% names(vers)) stop(
    "CBM4 v", version, " requirements not set. Use version = NULL or choose from versions: ",
    paste(names(vers), collapse = "; "))

  vers[[if (!is.null(version)) version else 1]]
}


