
#' cbm4_virtualenv_create
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

  # Initiate virtual environment
  if (!reticulate::virtualenv_exists(envname)){
    reticulate::virtualenv_create(envname, version = vers$python, ...)
  }

  # Install Python packages
  if (length(vers$packages) > 0){
    reticulate::virtualenv_install(
      envname,
      packages = vers$packages,
      pip_options = c("--upgrade"[upgrade], "-q"[quiet])
    )
  }

  # Install GDAL
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

  # Install CBM4 packages
  packages <- c(
    "libcbm"       = "https://github.com/cat-cfs/libcbm_py",
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
      if (!identical(refID, vers[[package]])){
        gert::git_pull(repo = pkg_path, verbose = FALSE)
        gert::git_reset_hard(vers[[package]], repo = pkg_path)
      }
    }else{
      gert::git_reset_hard(repo = pkg_path)
      gert::git_pull(repo = pkg_path, verbose = FALSE)
    }

    refID_new <- gert::git_commit_info(repo = pkg_path)$id
    install <- !any(c(package, sub("_", "-", package)) %in% envPackages) ||
      clone || upgrade || !identical(refID, refID_new)

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

    # Issue with formatting of view table from SQL results processor
    # "2.24.3" = list(
    #   python       = ">=3.12",
    #   gdal_win     = "https://github.com/cgohlke/geospatial-wheels/releases/download/v2025.10.25/gdal-3.11.4-cp312-cp312-win_amd64.whl",
    #   cbm4         = "063034f1b9d90e9609e5f80d37d44a3f1ff3194c", # 2026-06-25
    #   arrow_space  = "8744e4c80daa327fdf7aeda604d8e755f96aa509", # 2026-06-24
    #   cbmspec_cbm3 = "c13bc70a2f14e9aab614996324855701c6947913"  # 2026-06-24
    # ),

    "2.17.10" = list(
      python       = ">=3.12",
      gdal_win     = "https://github.com/cgohlke/geospatial-wheels/releases/download/v2025.10.25/gdal-3.11.4-cp312-cp312-win_amd64.whl",
      packages     = "pandas==2.3.3",
      cbm4         = "b8f0991bcd723661fd74aec52dd0e314c7d26dbb", # 2026-03-27
      arrow_space  = "7715ba811ef34a62ffd859d73e02c8659c4bf311", # 2026-03-27
      cbmspec_cbm3 = "da8c70a47c9ea7163a2e99dd6703bafef9b12a4e", # 2026-03-31
      libcbm       = "fee02dfa839fe4cee2252208d9e1e452a5e7adf3"  # 2026-01-21
    ),
    "2.17.9" = list(
      python   = ">=3.12,<3.13",
      gdal_win = "https://github.com/cgohlke/geospatial-wheels/releases/download/v2025.7.4/gdal-3.11.1-cp312-cp312-win_amd64.whl",
      gdal     = c(min = "0", max = "3.11.3"),
      cbm4     = "2.17.9"
    )
  )

  if (!is.null(version) && !version %in% names(vers)) stop(
    "CBM4 v", version, " requirements not set. Use version = NULL or choose from versions: ",
    paste(names(vers), collapse = "; "))

  vers[[if (!is.null(version)) version else 1]]
}


