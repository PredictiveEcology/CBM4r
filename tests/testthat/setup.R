
# Set virtual environment
if (Sys.getenv("VIRTUAL_ENV") == ""){

  cbm4_virtualenv <- Sys.getenv("RETICULATE_PYTHON_ENV", unset = "r-CBM4")
  if (!reticulate::virtualenv_exists(cbm4_virtualenv)) stop("CBM4 Python virtual environment not found: ", cbm4_virtualenv)

  if (testthat::is_testing()){
    withr::local_envvar(list(RETICULATE_PYTHON_ENV = cbm4_virtualenv), .local_envir = testthat::teardown_env())

  }else{

    Sys.setenv(RETICULATE_PYTHON_ENV = cbm4_virtualenv)

    # Import pyarrow before loading package to prevent DLL loading issues
    reticulate::use_virtualenv(cbm4_virtualenv)
    reticulate::import("pyarrow")
  }
}

if (!testthat::is_testing()){
  library(testthat)
  devtools::load_all()
}

# Set up test directories
testDirs <- list(
  testdata = testthat::test_path("testdata"),
  temp = list(
    root    = file.path(tempdir(), "testthat-CBM4r"),
    inputs  = file.path(tempdir(), "testthat-CBM4r", "inputs"),
    outputs = file.path(tempdir(), "testthat-CBM4r", "outputs")
  )
)
dir.create(testDirs$temp$root)
dir.create(testDirs$temp$inputs,  showWarnings = FALSE)
dir.create(testDirs$temp$outputs, showWarnings = FALSE)

if (testthat::is_testing()) withr::defer({
  unlink(testDirs$temp$root, recursive = TRUE)
  if (file.exists(testDirs$temp$root)) warning(
    "Temporary test directory could not be removed: ",
    testDirs$temp$root, call. = FALSE)
}, envir = testthat::teardown_env(), priority = "last")

# Set temporary location for virtual environment
if (as.logical(Sys.getenv("CI", "false"))){
  withr::local_envvar(list(RETICULATE_VIRTUALENV_ROOT = file.path(testDirs$temp$root, ".virtualenvs")),
                      .local_envir = testthat::teardown_env())
}


