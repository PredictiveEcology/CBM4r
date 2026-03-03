
# Set virtual environment
if (as.logical(Sys.getenv("CI", "false"))){

  withr::local_envvar(list(RETICULATE_PYTHON_ENV = "r-CBM4"), .local_envir = testthat::teardown_env())

}else{

  # Use local CBM4 install
  cbm4_virtualenv <- file.path(Sys.getenv("USERPROFILE"), "cbm4", ".venv")
  if (!reticulate::virtualenv_exists(cbm4_virtualenv)) stop("CBM4 Python virtual environment not found: ", cbm4_virtualenv)

  # Import pyarrow before loading package to prevent DLL loading issues
  reticulate::use_virtualenv(cbm4_virtualenv)
  reticulate::import("pyarrow")
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
dir.create(testDirs$temp$inputs,  recursive = TRUE)
dir.create(testDirs$temp$outputs, recursive = TRUE)

if (testthat::is_testing()) withr::defer({
  unlink(testDirs$temp$root, recursive = TRUE)
  if (file.exists(testDirs$temp$root)) warning(
    "Temporary test directory could not be removed: ",
    testDirs$temp$root, call. = FALSE)
}, envir = testthat::teardown_env(), priority = "last")

# Download CBM-CFS3 defaults database
cbm_defaults_db <- file.path(testDirs$temp$inputs, "cbm_defaults.db")
if (!file.exists(cbm_defaults_db)) download.file(
  "https://raw.githubusercontent.com/cat-cfs/libcbm_py/main/libcbm/resources/cbm_defaults_db/cbm_defaults_v1.2.9300.391.db",
  cbm_defaults_db, mode = "wb", quiet = TRUE)

