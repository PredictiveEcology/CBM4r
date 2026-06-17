
# Set virtual environment
cbm4_virtualenv <- Sys.getenv("RETICULATE_PYTHON_ENV", unset = "r-CBM4")
if (!reticulate::virtualenv_exists(cbm4_virtualenv)) stop("CBM4 Python virtual environment not found: ", cbm4_virtualenv)

# Import pyarrow before loading package to prevent DLL loading issues
reticulate::use_virtualenv(cbm4_virtualenv)
reticulate::import("pyarrow")

# Run all tests
testthat::test_local()

# Run simple simulation tests
testthat::test_local(filter = "simulate-1")

# Run individual special tests
testthat::test_local(filter = "simulate-(1-1|2-disturbance)")
testthat::test_local(filter = "simulate-(1-1|2-increments)")
testthat::test_local(filter = "simulate-(1-1|3-cbm4\\_step\\_with\\_cohorts)")

