
# Use local CBM4 install
cbm4_virtualenv <- file.path(Sys.getenv("USERPROFILE"), "cbm4", ".venv")
if (!reticulate::virtualenv_exists(cbm4_virtualenv)) stop("CBM4 Python virtual environment not found: ", cbm4_virtualenv)

# Import pyarrow before loading package to prevent DLL loading issues
reticulate::use_virtualenv(cbm4_virtualenv)
reticulate::import("pyarrow")

# Run all tests
testthat::test_local()

