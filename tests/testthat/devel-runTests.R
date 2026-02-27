
# Set virtual environment
## TEMPORARY: use already existing local CBM4 install
cbm4_virtualenv <- file.path(Sys.getenv("USERPROFILE"), "cbm4", ".venv")
if (!file.exists(cbm4_virtualenv)) stop("CBM4 Python virtual environment not found: ", cbm4_virtualenv)
reticulate::use_virtualenv(cbm4_virtualenv)
reticulate::import("pyarrow") # Import pyarrow before loading package to prevent DLL loading issues

# Run all tests
testthat::test_local()

