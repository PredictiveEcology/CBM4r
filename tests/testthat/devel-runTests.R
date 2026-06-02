
# Set virtual environment
cbm4_virtualenv <- Sys.getenv("RETICULATE_PYTHON_ENV", unset = "r-CBM4")
if (!reticulate::virtualenv_exists(cbm4_virtualenv)) stop("CBM4 Python virtual environment not found: ", cbm4_virtualenv)

# Import pyarrow before loading package to prevent DLL loading issues
reticulate::use_virtualenv(cbm4_virtualenv)
reticulate::import("pyarrow")

# Run all tests
testthat::test_local(stop_on_failure = TRUE)

# Run full simulation tests
testthat::test_local(filter = "simulate-1", stop_on_failure = TRUE)


# Trying different reporters
testthat::test_local(reporter = testthat::CompactProgressReporter) # Minimal: just the pass/fai/etc totals
testthat::test_local(reporter = testthat::ProgressReporter) # The default
testthat::test_local(reporter = testthat::SlowReporter) # Checks for slow stuff. Says which tests are done running
testthat::test_local(reporter = testthat::ListReporter)
testthat::test_local(reporter = testthat::StopReporter)
