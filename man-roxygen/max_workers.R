#' @param max_workers integer.
#' Number of parallel processes to use.
#' NULL or NA will bypass this and and cap processing at the system's available resources.
#' A positive integer will spawn multiple processes assigned to each `chunk_index`.
