
#' CBM4 data copy
#'
#' Copy a CBM4 spatial parquet datasets directory.
#'
#' @template template_data
#' @template cbm4_data
#' @param dataset_names character. Names of CBM4 spatial parquet datasets to copy.
#' @template overwrite
#'
#' @return `NULL`. Data directory will be copied.
#' @export
cbm4_data_copy <- function(
    template_data,
    cbm4_data,
    dataset_names = NULL,
    overwrite     = FALSE
){

  if (length(template_data) == 0)  stop("template_data invalid")
  if (!file.exists(template_data)) stop("template_data not found: ", template_data)

  if (length(cbm4_data) == 0)  stop("cbm4_data invalid")
  if (file.exists(cbm4_data)){
    if (!overwrite) stop("cbm4_data exists; set overwrite = TRUE: ", cbm4_data)
    unlink(cbm4_data, recursive = TRUE)
    if (file.exists(cbm4_data)) stop("cbm4_data could not be removed: ", cbm4_data)
  }

  dataset_options <- list.files(template_data, full.names = FALSE, recursive = FALSE)
  if (is.null(dataset_names)){
    dataset_names <- dataset_options
  }else if (!all(dataset_names %in% dataset_options)) stop(
    "dataset(s) not found: ", paste(shQuote(setdiff(dataset_names, dataset_options)), collapse = ", "))

  dir.create(cbm4_data)
  for (dataset_name in dataset_names) cbm4_data_copy_dataset(
    cbm4_data     = cbm4_data,
    dataset_name  = dataset_name,
    template_data = template_data
  )

  return(invisible())
}


#' CBM4 data copy dataset
#'
#' Copy a CBM4 spatial parquet dataset.
#'
#' @template cbm4_data
#' @template dataset_name
#' @template dataset_path
#' @template template_data
#' @template template_name
#' @template template_path
#' @param ... arguments to \code{\link{arrow_space_dataset_copy}}
#'
#' @return `NULL`. Dataset will be copied.
#' @export
cbm4_data_copy_dataset <- function(
    cbm4_data     = NULL,
    dataset_name  = NULL,
    dataset_path  = file.path(cbm4_data, dataset_name),
    template_data = cbm4_data,
    template_name = dataset_name,
    template_path = file.path(template_data, template_name),
    ...
){

  arrow_space_dataset_copy(
    dataset_dir   = cbm4_data,
    dataset_name  = dataset_name,
    dataset_path  = dataset_path,
    template_name = template_name,
    template_path = template_path,
    ...
  )
}
