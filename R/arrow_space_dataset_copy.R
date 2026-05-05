
#' arrow_space_dataset_copy
#'
#' @param dataset_dir character. Path to `arrow_space` dataset directory.
#' @param dataset_name character. name of `arrow_space` dataset directory.
#' @param dataset_path character. Path to `arrow_space` dataset.
#' Defaults to `file.path(dataset_dir, dataset_name)`
#' @param template_name character. name of `arrow_space` dataset directory to copy table from.
#' @param template_path character. Path to `arrow_space` dataset to copy table from.
#' Defaults to `file.path(dataset_dir, template_name)`
#' @param table_names character. TODO
#' @param overwrite logical. TODO
#' @param skip_missing logical. TODO
arrow_space_dataset_copy <- function(
    dataset_dir = NULL,
    dataset_name,
    template_name = dataset_name,
    table_names   = NULL,
    overwrite     = FALSE,
    skip_missing  = FALSE,
    dataset_path  = file.path(dataset_dir, dataset_name),
    template_path = file.path(dataset_dir, template_name)
){

  if (identical(dataset_path, template_path)) stop(
    "dataset_path and template_path are identical: ", dataset_path)

  if (file.exists(dataset_path)){
    if (overwrite){
      unlink(dataset_path, recursive = TRUE)
    }else stop("Existing dataset found; set overwrite = TRUE: ", dataset_path)
  }

  table_names <- sub(paste0("^", template_name, "$"), "", gsub(
    paste0("^", template_name, "\\-"), "",
    list.files(template_path, full.names = FALSE)))

  for (table_name in table_names){
    arrow_space_dataset_copy_table(
      table_name    = table_name,
      dataset_dir   = dataset_dir,
      dataset_name  = dataset_name,
      dataset_path  = dataset_path,
      template_name = template_name,
      template_path = template_path,
      skip_missing  = skip_missing,
      overwrite     = overwrite
    )
  }

  return(invisible())
}

