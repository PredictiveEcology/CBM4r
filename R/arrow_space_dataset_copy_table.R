
arrow_space_dataset_copy_table <- function(
    dataset_dir = NULL,
    dataset_name,
    template_name = dataset_name,
    table_name    = NULL,
    overwrite     = FALSE,
    skip_missing  = FALSE,
    dataset_path  = file.path(dataset_dir, dataset_name),
    template_path = file.path(dataset_dir, template_name)
){

  table_paths <- c(
    copy = file.path(template_path, template_name),
    new  = file.path(dataset_path,  dataset_name)
  )
  if (!is.null(table_name) && table_name != ""){
    for (i in 1:2) table_paths[[i]] <- paste0(table_paths[[i]], "-", table_name)
  }

  if (identical(table_paths[["copy"]], table_paths[["new"]])) stop(
    "copy and destination table paths are identical: ", table_paths[["new"]])

  if (!file.exists(table_paths[["copy"]]) & skip_missing) return(FALSE)

  if (file.exists(table_paths[["new"]])){
    if (overwrite){
      unlink(table_paths[["new"]], recursive = TRUE)
    }else stop("Existing table found; set overwrite = TRUE: ", table_paths[["new"]])
  }

  dir.create(dataset_path, showWarnings = FALSE)
  dir.create(dirname(table_paths[["new"]]), showWarnings = FALSE)
  file.copy(table_paths[["copy"]], dirname(table_paths[["new"]]), recursive = TRUE)
  if (dataset_name != template_name) file.rename(
    file.path(dirname(table_paths[["new"]]), basename(table_paths[["copy"]])),
    table_paths[["new"]]
  )

  return(TRUE)
}


