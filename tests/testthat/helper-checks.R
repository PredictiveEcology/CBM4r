
expect_equal_dir <- function(dirTest, dirExpect){

  dir_md5sum <- lapply(list(
    test   = dirTest,
    expect = dirExpect
  ), function(d){
    fs <- list.files(d, recursive = TRUE)
    setNames(tools::md5sum(file.path(d, fs)), dirname(fs))
  })

  expect_equal(dir_md5sum$test, dir_md5sum$expect)
}

