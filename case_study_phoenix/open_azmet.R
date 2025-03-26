open_azmet <- function(path = NULL) {
  azmet <- readRDS(
    paste0(
      path,
      "./input/azmet_2023070108_2023080108.rds"
    )
  )
  return(azmet)
}
