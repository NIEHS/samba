# dataframe created with brassens
open_mesonet <- function(yyyymm, path = NULL) {
  x <- readRDS(
    paste0(
      path,
      "./input/mesonet/mesonet_",
      yyyymm,
      "_nyc_nj.rds"
    )
  )
  return(x)
}