test_that("find_cells works", {
  # open north carolina polygon
  nc <- terra::vect(system.file("shape/nc.shp", package = "sf"))
  expect_no_error(find_cells(nc))
  expect_true(740 %in% find_cells(nc))
})
