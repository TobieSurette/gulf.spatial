context("Function tests")
library(gulf.spatial)

test_that("function deg2str works", {
  expect_match(gulf.spatial::deg2str(45), "45°")
  expect_match(gulf.spatial::deg2str(47.5), "47°30'")
}
)

test_that("function deg2dmm works", {
  expect_equal(gulf.spatial::deg2dmm(45.25), 4515)
}
)

test_that("function dms2deg works", {
  expect_equal(gulf.spatial::dms2deg(475142), 47.86167, tolerance=1e-3)
}
)

# test_that("function deg2km works", {
#   expect_equal(gulf.spatial::deg2km(-63.123, 47.56)$x, 490.747, tolerance=1e-3)
#   expect_equal(gulf.spatial::deg2km(-63.123, 47.56)$y, 5267.405, tolerance=1e-3)
# }
# )
