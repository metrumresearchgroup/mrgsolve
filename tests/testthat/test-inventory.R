context("inventory")

test_that("inven_report displays warns and missing items", {
  obj <- data.frame(KA = 1, CL = 1, V = 1, OCC = 1, F = 1)
  expect_equal(inven_report(obj, c("KA", "CL")), vector(mode = "character"))
  expect_equal(inven_report(obj, c("KA", "OCC2")), c("OCC2"))
  expect_warning(inven_report(obj, c("KA", "OCC2")), "The object is missing these parameters:\\n - OCC2")
})
