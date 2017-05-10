context("inventory")

test_that("inven_report displays warns and missing items", {
  obj <- data.frame(KA = 1, CL = 1, V = 1, OCC = 1, F = 1)
  inven_report <- mrgsolve:::inven_report
  expect_equal(inven_report(obj, c("KA", "CL")), vector(mode = "character"))
  expect_equal(inven_report(obj, c("KA", "OCC2")), c("OCC2"))
  expect_warning(inven_report(obj, c("KA", "OCC2")), "The object is missing these parameters:\\n - OCC2")
})

mod <- mcode("test", "$PARAM KA = 1, CL = 1, V = 1, OCC1 = 1, OCC2 = 1, F = 1")
all_obj <- data.frame(KA = 1, CL = 1, V = 1, OCC1 = 1, OCC2 = 1, F = 1)
missing_obj <- data.frame(KA = 1, CL = 1, V = 1, OCC1 = 1, F = 1)
  
test_that("inventory works", {
  # no requirements gives back model 
  expect_s4_class(inventory(mod, all_obj), "mrgmod")
 
  # everything says it found all and returns original model 
  expect_message(inventory(mod, all_obj, dplyr::everything()), "Found all required parameters")
  expect_s4_class(inventory(mod, all_obj, dplyr::everything()), "mrgmod")
  
})

test_that("inventory errors when missing required params", {
  expect_error(inventory(mod, missing_obj, dplyr::everything()), 
               "The object is missing required parameters:\\n- OCC2")
  expect_error(inventory(mod, missing_obj, contains("OCC")), 
               "The object is missing required parameters:\\n- OCC2")
  expect_error(inventory(mod, missing_obj, V:F), 
               "The object is missing required parameters:\\n- OCC2")
  expect_error(inventory(mod, missing_obj, OCC2), 
               "The object is missing required parameters:\\n- OCC2")
})

test_that("inventory warns when missing required params but not checking strictly", {
  expect_warning(inventory(mod, missing_obj, 
                            dplyr::everything(), 
                            .strict = FALSE), 
                 "The object is missing these parameters:\\n - OCC2")
  expect_s4_class(inventory(mod, missing_obj, 
                            dplyr::everything(), 
                            .strict = FALSE), "mrgmod")
  expect_warning(inventory(mod, missing_obj, 
                            OCC2, 
                            .strict = FALSE), 
                 "The object is missing these parameters:\\n - OCC2")
  expect_s4_class(inventory(mod, missing_obj, 
                            OCC2, 
                            .strict = FALSE), "mrgmod")
})