test_that("dose_dm", {
  expect_equal(nrow(dose_dm(dose_data)), 2325)
})

test_that("dose_dm with follow_up", {
  expect_equal(nrow(dose_dm(dose_data, last_follow_up_date="1991-04-28")), 599)
})

test_that("new_var", {
  d <- dose_new_variables(dose_dm(data = dose_data))
  expect_equal(length(names(d)), 19)
})

test_that("external", {
  d <- dose_external(dose_new_variables(dose_dm(data = dose_data)),
                     organ_ls = c("Thyroid"))
  expect_equal(mean(d$Dext_Thyroid), 0.03401813, tolerance = 10^(-6))
})

test_that("internal", {
  d <- dose_internal_ing(dose_new_variables(dose_dm(data = dose_data)),
                     organ_ls = c("Thyroid"))
  expect_equal(mean(d$Dint_Thyroid), 0.01446772, tolerance = 10^(-6))
})

test_that("milk", {
  d <- dose_internal_milk(dose_new_variables(dose_dm(data = dose_data)),
                     organ_ls = c("Thyroid"))
  expect_equal(mean(d$Dmilk_Thyroid), 0.001954314, tolerance = 10^(-6))
})

test_that("inhalation", {
  d <- dose_inhalation(dose_new_variables(dose_dm(data = dose_data)),
                     organ_ls = c("Thyroid"))
  expect_equal(mean(d$Dinh_Thyroid), 0.01018488, tolerance = 10^(-6))
})

test_that("all", {
  d <- calculate_dose(data = dose_data,
                       organ_ls = c("Colon"))
  expect_equal(mean(d$Dtot_Colon), 0.04963588, tolerance = 10^(-6))
})

test_that("all id", {
  d <- dose_total_per_person(calculate_dose(data = dose_data,
                      organ_ls = c("Colon")))
  expect_equal(mean(d$Dtot_Colon), 1.165691, tolerance = 10^(-6))
})
