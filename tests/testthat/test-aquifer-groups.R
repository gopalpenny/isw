test_that("numeric aquifer grouping reuses normalized physical properties", {
  wells <- example_pumping_wells
  wells$K <- rep(wells$K[1], nrow(wells))
  wells$D <- rep(wells$D[1], nrow(wells))
  wells$V <- rep(wells$V[1], nrow(wells))
  metric_groups <- isw:::.prepare_aquifer_groups(isw:::.prepare_numeric_pumping_wells(wells))
  mixed_units <- wells
  mixed_units$K <- units::set_units(wells$K, "ft/hour")
  mixed_units$D <- units::set_units(wells$D, "ft")
  mixed_groups <- isw:::.prepare_aquifer_groups(isw:::.prepare_numeric_pumping_wells(mixed_units))
  expect_identical(metric_groups$aquifer_id, rep("aquifer_1", nrow(wells)))
  expect_identical(mixed_groups$aquifer_id, metric_groups$aquifer_id)
  prepared <- isw:::.prepare_numeric_pumping_wells(wells)
  local_mocked_bindings(
    .validate_pumping_wells = function(...) stop("unexpected repeated validation"),
    check_dimensionality = function(...) stop("unexpected repeated unit conversion"),
    .package = "isw"
  )
  expect_identical(isw:::.get_aquifer_groups(prepared), metric_groups)
})

test_that("aquifer identifier relationships are checked during input preparation", {
  wells <- example_pumping_wells
  wells$aquifer_id <- rep("shared", nrow(wells))
  wells$K[2] <- wells$K[1] * 2
  expect_error(isw:::.prepare_aquifer_groups(isw:::.prepare_numeric_pumping_wells(wells)),
               "identical K, D, and V")
  wells$aquifer_id <- rep(NA_character_, nrow(wells))
  expect_error(isw:::.prepare_aquifer_groups(isw:::.prepare_numeric_pumping_wells(wells)),
               "nonmissing, nonempty")
})
