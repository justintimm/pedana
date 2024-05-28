test_that("Simulate pedigree problems", {
  ped1 <- sim_pedigree_problem(inheritance = "AD", time_limit = Inf, seed = 101)
  ped2 <- sim_pedigree_problem(inheritance = "AR", time_limit = Inf, seed = 101)
  ped3 <- sim_pedigree_problem(inheritance = "XD", time_limit = Inf, seed = 101)
  ped4 <- sim_pedigree_problem(inheritance = "XR", time_limit = Inf, seed = 101)

  expect_output(str(ped1$data), "12 obs")
  expect_output(str(ped2$data), "9 obs")
  expect_output(str(ped3$data), "30 obs")
  expect_output(str(ped4$data), "17 obs")
})

test_that("Simulating pedigree problems may fail", {
  expect_error(sim_pedigree_problem(inheritance = "XD", iter_limit = 1,
                                    time_limit = Inf, seed = 103))
  expect_error(sim_pedigree_problem(inheritance = "XR", iter_limit = 1,
                                    time_limit = Inf, seed = 103))
})

test_that("Simulating pedigree problems should not fail when using force", {
  ped5 <- sim_pedigree_problem(inheritance = "XD", force = T, seed = 103)
  ped6 <- sim_pedigree_problem(inheritance = "XR", force = T, seed = 103)
  expect_s3_class(ped5, "ped")
  expect_s3_class(ped6, "ped")
})
