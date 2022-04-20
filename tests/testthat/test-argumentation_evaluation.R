test_that("Argumentation", {
  ped9 <- sim_pedigree_problem(inheritance = "AD", time_limit = Inf, seed = 110)

  argument1 <- list(id = 1,
                    claim = "excludedAR",
                    proof = "const",
                    father = 10,
                    mother = 11,
                    child = 12)
  argument2 <- list(id = 2,
                    claim = "confirmedXR",
                    proof = "mainly_m",
                    father = NA,
                    mother = NA,
                    child = NA)
  argument3 <- list(id = 3,
                    claim = "unlikelyXR",
                    proof = "not_every_gen",
                    father = NA,
                    mother = NA,
                    child = NA)
  argument4 <- list(id = 4,
                    claim = "confirmedAD",
                    proof = "other_arguments",
                    father = NA,
                    mother = NA,
                    child = NA)

  argumentation <- list('arg-1' = argument1,
                        'arg-2' = argument2,
                        'arg-3' = argument3,
                        'arg-4' = argument4)

  result <- check_argumentation(ped9, argumentation)

  expect_identical(result$table$likelihood_input,
                   c("confirmed", "excluded", NA, "confirmed"))
  expect_identical(result$table$score,
                   c(0, 1, 0, 0))
  expect_identical(result$table$unique_statements,
                   c(1L, 1L, 0L, 2L))
})

test_that("Argument", {
  ped10 <- sim_pedigree_problem(inheritance = "XR", force = T, seed = 110)

  argument <- list(id = 1,
                   claim = "excludedAD",
                   proof = "const",
                   father = 16,
                   mother = 24,
                   child = 25)

  result <- check_argument(ped10, argument, l = "de")
  expect_match(result$fb, pedana:::tr("fb_const_102", l = "de"))
  expect_identical(result$score, 1)

  argument <- list(id = 1,
                   claim = "excludedAD",
                   proof = "few_affected",
                   father = NA,
                   mother = NA,
                   child = NA)
  result <- check_argument(ped10, argument, l = "de")
  expect_match(result$fb, pedana:::tr("fb_prevalence", l = "de"))
  expect_identical(result$score, 0)

  argument$proof <- "not_every_gen"
  result <- check_argument(ped10, argument, l = "de")
  expect_match(result$fb, pedana:::tr("fb_distribution_pattern", l = "de"))
  expect_identical(result$score, 0)

  argument$proof <- "other_arguments"
  result <- check_argument(ped10, argument, l = "de")
  expect_identical(result$fb, NA)
  expect_identical(result$score, NA)


  argument$proof <- "no_male_to_male"
  result <- check_argument(ped10, argument, l = "de")
  expect_match(result$fb, pedana:::tr("fb_strong_claim_weak_proof", l = "de"))
  expect_identical(result$score, 0)

  argument$claim <- "likelyXD"
  result <- check_argument(ped10, argument, l = "de")
  expect_match(result$fb, pedana:::tr("fb_no_male_to_male", l = "de"))
  expect_identical(result$score, 0)
})
