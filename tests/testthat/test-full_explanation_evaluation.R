test_that("autosomal dominant pedigree: correct explanation", {
  ped <- sim_pedigree_problem(inheritance = "AD", time_limit = Inf, seed = 110)

  argument1 <- list(id = 1,
                    claim = "excludedAR",
                    proof = "const",
                    father = 10,
                    mother = 11,
                    child = 12)
  argument2 <- list(id = 2,
                    claim = "excludedXD",
                    proof = "const",
                    father = 13,
                    mother = 17,
                    child = 18)
  argument3 <- list(id = 3,
                    claim = "excludedXR",
                    proof = "const",
                    father = 10,
                    mother = 11,
                    child = 12)
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

  result <- check_argumentation(ped, argumentation)

  expect_identical(result$table$conclusion_score, c(1, 1, 1, 1))
  expect_identical(result$table$evidence_score, c(1, 1, 1, 1))
})

test_that("autosomal recessive pedigree: incorrect explanation", {
  ped <- sim_pedigree_problem(inheritance = "AR", time_limit = Inf, seed = 193)

  argument1 <- list(id = 1,
                    claim = "excludedAD",
                    proof = "few_affected",
                    father = NA,
                    mother = NA,
                    child = NA)
  argument2 <- list(id = 2,
                    claim = "excludedXD",
                    proof = "not_every_gen",
                    father = NA,
                    mother = NA,
                    child = NA)
  argument3 <- list(id = 3,
                    claim = "excludedXR",
                    proof = "m_and_f",
                    father = NA,
                    mother = NA,
                    child = NA)
  argument4 <- list(id = 4,
                    claim = "confirmedAR",
                    proof = "other_arguments",
                    father = NA,
                    mother = NA,
                    child = NA)

  argumentation <- list('arg-1' = argument1,
                        'arg-2' = argument2,
                        'arg-3' = argument3,
                        'arg-4' = argument4)

  result <- check_argumentation(ped, argumentation)

  expect_identical(result$table$conclusion_score, c(1, 1, 1, 1))
  expect_identical(result$table$evidence_score, c(0, 0, 0, 0))
})


test_that("X-linked dominant pedigree: correct explanation", {
  ped <- sim_pedigree_problem(inheritance = "XD", force = TRUE, seed = 148)

  argument1 <- list(id = 1,
                    claim = "excludedAR",
                    proof = "const",
                    father = 68,
                    mother = 69,
                    child = 70)
  argument2 <- list(id = 2,
                    claim = "likelyXD",
                    proof = "no_male_to_male",
                    father = NA,
                    mother = NA,
                    child = NA)
  argument3 <- list(id = 3,
                    claim = "unlikelyAD",
                    proof = "no_male_to_male",
                    father = NA,
                    mother = NA,
                    child = NA)
  argument4 <- list(id = 4,
                    claim = "excludedXR",
                    proof = "const",
                    father = 31,
                    mother = 16,
                    child = 32)

  argumentation <- list('arg-1' = argument1,
                        'arg-2' = argument2,
                        'arg-3' = argument3,
                        'arg-4' = argument4)

  result <- check_argumentation(ped, argumentation)

  expect_identical(result$table$conclusion_score,
                   c(1, 1, 1, 1))
  expect_identical(result$table$evidence_score,
                   c(1, 1, 1, 1))
})

test_that("X-linked recessive pedigree: correct explanation", {
  ped <- sim_pedigree_problem(inheritance = "XR", force = TRUE, seed = 178)

  argument1 <- list(id = 1,
                    claim = "unlikelyAR",
                    proof = "mainly_m",
                    father = NA,
                    mother = NA,
                    child = NA)
  argument2 <- list(id = 2,
                    claim = "excludedXD",
                    proof = "const",
                    father = 16,
                    mother = 13,
                    child = 19)
  argument3 <- list(id = 3,
                    claim = "likelyXR",
                    proof = "carrier_frequency_low",
                    father = NA,
                    mother = NA,
                    child = NA)
  argument4 <- list(id = 4,
                    claim = "excludedAD",
                    proof = "const",
                    father = 20,
                    mother = 14,
                    child = 21)

  argumentation <- list('arg-1' = argument1,
                        'arg-2' = argument2,
                        'arg-3' = argument3,
                        'arg-4' = argument4)

  result <- check_argumentation(ped, argumentation)

  expect_identical(result$table$conclusion_score,
                   c(1, 1, 1, 1))
  expect_identical(result$table$evidence_score,
                   c(1, 1, 1, 1))
})
