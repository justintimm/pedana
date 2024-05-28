test_that("Argument analysis: claim evaluation", {
  ped <- sim_pedigree_problem(inheritance = "XR", force = T, seed = 110)

  argument <- list(id = 1,
                   claim = "excludedAD",
                   proof = "const",
                   father = 16,
                   mother = 24,
                   child = 25)

  result <- check_argument(ped, argument)
  expect_match(result$fb, pedana:::tr("fb_conclusion_correct_evidence_correct_const"))
  expect_identical(result$conclusion_score, 1)
  expect_identical(result$evidence_score, 1)

  argument <- list(id = 1,
                   claim = "likelyXR",
                   proof = "carrier_frequency_low",
                   father = NA,
                   mother = NA,
                   child = NA)

  result <- check_argument(ped, argument, elaborated_feedback = FALSE)
  expect_match(result$fb, pedana:::tr("fb_conclusion_correct_evidence_correct_superficial"))
  expect_identical(result$conclusion_score, 1)
  expect_identical(result$evidence_score, 1)

  argument <- list(id = 1,
                   claim = "excludedAD",
                   proof = "few_affected",
                   father = NA,
                   mother = NA,
                   child = NA)

  result <- check_argument(ped, argument, elaborated_feedback = FALSE)
  expect_match(result$fb, pedana:::tr("fb_conclusion_correct_evidence_incorrect"))
  expect_identical(result$conclusion_score, 1)
  expect_identical(result$evidence_score, 0)

  argument <- list(id = 1,
                   claim = "confirmedXR",
                   proof = "mainly_m",
                   father = NA,
                   mother = NA,
                   child = NA)

  result <- check_argument(ped, argument, elaborated_feedback = FALSE)
  expect_match(result$fb, pedana:::tr("fb_conclusion_incorrect"))
  expect_identical(result$conclusion_score, 0)
  expect_identical(result$evidence_score, 0)
})

test_that("Argument analysis: evaluation of family constellation based explanations I/II", {
  ped <- sim_pedigree_problem(inheritance = "AR", time = Inf, seed = 134)

  argument <- list(id = 1,
                   claim = "unlikelyAD",
                   proof = "const",
                   father = 12,
                   mother = 13,
                   child = 14)

  result <- check_argument(ped, argument)
  expect_true(grepl(pattern = pedana:::tr("fb_weak_claim_strong_proof_const"),
                    x = result$fb,
                    fixed = TRUE))
  expect_identical(result$conclusion_score, 0)
  expect_identical(result$evidence_score, 0)

  argument <- list(id = 1,
                   claim = "excludedAD",
                   proof = "const",
                   father = 10,
                   mother = 11,
                   child = 14)

  result <- check_argument(ped, argument)
  expect_true(grepl(pattern = pedana:::tr("fb_const_unknown"),
                    x = result$fb,
                    fixed = TRUE))
  expect_identical(result$conclusion_score, 1)
  expect_identical(result$evidence_score, 0)
})

test_that("Argument analysis: evaluation of family constellation based explanations II/II", {
  pedAR <- sim_pedigree_problem(inheritance = "AR", time = Inf, seed = 134)

  # id 101
  # AD, XD, XR excluded
  argument <- list(id = 1,
                   claim = "confirmedAD",
                   proof = "const",
                   father = 12,
                   mother = 13,
                   child = 14)

  result <- check_argument(pedAR, argument)
  expect_true(grepl(pattern = sprintf(pedana:::tr("fb_const_101"),
                                      pedana:::tr("claim_excludedAD")),
                    x = result$fb,
                    fixed = TRUE))
  expect_identical(result$conclusion_score, 0)
  expect_identical(result$evidence_score, 0)

  # id 102
  # AD, XD excluded
  argument <- list(id = 1,
                   claim = "confirmedAD",
                   proof = "const",
                   father = 12,
                   mother = 13,
                   child = 16)

  result <- check_argument(pedAR, argument)
  expect_true(grepl(pattern = sprintf(pedana:::tr("fb_const_102"),
                                      pedana:::tr("claim_excludedAD")),
                    x = result$fb,
                    fixed = TRUE))
  expect_identical(result$conclusion_score, 0)
  expect_identical(result$evidence_score, 0)

  # id 105
  # XD excluded
  argument <- list(id = 1,
                   claim = "confirmedXD",
                   proof = "const",
                   father = 16,
                   mother = 19,
                   child = 21)

  result <- check_argument(pedAR, argument)
  expect_true(grepl(pattern = sprintf(pedana:::tr("fb_const_105"),
                                      pedana:::tr("claim_excludedXD")),
                    x = result$fb,
                    fixed = TRUE))
  expect_identical(result$conclusion_score, 0)
  expect_identical(result$evidence_score, 0)

  # id 108
  # XR excluded
  argument <- list(id = 1,
                   claim = "confirmedXR",
                   proof = "const",
                   father = 10,
                   mother = 11,
                   child = 12)

  result <- check_argument(pedAR, argument)
  expect_true(grepl(pattern = sprintf(pedana:::tr("fb_const_108"),
                                      pedana:::tr("claim_excludedXR")),
                    x = result$fb,
                    fixed = TRUE))
  expect_identical(result$conclusion_score, 0)
  expect_identical(result$evidence_score, 0)

  pedAD <- sim_pedigree_problem(inheritance = "AD", time = Inf, seed = 141)

  # id 103
  # AR, XD, XR excluded
  argument <- list(id = 1,
                   claim = "confirmedAR",
                   proof = "const",
                   father = 10,
                   mother = 11,
                   child = 12)

  result <- check_argument(pedAD, argument)
  expect_true(grepl(pattern = sprintf(pedana:::tr("fb_const_103"),
                                      pedana:::tr("claim_excludedAR")),
                    x = result$fb,
                    fixed = TRUE))
  expect_identical(result$conclusion_score, 0)
  expect_identical(result$evidence_score, 0)

  # id 106
  # XD excluded
  argument <- list(id = 1,
                   claim = "confirmedXD",
                   proof = "const",
                   father = 13,
                   mother = 18,
                   child = 19)

  result <- check_argument(pedAD, argument)
  expect_true(grepl(pattern = sprintf(pedana:::tr("fb_const_106"),
                                      pedana:::tr("claim_excludedXD")),
                    x = result$fb,
                    fixed = TRUE))
  expect_identical(result$conclusion_score, 0)
  expect_identical(result$evidence_score, 0)

  pedXD <- sim_pedigree_problem(inheritance = "XD", force = T, seed = 141)

  # id 104
  # AR, XR excluded
  argument <- list(id = 1,
                   claim = "confirmedAR",
                   proof = "const",
                   father = 40,
                   mother = 41,
                   child = 42)

  result <- check_argument(pedXD, argument)
  expect_true(grepl(pattern = sprintf(pedana:::tr("fb_const_104"),
                                      pedana:::tr("claim_excludedAR")),
                    x = result$fb,
                    fixed = TRUE))
  expect_identical(result$conclusion_score, 0)
  expect_identical(result$evidence_score, 0)

  # id 107
  # XR excluded
  argument <- list(id = 1,
                   claim = "confirmedXR",
                   proof = "const",
                   father = 10,
                   mother = 11,
                   child = 12)

  result <- check_argument(pedXD, argument)
  expect_identical(result$conclusion_score, 0)
  expect_identical(result$evidence_score, 0)

})

test_that("Argument analysis: evaluation of superficial explanations", {
  ped <- sim_pedigree_problem(inheritance = "AR", time = Inf, seed = 127)

  argument <- list(id = 1,
                   claim = "excludedAD",
                   proof = "few_affected",
                   father = NA,
                   mother = NA,
                   child = NA)

  result <- check_argument(ped, argument)
  expect_true(grepl(pattern = pedana:::tr("fb_prevalence"),
                    x = result$fb,
                    fixed = TRUE))
  expect_identical(result$conclusion_score, 1)
  expect_identical(result$evidence_score, 0)

  argument <- list(id = 1,
                   claim = "excludedAR",
                   proof = "many_affected",
                   father = NA,
                   mother = NA,
                   child = NA)

  result <- check_argument(ped, argument)
  expect_true(grepl(pattern = pedana:::tr("fb_prevalence"),
                    x = result$fb,
                    fixed = TRUE))
  expect_identical(result$conclusion_score, 0)
  expect_identical(result$evidence_score, 0)

  argument <- list(id = 1,
                   claim = "excludedAR",
                   proof = "every_gen",
                   father = NA,
                   mother = NA,
                   child = NA)

  result <- check_argument(ped, argument)
  expect_true(grepl(pattern = pedana:::tr("fb_distribution_pattern"),
                    x = result$fb,
                    fixed = TRUE))
  expect_identical(result$conclusion_score, 0)
  expect_identical(result$evidence_score, 0)

  argument <- list(id = 1,
                   claim = "excludedAD",
                   proof = "not_every_gen",
                   father = NA,
                   mother = NA,
                   child = NA)

  result <- check_argument(ped, argument)
  expect_true(grepl(pattern = pedana:::tr("fb_distribution_pattern"),
                    x = result$fb,
                    fixed = TRUE))
  expect_identical(result$conclusion_score, 1)
  expect_identical(result$evidence_score, 0)

  argument <- list(id = 1,
                   claim = "excludedAD",
                   proof = "not_every_gen",
                   father = NA,
                   mother = NA,
                   child = NA)

  result <- check_argument(ped, argument)
  expect_true(grepl(pattern = pedana:::tr("fb_distribution_pattern"),
                    x = result$fb,
                    fixed = TRUE))
  expect_identical(result$conclusion_score, 1)
  expect_identical(result$evidence_score, 0)
})

test_that("Argument analysis: evaluation of X-linked specific explanations", {
  ped <- sim_pedigree_problem(inheritance = "XR", force = TRUE, seed = 199)

  argument <- list(id = 1,
                   claim = "excludedAR",
                   proof = "carrier_frequency_low",
                   father = NA,
                   mother = NA,
                   child = NA)

  result <- check_argument(ped, argument)
  expect_true(grepl(pattern = pedana:::tr("fb_strong_claim_weak_proof"),
                    x = result$fb,
                    fixed = TRUE))
  expect_identical(result$conclusion_score, 0)
  expect_identical(result$evidence_score, 0)

  argument <- list(id = 1,
                   claim = "unlikelyXR",
                   proof = "carrier_frequency_low",
                   father = NA,
                   mother = NA,
                   child = NA)

  result <- check_argument(ped, argument)
  expect_true(grepl(pattern = pedana:::tr("fb_carrier_frequency"),
                    x = result$fb,
                    fixed = TRUE))
  expect_identical(result$conclusion_score, 0)
  expect_identical(result$evidence_score, 0)

  argument <- list(id = 1,
                   claim = "likelyXR",
                   proof = "carrier_frequency_high",
                   father = NA,
                   mother = NA,
                   child = NA)

  result <- check_argument(ped, argument)
  expect_true(grepl(pattern = pedana:::tr("fb_carrier_frequency"),
                    x = result$fb,
                    fixed = TRUE))
  expect_identical(result$conclusion_score, 1)
  expect_identical(result$evidence_score, 0)

  argument <- list(id = 1,
                   claim = "unlikelyXD",
                   proof = "no_male_to_male",
                   father = NA,
                   mother = NA,
                   child = NA)

  result <- check_argument(ped, argument)
  expect_true(grepl(pattern = pedana:::tr("fb_no_male_to_male"),
                    x = result$fb,
                    fixed = TRUE))
  expect_identical(result$conclusion_score, 0)
  expect_identical(result$evidence_score, 0)
})

test_that("Argument analysis: evaluation of gender ratio based explanations", {
  ped <- sim_pedigree_problem(inheritance = "XR", force = TRUE, seed = 202)

  argument <- list(id = 1,
                   claim = "unlikelyXR",
                   proof = "mainly_m",
                   father = NA,
                   mother = NA,
                   child = NA)

  result <- check_argument(ped, argument)
  expect_true(grepl(pattern = pedana:::tr("fb_mainly_m"),
                    x = result$fb,
                    fixed = TRUE))
  expect_identical(result$conclusion_score, 0)
  expect_identical(result$evidence_score, 0)

  argument <- list(id = 1,
                   claim = "likelyXD",
                   proof = "mainly_f",
                   father = NA,
                   mother = NA,
                   child = NA)

  result <- check_argument(ped, argument)
  expect_true(grepl(pattern = pedana:::tr("fb_mainly_f"),
                    x = result$fb,
                    fixed = TRUE))
  expect_identical(result$conclusion_score, 0)
  expect_identical(result$evidence_score, 0)

  argument <- list(id = 1,
                   claim = "likelyAR",
                   proof = "m_and_f",
                   father = NA,
                   mother = NA,
                   child = NA)

  result <- check_argument(ped, argument)
  expect_true(grepl(pattern = pedana:::tr("fb_gender_ratio_likely_autosomal"),
                    x = result$fb,
                    fixed = TRUE))
  expect_identical(result$conclusion_score, 0)
  expect_identical(result$evidence_score, 0)

  argument <- list(id = 1,
                   claim = "likelyXR",
                   proof = "m_and_f",
                   father = NA,
                   mother = NA,
                   child = NA)

  result <- check_argument(ped, argument)
  expect_true(grepl(pattern = pedana:::tr("fb_gender_ratio_likely_X-linked"),
                    x = result$fb,
                    fixed = TRUE))
  expect_identical(result$conclusion_score, 1)
  expect_identical(result$evidence_score, 0)
})
