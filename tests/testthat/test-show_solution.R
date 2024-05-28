test_that("Pedigree problem solution - AD", {
  ped <- sim_pedigree_problem(inheritance = "AD", time_limit = Inf, seed = 156)

  solution <- data.frame(mother = c(NA, 11, 24, 11),
                         father = c(NA, 10, 14, 10),
                         child = c(NA, 12, 26, 12),
                         id = c(115, 104, 105, 104),
                         moi = c("AD", "AR", "XD", "XR"),
                         likelihood = c("confirmed", "excluded",
                                        "excluded", "excluded"))
  row.names(solution) <- c("AD", "AR", "XD", "XR")

  expect_identical(show_solution(ped), solution)

  solution_step1 <- solution
  solution_step1["AD", "likelihood"] <- NA
  solution_step2 <- solution_step1
  solution_step2["AD", c("mother", "father", "child", "id")] <- NA
  solution_step3 <- solution_step2
  solution_step3["AR", "likelihood"] <- NA
  solution_step4 <- solution_step3
  solution_step4["AR", c("mother", "father", "child", "id")] <- NA

  expect_identical(show_solution(ped, step = 1), solution_step1)
  expect_identical(show_solution(ped, step = 2), solution_step2)
  expect_identical(show_solution(ped, step = 3), solution_step3)
  expect_identical(show_solution(ped, step = 4), solution_step4)

  solution_step5 <- solution
  solution_step5[c("AR", "XR"), "likelihood"] <- NA
  solution_step6 <- solution_step5
  solution_step6[c("AR", "XR"), c("mother", "father", "child", "id")] <- NA

  expect_identical(show_solution(ped, step = 5), solution_step5)
  expect_identical(show_solution(ped, step = 6), solution_step6)

  expect_error(show_solution(ped, step = 7))
  expect_error(show_solution(ped, step = 8))
  expect_error(show_solution(ped, step = 9))
  expect_error(show_solution(ped, step = 10))
  expect_error(show_solution(ped, step = 11))

  solution_step12 <- solution
  solution_step12[c("AD", "XD"),
                  c("mother", "father", "child", "id", "likelihood")] <- NA

  expect_identical(show_solution(ped, step = 12), solution_step12)
})

test_that("Pedigree problem solution - AR", {
  ped <- sim_pedigree_problem(inheritance = "AR", time_limit = Inf, seed = 102)

  solution <- data.frame(mother = c(11, NA, 11, 11),
                         father = c(10, NA, 10, 10),
                         child = c(13, NA, 13, 13),
                         id = c(101, 115, 101, 101),
                         moi = c("AD", "AR", "XD", "XR"),
                         likelihood = c("excluded", "confirmed",
                                        "excluded", "excluded"))
  row.names(solution) <- c("AD", "AR", "XD", "XR")

  expect_identical(show_solution(ped), solution)

  solution_step1 <- solution
  solution_step1["AR", "likelihood"] <- NA
  solution_step2 <- solution_step1
  solution_step2["AR", c("mother", "father", "child", "id")] <- NA
  solution_step3 <- solution_step2
  solution_step3["AD", "likelihood"] <- NA
  solution_step4 <- solution_step3
  solution_step4["AD", c("mother", "father", "child", "id")] <- NA

  expect_identical(show_solution(ped, step = 1), solution_step1)
  expect_identical(show_solution(ped, step = 2), solution_step2)
  expect_identical(show_solution(ped, step = 3), solution_step3)
  expect_identical(show_solution(ped, step = 4), solution_step4)

  solution_step5 <- solution
  solution_step5[c("AD", "XR"), "likelihood"] <- NA
  solution_step6 <- solution_step5
  solution_step6[c("AD", "XR"), c("mother", "father", "child", "id")] <- NA

  expect_identical(show_solution(ped, step = 5), solution_step5)
  expect_identical(show_solution(ped, step = 6), solution_step6)

  expect_error(show_solution(ped, step = 7))
  expect_error(show_solution(ped, step = 8))
  expect_error(show_solution(ped, step = 9))
  expect_error(show_solution(ped, step = 10))

  solution_step11 <- solution
  solution_step11[c("AR", "XR"),
                  c("mother", "father", "child", "id", "likelihood")] <- NA

  expect_identical(show_solution(ped, step = 11), solution_step11)

  expect_error(show_solution(ped, step = 12))
})

test_that("Pedigree problem solution - XD", {
  ped <- sim_pedigree_problem(inheritance = "XD", force = TRUE, seed = 196)

  solution <- data.frame(mother = c(NA, 36, NA, 36),
                         father = c(NA, 35, NA, 35),
                         child = c(NA, 37, NA, 37),
                         id = c(111, 104, 111, 104),
                         moi = c("AD", "AR", "XD", "XR"),
                         likelihood = c("unlikely", "excluded",
                                        "likely", "excluded"))
  row.names(solution) <- c("AD", "AR", "XD", "XR")

  expect_identical(show_solution(ped), solution)

  expect_error(show_solution(ped, step = 1))
  expect_error(show_solution(ped, step = 2))
  expect_error(show_solution(ped, step = 3))
  expect_error(show_solution(ped, step = 4))
  expect_error(show_solution(ped, step = 5))
  expect_error(show_solution(ped, step = 6))
  expect_error(show_solution(ped, step = 7))
  expect_error(show_solution(ped, step = 8))

  solution_step9 <- solution
  solution_step9[c("AD", "XD"), "likelihood"] <- NA
  solution_step10 <- solution_step9
  solution_step10[c("AD", "XD"), "id"] <- NA

  expect_identical(show_solution(ped, step = 9), solution_step9)
  expect_identical(show_solution(ped, step = 10), solution_step10)

  expect_error(show_solution(ped, step = 11))

  solution_step12 <- solution
  solution_step12[c("AD", "XD"),
                  c("mother", "father", "child", "id", "likelihood")] <- NA

  expect_identical(show_solution(ped, step = 12), solution_step12)
})

test_that("Pedigree problem solution - XR", {
  ped <- sim_pedigree_problem(inheritance = "XR", force = TRUE, seed = 109)

  solution <- data.frame(mother = c(11, NA, 11, NA),
                         father = c(10, NA, 10, NA),
                         child = c(12, NA, 12, NA),
                         id = c(102, 113, 102, 113),
                         moi = c("AD", "AR", "XD", "XR"),
                         likelihood = c("excluded", "unlikely",
                                        "excluded", "likely"))
  row.names(solution) <- c("AD", "AR", "XD", "XR")

  expect_identical(show_solution(ped), solution)

  expect_error(show_solution(ped, step = 1))
  expect_error(show_solution(ped, step = 2))
  expect_error(show_solution(ped, step = 3))
  expect_error(show_solution(ped, step = 4))
  expect_error(show_solution(ped, step = 5))
  expect_error(show_solution(ped, step = 6))

  solution_step7 <- solution
  solution_step7[c("AR", "XR"), "likelihood"] <- NA
  solution_step8 <- solution_step7
  solution_step8[c("AR", "XR"), "id"] <- NA

  expect_identical(show_solution(ped, step = 7), solution_step7)
  expect_identical(show_solution(ped, step = 8), solution_step8)

  expect_error(show_solution(ped, step = 9))
  expect_error(show_solution(ped, step = 10))

  solution_step11 <- solution
  solution_step11[c("AR", "XR"),
                  c("mother", "father", "child", "id", "likelihood")] <- NA

  expect_identical(show_solution(ped, step = 11), solution_step11)

  expect_error(show_solution(ped, step = 12))
})
