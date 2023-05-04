anxiety_wide <- anxiety %>%
  tidyr::pivot_wider(values_from = anxiety_level,
              names_from = rater_id,
              names_prefix = "rater_")

anxiety_matrix <- anxiety_wide %>%
  dplyr::select(-subject_id) %>%
  as.matrix()

diagnoses_wide <- diagnoses %>%
  tidyr::pivot_wider(values_from = 'diagnosis',
              names_from = 'rater_id',
              names_prefix = "rater_")

diagnoses_matrix <- diagnoses_wide %>%
  dplyr::select(-patient_id) %>%
  dplyr::mutate(across(everything(),
                as.numeric)) %>%
  as.matrix()

anxiety_test <- irr_stats(anxiety,
                          rater_column = "rater_id",
                          subject_column = "subject_id",
                          coding_column = "anxiety_level",
                          include_n_raters = length(unique(anxiety$rater_id)))

diagnoses_test <- irr_stats(diagnoses,
                            rater_column = "rater_id",
                            subject_column = "patient_id",
                            coding_column = "diagnosis",
                            include_n_raters = length(unique(diagnoses$rater_id)))

test_that("Percent agreement value is correct (anxiety)", {

  anxiety_agree_test <- anxiety_test %>%
    dplyr::filter(statistic == "Percentage agreement") %>%
    dplyr::pull(value)
  expect_equal(irr::agree(anxiety_matrix)$value,
               anxiety_agree_test)

})

test_that("Percent agreement value is correct (diagnoses)", {

   diagnoses_agree_test <- diagnoses_test %>%
     dplyr::filter(statistic == "Percentage agreement") %>%
     dplyr::pull(value)
  expect_equal(round(irr::agree(diagnoses_matrix)$value, 2),
               round(diagnoses_agree_test, 2))

})

test_that("Kirppendorf's alpha is correct (anxiety)", {

  anxiety_kripp_test <- anxiety_test %>%
    dplyr::filter(statistic == "Krippendorf's Alpha") %>%
    dplyr::pull(value)
  expect_equal(round(irr::kripp.alpha(anxiety_matrix)$value, 2),
               anxiety_kripp_test)

})

test_that("Kirppendorf's alpha is correct (diagnoses)", {

  diagnoses_kripp_test <- diagnoses_test %>%
    dplyr::filter(statistic == "Krippendorf's Alpha") %>%
    dplyr::pull(value)
  expect_equal(round(irr::kripp.alpha(diagnoses_matrix)$value, 2),
               diagnoses_kripp_test)

})

test_that("Number of subjects is correct (anxiety)", {
  n_subs_test_anxiety <- anxiety_test %>%
    dplyr::pull(n_subjects) %>%
    unique()
  stopifnot(length(n_subs_test_anxiety) == 1)

  expect_equal(irr::agree(anxiety_matrix)$subjects,
               n_subs_test_anxiety)
})

test_that("Number of subjects is correct (diagnoses)", {
  n_subs_test_diagnoses <- diagnoses_test %>%
    dplyr::pull(n_subjects) %>%
    unique()
  stopifnot(length(n_subs_test_diagnoses) == 1)

  expect_equal(irr::agree(diagnoses_matrix)$subjects,
               n_subs_test_diagnoses)
})




