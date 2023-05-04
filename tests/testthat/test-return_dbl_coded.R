test_data_2 <- tibble::tribble(
  ~rater,	~subject,	~coding,	~multi_coded,	~agree,	~percent_agree,
  1,	2,	0,	0,	NA,	NA,
  2,	3,	10,	0,	NA,	NA,
  3,	4,	1,	0,	NA,	NA,
  4,	5,	2,	0,	NA,	NA,
  5,	6,	3,	0,	NA,	NA,
  6,	7,	4,	0,	NA,	NA
)

test_that("No multi-coded rows throws an error.", {
  testthat::expect_error(return_multicoded(test_data_2,
                                           rater_column = "rater",
                                           subject_column = 'subject',
                                           coding_column = "coding"),
                         "There are no subjects coded by more than one rater. Therefore, IRR stats cannot be computed.")
})


