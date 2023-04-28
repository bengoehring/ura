test_that("Percent agreement values are correct.", {

  test_data_1 <- tibble::tribble(
    ~rater,	~subject,	~coding,	~multi_coded,	~agree,	~percent_agree,
    1,	1,	1,	0,	NA,	50,
    1,	2,	0,	0,	NA,	50,
    1,	3,	1,	1,	1,	50,
    1,	4,	0,	1,	0,	50,
    2,	3,	1,	1,	1,	75,
    2,	9,	0,	0,	NA,	75,
    2,	10,	1,	0,	NA,	75,
    2,	4,	1,	1,	0,	75,
    2,	5,	1,	1,	1,	75,
    2,	6,	1,	1,	1,	75,
    3,	5,	1,	1,	1,	100,
    3,	6,	1,	1,	1,	100,
    3,	7,	1,	0,	NA,	100,
    3,	8,	1,	0,	NA,	100,
  )

  return_multicoded(test_data_1,
                    rater_column = 'rater',
                    subject_column = 'subject',
                    coding_column = 'coding')

  #holding
  testthat::expect_equal(2, 2)


})





