library(tidyverse)
library(irr)

test_data <- tibble(coder_1 = sample(c(1, 2, 3), replace = T, size = 1000),
                    coder_2 = sample(c(1, 2, 3), replace = T, size = 1000))

test_2_no_missing <- tibble(coder_1 = sample(c(1, 2, 3), replace = T, size = 1000),
                            coder_2 = sample(c(1, 2, 3), replace = T, size = 1000))

# create a test dataset where different coders are coding different subjects
test_9_missing <- tibble(coder = c(rep(1, 50),
                                 rep(2, 50),
                                 rep(3, 50),
                                 rep(4, 50),
                                 rep(5, 50),
                                 rep(6, 50),
                                 rep(7, 50),
                                 rep(8, 50),
                                 rep(9, 50))) %>%
  group_by(coder) %>%
  mutate(action_id = case_when(
    coder == 1 ~ 1:50,
    coder == 2 ~ 41:90,
    coder == 3 ~ 81:130,
    coder == 4 ~ 121:170,
    coder == 5 ~ 161:210,
    coder == 6 ~ 201:250,
    coder == 7 ~ 241:290,
    coder == 8 ~ 281:330,
    coder == 9 ~ c(1:25, 85:95, 331:344)
  )) %>%
  ungroup() %>%
  mutate(coverage = sample(c(0,
                             1),
                           size = n(),
                           replace = T,
                           prob = c(.75,
                                    .25)))


irr_stats(test_9_missing,
          rater_column = 'coder',
          subject_column = 'action_id',
          coding_column = 'coverage',
          include_n_raters = 3)

irr_stats(anxiety,
          rater_column = 'rater_id',
          subject_column = 'subject_id',
          coding_column = 'anxiety_level',
          include_n_raters = 3)

irr_stats(diagnoses,
          rater_column = 'rater_id',
          subject_column = 'patient_id',
          coding_column = 'diagnosis',
          include_n_raters = 9)





