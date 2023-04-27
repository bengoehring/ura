## code to prepare diagnoses dataset goes here

# diagnoses dataset -- cleaning this up and making it long
data("diagnoses")
diagnoses_long <- diagnoses %>%
  mutate(patient_id = row_number()) %>%
  pivot_longer(cols = rater1:rater6,
               names_to = 'rater_id',
               values_to = 'diagnosis') %>%
  mutate(rater_id = as.numeric(str_remove(rater_id, 'rater'))) %>%
  mutate(diagnosis = str_squish(str_to_lower(str_remove(diagnosis,
                                                        "^\\d{1}\\.\\s")))) %>%
  mutate(diagnosis = as.factor(diagnosis)) %>%
  mutate(rater_id = as.numeric(rater_id)) %>%
  mutate(patient_id = as.numeric(patient_id))


diagnoses <- diagnoses_long
usethis::use_data(diagnoses,
                  overwrite = TRUE)
