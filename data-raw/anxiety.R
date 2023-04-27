## code to prepare anxiety dataset goes here

# anxiety dataset -- cleaning this up and making it long
data("anxiety")
anxiety_long <- anxiety %>%
  mutate(subject_id = row_number()) %>%
  pivot_longer(cols = rater1:rater3,
               names_to = 'rater_id',
               values_to = 'anxiety_level') %>%
  mutate(rater_id = as.numeric(str_remove(rater_id, 'rater'))) %>%
  mutate(across(.fns = as.numeric))


anxiety <- anxiety_long
usethis::use_data(anxiety,
                  overwrite = TRUE)
