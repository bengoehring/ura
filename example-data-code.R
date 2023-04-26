example_data <- tibble(coder = c(rep(1, 50),
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
    coder == 9 ~ 321:370
  )) %>%
  ungroup() %>%
  mutate(coverage = sample(c(0,
                             1),
                           size = n(),
                           replace = T,
                           prob = c(.75,
                                    .25)))
