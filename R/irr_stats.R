#' irr_stats
#'
#' \code{irr_stats} Calculates a variety of IRR statistics.
#' @param object_name A dataframe or tibble containing raters' codings. Each row should contain the assigned coding
#' from a given rater-subject.
#' @param rater_column The name of the column containing the raters' names as a string.
#' @param subject_column The name of the column containing the names of the subjects being coded as a string.
#' @param coding_column The name of the column containing the codings assigned by the raters as a string.
#' @param round_digits The number of decimals to round the irr values by. The default is 2.
#' @param stats_to_include The IRR statistics to include in the output. See the documentation of the \href{https://cran.r-project.org/web/packages/irr/irr.pdf}{irr package} for more information about the specific statistics.
#' @author Benjamin Goehring <bengoehr@umich.edu>
#' @export
irr_stats <- function(object_name,
                      rater_column,
                      subject_column,
                      coding_column,
                      round_digits = 2,
                      stats_to_include = c("Percentage agreement",
                                           "Cohen's Kappa",
                                           "Maxwell's RE",
                                           "Krippendorf's Alpha")) {
  # run checks and return double-coded subjects
  dbl_coded_df <- int_return_dbl_coded(in_object_name = object_name,
                                       in_rater_column = rater_column,
                                       in_subject_column = subject_column,
                                       in_coding_column = coding_column)

  # widen double-coded observations and create generic coder 1 and 2 columns
  dbl_coded_df_wide <- dbl_coded_df %>%
    dplyr::group_by(.data[[subject_column]]) %>%
    dplyr::mutate(bi_coder = str_c("coder_",
                                   dplyr::row_number())) %>%
    dplyr::ungroup() %>%
    dplyr::select(-all_of(rater_column)) %>%
    tidyr::pivot_wider(names_from = bi_coder,
                       values_from = all_of(coding_column))

  # turn into matrix for irr stat functions
  dbl_coded_df_matrix <- dbl_coded_df_wide %>%
    dplyr::select(-all_of(subject_column)) %>%
    as.matrix()

  # return irr stats
  all_statistics <- list("Percentage agreement" = irr::agree(dbl_coded_df_matrix)$value,
                         "Cohen's Kappa" = irr::kappa2(dbl_coded_df_matrix)$value,
                         "Maxwell's RE" = irr::maxwell(dbl_coded_df_matrix)$value,
                         "Krippendorf's Alpha" = irr::kripp.alpha(t(dbl_coded_df_matrix),
                                                             method = 'nominal')$value)
  all_statistics_df <- dplyr::as_tibble(all_statistics) %>%
    tidyr::pivot_longer(cols = everything(),
                 names_to = 'statistic') %>%
    mutate(value = round(value,
                         2)) %>%
    filter(statistic %in% stats_to_include)


  return(all_statistics_df)
}

