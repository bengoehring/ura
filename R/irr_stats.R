#' irr_stats
#'
#' \code{irr_stats} Calculates a variety of IRR statistics.
#' @param object_name A dataframe or tibble containing raters' codings. Each row should contain the assigned coding
#' from a given rater-subject.
#' @param rater_column The name of the column containing the raters' names as a string.
#' @param subject_column The name of the column containing the names of the subjects being coded as a string.
#' @param coding_column The name of the column containing the codings assigned by the raters as a string.
#' @param round_digits The number of decimals to round the IRR values by. The default is 2.
#' @param include_n_raters Among subjects coded by more than one rater, across how many raters should IRR statistics be computed?
#' For instance, if include_n_raters = 2, only subjects coded by two and only two raters are included for IRR stats. If all of your raters coded
#' all of your subjects, be sure to set include_n_raters to the number of raters in your dataset. This is done
#' to handle missing values and the likelihood that, in many settings, subjects might not be coded by the same number of raters.
#' @param stats_to_include The IRR statistics to include in the output. See the documentation of the \href{https://cran.r-project.org/web/packages/irr/irr.pdf}{irr package} for more information about the specific statistics.
#' @author Benjamin Goehring <bengoehr@umich.edu>
#' @export
irr_stats <- function(object_name,
                      rater_column,
                      subject_column,
                      coding_column,
                      include_n_raters,
                      round_digits = 2,
                      stats_to_include = c("Percentage agreement",
                                           "Cohen's Kappa",
                                           "Maxwell's RE",
                                           "Krippendorf's Alpha")) {
  # run checks and return multi-coded subjects
  dbl_coded_df <- int_return_dbl_coded(in_object_name = object_name,
                                       in_rater_column = rater_column,
                                       in_subject_column = subject_column,
                                       in_coding_column = coding_column)

  # widen multi-coded observations and create generic rater columns
  dbl_coded_df_wide <- dbl_coded_df %>%
    dplyr::group_by(.data[[subject_column]]) %>%
    dplyr::mutate(generic_rater = stringr::str_c("rater_",
                                           dplyr::row_number())) %>%
    dplyr::ungroup() %>%
    dplyr::select(-dplyr::all_of(rater_column)) %>%
    dplyr::filter(.data[['generic_rater']] %in% c(stringr::str_c("rater_", 1:include_n_raters))) %>%
    tidyr::pivot_wider(names_from = dplyr::all_of('generic_rater'),
                       values_from = dplyr::all_of(coding_column))

  dbl_coded_df_wide <- stats::na.omit(dbl_coded_df_wide)

  # turn into matrix for irr stat functions
  dbl_coded_df_matrix <- dbl_coded_df_wide %>%
    dplyr::select(-dplyr::all_of(subject_column)) %>%
    as.matrix()

  # return irr stats depending on number of comparisons and whether ratings are binary or not
  if(include_n_raters == 2 & length(unique(dplyr::pull(dbl_coded_df, .data[[coding_column]]))) == 2) {
    all_statistics <- list("Percentage agreement" = irr::agree(dbl_coded_df_matrix)$value,
                           "Cohen's Kappa" = irr::kappa2(dbl_coded_df_matrix)$value,
                           "Maxwell's RE" = irr::maxwell(dbl_coded_df_matrix)$value,
                           "Krippendorf's Alpha" = irr::kripp.alpha(t(dbl_coded_df_matrix),
                                                                    method = 'nominal')$value)

    cat("\n\nReturning IRR statistics applicable for comparing 2 coders (see include_n_raters) and binary values.\n\n")

  } else if(include_n_raters > 2 & length(unique(dplyr::pull(dbl_coded_df, .data[[coding_column]]))) == 2) {

    all_statistics <- list("Percentage agreement" = irr::agree(dbl_coded_df_matrix)$value,
                           "Krippendorf's Alpha" = irr::kripp.alpha(t(dbl_coded_df_matrix),
                                                                    method = 'nominal')$value)

    cat("\n\nReturning IRR statistics applicable for comparing >2 coders (see include_n_raters) and binary values.\n\n")

  } else if(include_n_raters == 2 & length(unique(dplyr::pull(dbl_coded_df, .data[[coding_column]]))) > 2) {

    all_statistics <- list("Percentage agreement" = irr::agree(dbl_coded_df_matrix)$value,
                           "Cohen's Kappa" = irr::kappa2(dbl_coded_df_matrix)$value,
                           "Krippendorf's Alpha" = irr::kripp.alpha(t(dbl_coded_df_matrix),
                                                                    method = 'nominal')$value)

    cat("\n\nReturning IRR statistics applicable for comparing 2 coders (see include_n_raters) and non-binary values.\n\n")

  } else if(include_n_raters > 2 & length(unique(dplyr::pull(dbl_coded_df, .data[[coding_column]]))) > 2) {

    all_statistics <- list("Percentage agreement" = irr::agree(dbl_coded_df_matrix)$value,
                           "Krippendorf's Alpha" = irr::kripp.alpha(t(dbl_coded_df_matrix),
                                                                    method = 'nominal')$value)
    cat("\n\nReturning IRR statistics applicable for comparing >2 coders (see include_n_raters) and non-binary values.\n\n")
  }

  all_statistics_df <- tibble::as_tibble(all_statistics) %>%
    tidyr::pivot_longer(cols = dplyr::everything(),
                        names_to = 'statistic') %>%
    dplyr::mutate(value = round(.data[['value']],
                                round_digits)) %>%
    dplyr::filter(.data[['statistic']] %in% stats_to_include) %>%
    dplyr::mutate(n_subjects = nrow(dbl_coded_df_matrix))

  return(all_statistics_df)
}




