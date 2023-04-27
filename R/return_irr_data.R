#' return_irr_data
#'
#' \code{return_irr_data} returns a matrix containing the subjects double-coded by the raters.
#' The returned matrix is the same object used to calculate irr stats in \code{irr_stats}.
#' @param object_name A dataframe or tibble containing raters' codings. Each row should contain the assigned coding
#' from a given rater-subject.
#' @param rater_column The name of the column containing the raters' names as a string.
#' @param subject_column The name of the column containing the names of the subjects being coded as a string.
#' @param coding_column The name of the column containing the codings assigned by the raters as a string.
#' @return A matrix containing the coding of a given subject for a generic 'coder 1' and 'coder 2'. Coders 1 and 2 do not represent actual coders (e.g., you might have >2). Rather, they are general placeholders used to compare reliability across double-coded subjects.
#' @author Benjamin Goehring <bengoehr@umich.edu>
#' @export
return_irr_data <- function(object_name,
                            rater_column,
                            subject_column,
                            coding_column) {
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

  return(dbl_coded_df_matrix)
}







