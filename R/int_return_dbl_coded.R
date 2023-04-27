#' int_return_dbl_coded
#'
#' \code{int_return_dbl_coded} An internal function to return the subjects double-coded by the raters. It runs a number of checks along the way
#' @param in_in_object_name A dataframe or tibble containing raters' codings. Each row should contain the assigned coding
#' from a given rater-subject.
#' @param in_in_rater_column The name of the column containing the raters' names as a string.
#' @param in_in_subject_column The name of the column containing the names of the subjects being coded as a string.
#' @param in_in_coding_column The name of the column containing the codings assigned by the raters as a string.
#' @author Benjamin Goehring <bengoehr@umich.edu>

int_return_dbl_coded <- function(in_object_name,
                                 in_rater_column,
                                 in_subject_column,
                                 in_coding_column) {
  # checks
  if(is.null(in_object_name)) {
    stop("object_name must be specified.")
  }
  if(!is.null(in_object_name) && !is.data.frame(in_object_name)) {
    stop("object_name must be a dataframe or tibble.")
  }
  if(is.null(in_rater_column) | is.null(in_subject_column) | is.null(in_coding_column)) {
    stop("rater_column, subject_column, and coding_column all must be specified.")
  }
  if(!is.character(in_rater_column) | !is.character(in_subject_column) | !is.character(in_coding_column)) {
    stop("rater_column, subject_column, and coding_column must be provided as strings.")
  }
  if(!all(c(in_rater_column, in_subject_column, in_coding_column) %in% colnames(in_object_name))) {
    stop("Either rater_column, subject_column or coding_column are not present in object_name")
  }
  if({
    in_object_name %>%
      dplyr::group_by(.data[[in_rater_column]],
                      .data[[in_subject_column]]) %>%
      dplyr::filter(n() > 1) %>%
      nrow() != 0
  }) {
    stop("There are duplicates in the dataframe. Please be sure that there are no duplicate rater-subject observations.")
  }

  # filter input data down to only the double-coded observations.
  dbl_coded_df <- in_object_name %>%
    dplyr::group_by(.data[[in_subject_column]]) %>%
    dplyr::filter(n() == 2) %>%
    dplyr::ungroup()

  if(nrow(dbl_coded_df) == 0) {
    stop("There are no subjects coded by more than one rater. Therefore, IRR stats cannot be computed. ")
  }

  if({
    in_object_name %>%
      dplyr::group_by(.data[[in_subject_column]]) %>%
      dplyr::filter(n() > 2) %>%
      dplyr::ungroup() %>%
      nrow() != 0
  }) {
    print('Some subjects in in_object_name are coded by >2 raters. Only keeping subjects that were coded by two (and only two) raters.')
  }

  return(dbl_coded_df)
}

#notes: figure out what types the rater, coding, and subject columns need to be.


