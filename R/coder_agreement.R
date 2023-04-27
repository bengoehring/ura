#' coder_agreement
#'
#' \code{coder_agreement} Calculates the percent agreement between each rater and the other raters who coded the same subjects.
#' @param object_name A dataframe or tibble containing raters' codings. Each row should contain the assigned coding
#' from a given rater-subject.
#' @param rater_column The name of the column containing the raters' names as a string.
#' @param subject_column The name of the column containing the names of the subjects being coded as a string.
#' @param coding_column The name of the column containing the codings assigned by the raters as a string.
#' @return A tibble where each row notes the percent agreement between rater i and all other raters who coded the same subjects (percent_agree).
#' The n_dbl_coded column notes how many subjects have been coded by rater i that have also been coded by other raters.
#' @author Benjamin Goehring <bengoehr@umich.edu>
#' @export

coder_agreement <- function(object_name,
                            rater_column,
                            subject_column,
                            coding_column) {

  # run checks and return double-coded subjects
  dbl_coded_df <- int_return_dbl_coded(in_object_name = object_name,
                                       in_rater_column = rater_column,
                                       in_subject_column = subject_column,
                                       in_coding_column = coding_column)


  # Loop through each of the raters to find the matrix containing the codings
  # of rater i and any other rater who coded the same subjects. So, each row
  # will contain two columns: one wil contain the codings made by rater i and the
  # other will contain the codings for the same subjects made by other raters.

  # From there, calculate the percent agreement between rater i and the other
  # raters who coded the same actions and return as a tibble
  raters <- unique(dplyr::pull(dplyr::select(object_name,
                                             dplyr::all_of(rater_column))))
  all_pct_agree <- vector('list',
                          length = length(raters))

  for(i in 1:length(raters)) {

    # return the subjects coded by rater i
    rater_i <- raters[i]

    rater_i_ids <- dbl_coded_df %>%
      dplyr::filter(.data[[rater_column]] == rater_i) %>%
      dplyr::pull(.data[[subject_column]])

    subjects_i <- dbl_coded_df %>%
      dplyr::filter(.data[[subject_column]] %in% rater_i_ids)

    # merge the data with itself to get the double-coded subjects as seperate
    # columns.
    # create column names for merges
    rater_column_x <- stringr::str_c(rater_column, '.x')
    rater_column_y <- stringr::str_c(rater_column, '.y')

    codings_i <- dplyr::left_join(subjects_i,
                           subjects_i,
                           by = subject_column) %>%
      dplyr::filter(.data[[rater_column_x]] == i) %>%
      dplyr::filter(.data[[rater_column_x]] != .data[[rater_column_y]]) %>%
      dplyr::select(-all_of(subject_column),
                    -all_of(rater_column_y),
                    -all_of(rater_column_x))

    # turn into a matrix and calcuate percent agreement.
    codings_i_matrix <- as.matrix(codings_i)

    all_pct_agree[[i]] <- dplyr::tibble(rater = rater_i,
                                 percent_agree = irr::agree(codings_i_matrix)$value,
                                 n_dbl_coded = nrow(codings_i_matrix))
  }

  final <- all_pct_agree %>%
    dplyr::bind_rows()

  return(final)
}

