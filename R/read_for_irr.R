#' read_for_irr
#'
#' \code{read_for_irr} reads in raters' codings for further analysis from a variety
#' of formats
#' @param object_name A dataframe or tibble containing raters' codings. Each row should contain the assigned coding
#' from a given rater-subject.
#' @param rater_column The name of the column containing the raters' names as a string.
#' @param subject_column The name of the column containing the names of the subjects being coded as a string.
#' @param coding_column The name of the column containing the codings assigned by the raters as a string.
#' @param round_digits The number of decimals to round the irr values by. The default is 2.
#' @author Benjamin Goehring <bengoehr@umich.edu>
#' @export
read_for_irr <- function(object_name,
                         rater_column,
                         subject_column,
                         coding_column,
                         round_digits = 2) {
  # checks
  if(is.null(object_name)) {
    stop("object_name must be specified.")
  }
  if(!is.null(object_name) && !is.data.frame(object_name)) {
    stop("object_name must be a dataframe or tibble.")
  }
  if(is.null(rater_column) | is.null(subject_column) | is.null(coding_column)) {
    stop("rater_column, subject_column, and coding_column all must be specified.")
  }
  if(!is.character(rater_column) | !is.character(subject_column) | !is.character(coding_column)) {
    stop("rater_column, subject_column, and coding_column all must be strings.")
  }
  if(!all(c(rater_column, subject_column, coding_column) %in% colnames(object_name))) {
    stop("Either rater_column, subject_column or coding_column are not present in object_name")
  }
  if({
    object_name %>%
      dplyr::group_by(.data[[rater_column]],
               .data[[subject_column]]) %>%
      dplyr::filter(n() > 1) %>%
      nrow() != 0
  }) {
    stop("There are duplicates in the dataframe. Please be sure that there are no duplicate rater-subject observations.")
  }

  # filter input data down to only the double-coded observations.
  dbl_coded_df <- object_name %>%
    dplyr::group_by(.data[[subject_column]]) %>%
    dplyr::filter(n() == 2) %>%
    dplyr::ungroup()

  if(nrow(dbl_coded_df) == 0) {
    stop("There are no subjects coded by more than one rater. Therefore, IRR stats cannot be computed. ")
  }

  if({
    object_name %>%
      dplyr::group_by(.data[[subject_column]]) %>%
      dplyr::filter(n() > 2) %>%
      dplyr::ungroup() %>%
      nrow() != 0
  }) {
    print('Some subjects in object_name are coded by >2 raters. Only keeping subjects that were coded by two (and only two) raters.')
  }

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
                         2))
  return(all_statistics_df)
}

