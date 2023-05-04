
<!-- README.md is generated from README.Rmd. Please edit that file -->

# ura

<!-- badges: start -->

[![R-CMD-check](https://github.com/bengoehring/ura/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/bengoehring/ura/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

`ura` provides a set of tools for calculating inter-rater reliability
(IRR) statistics by rater, allowing for real-time monitoring of rater
progress and accuracy. While far from the first package to provide users
access to IRR diagnostics (e.g., the great package
[irr](https://cran.r-project.org/web/packages/irr/irr.pdf)), `ura` aims
to provide a simple set of tools for quickly monitoring rater progress
and accuracy. You can use `ura` to, for instance, find the percentage
agreement or Krippendorf’s Alpha of all of the subjects coded by your
raters. Another helpful use is to calculate percentage agreement values
by coder, providing an efficient way to monitor the relative accuracy of
your raters.

This package complements a [working
paper](https://bengoehring.github.io/files/perspectives-paper.pdf)
entitled “Improving Data Collection and Classification: Tips for Working
with Undergraduate Research Assistants.” Please refer to this paper for
a more general discussion about training and monitoring student raters.
Also, be sure to check out the paper for more information about how to
use the tools in `ura` to monitor progress without compromising
reproducibility.

## Installation

`ura` is currently only available on GitHub. You can install it from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("bengoehring/ura")
```

## Usage Examples

### IRR statistics

`ura` can be used to calculate key IRR statistics, such as percentage
agreement and Krippendorf’s Alpha via the `irr_stats()` function. This
function largely serves as a wrapper around `irr::agree()` and
`irr::kripp.alpha()` but aims to simplify users’ lives by only having to
provide a dataframe and specify key columns.

For instance, here I calculate the percentage agreement and Krippendorf
Alpha of the `diagnoses` dataset, which notes the psychiatric
evaluations of 30 patients from 6 raters. The `diagnoses` dataset is
included with the `ura` package and is simply a reshaped version of the
same dataset in the `irr` package.

``` r
library(ura)

# calculate IRR statistics using built in diagnoses dataset 
#   note:  this is the same diagnoses dataset as in the irr package, just pivoted
#   into a long format. 

irr_stats(diagnoses,
          rater_column = 'rater_id',
          subject_column = 'patient_id',
          coding_column = 'diagnosis')
#> # A tibble: 2 × 3
#>   statistic            value n_subjects
#>   <chr>                <dbl>      <int>
#> 1 Percentage agreement 16.7          30
#> 2 Krippendorf's Alpha   0.07         30
```

A few things to note here. First, the unit of analysis in `diagnoses` is
rater-subject — that is, each row provides the coding decision of rater
i for subject j. All data inputted into a `ura` function should be long
by rater-subject. Second, you will see that the dataframe returned by
`irr_stats()` notes the number of subjects used to calcuate the given
IRR statistic. In the case of diagnoses, this value is equal to the
number of unique subjects in the dataframe:

``` r
length(unique(diagnoses$patient_id))
#> [1] 30
```

This is not always the case. If your dataframe includes subjects that
were coded by more than one rater and subjects coded by a single rater
(this is a common approach for balancing efficiency with the need for
IRR statistics), `ura` will automatically only use the subjects coded by
more than one rater. The resulting number of subjects will then appear
in the `n_subjects` column.

### Percentage Agreement by Rater

The `coder_agreement()` function is the key method for monitoring the
accuracy and progress of raters. While `irr_stats()` provides pooled IRR
statistics across all raters, `coder_agreement()` provides the percent
share of a given raters’ codings that agree with other raters’ codings.
In other words, it offers supervisors a method for checking the relative
accuracy of each rater in real time. Since interventions in coding
procedures should be used sparingly, I suggest taking a look at the
working paper linked above for more information about when and why to
intervene based on information gleaned from `coder_agreement()`:

In the snippet below, all raters have the same percent agreement: 17%.
That is because, as implied by the n_multi_coded column, every rater
codes every subject in the diagnoses dataset.

``` r
coder_agreement(diagnoses,
                rater_column = 'rater_id',
                subject_column = 'patient_id',
                coding_column = 'diagnosis')
#> # A tibble: 6 × 3
#>   rater percent_agree n_multi_coded
#>   <int>         <dbl>         <int>
#> 1     1            17            30
#> 2     2            17            30
#> 3     3            17            30
#> 4     4            17            30
#> 5     5            17            30
#> 6     6            17            30
```

A more helpful use case is when you only have your raters multi-code a
subset of subjects. Take this hypothetical dataset, for instance:

``` r
example_data <- tibble::tribble(
    ~rater, ~subject,   ~coding,
    1,  1,  1,
    1,  2,  0,
    1,  3,  1,
    1,  4,  0,
    2,  3,  1,
    2,  9,  0,
    2,  10, 1,
    2,  4,  1,
    2,  5,  1,
    2,  6,  1,
    3,  5,  1,
    3,  6,  1,
    3,  7,  1,
    3,  8,  1,
  )
```

Here, some subjects are coded by multiple raters while others are coded
by a single rater. As a result:

``` r
coder_agreement(example_data,
                rater_column = 'rater',
                subject_column = 'subject',
                coding_column = 'coding')
#> # A tibble: 3 × 3
#>   rater percent_agree n_multi_coded
#>   <int>         <dbl>         <int>
#> 1     1            50             2
#> 2     2            75             4
#> 3     3           100             2
```

In terms of interpretation, row 1 shows that of the 2 subjects coded by
rater 1 that were also coded by another rater, rater 1 agrees with the
other rater(s) 50% of the time. Looking back at example_data, it appears
that rater 1 agreed with rater 2 on the coding of subject 3 but not on
subject 4.
