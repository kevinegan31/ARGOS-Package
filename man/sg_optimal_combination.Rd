% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/argos_files.R
\name{sg_optimal_combination}
\alias{sg_optimal_combination}
\title{Optimal Savitzky-Golay Filter Parameters Finder}
\usage{
sg_optimal_combination(x_t, dt = 1, polyorder)
}
\arguments{
\item{x_t}{A numeric vector or one-column matrix. The data to be smoothed.}

\item{dt}{A numeric scalar. The time-step interval of the data. Default is 1.}

\item{polyorder}{A numeric scalar. The order of the polynomial to be used in
the Savitzky-Golay filter. If not specified, 4 will be used
by default.}
}
\value{
A list with three elements:
  - sg_combinations: a matrix where each row represents a combination of
                     polynomial order and window length tried.
  - sg_order_wl: a vector of length 2 with the optimal polynomial order and
                 window length.
  - f_dist: a data frame with the mean squared error of the differences
            between the original data and the smoothed data for each
            combination.
}
\description{
This function finds the optimal parameters for the Savitzky-Golay filter
by evaluating combinations of polynomial orders and window lengths.
}
