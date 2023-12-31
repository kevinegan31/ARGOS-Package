% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/argos_files.R
\name{linear2d_system}
\alias{linear2d_system}
\title{Linear 2D System}
\usage{
linear2d_system(n, init_conditions, dt, snr = Inf)
}
\arguments{
\item{n}{Number of time points (rounded to the nearest integer).}

\item{init_conditions}{Initial conditions as a numeric vector of length 2.}

\item{dt}{Time step between observations.}

\item{snr}{Signal-to-noise ratio (in dB). Use Inf for no noise.}
}
\value{
A numeric matrix representing the system's state over time. Each row
        corresponds to a time point, and each column represents a variable.
}
\description{
Simulates a two-dimensional damped oscillator with linear dynamics and optional
noise.
}
\details{
This function simulates a two-dimensional damped oscillator with linear dynamics.
It uses the specified time step and initial conditions to compute the system's
state over time. If a non-Infinite SNR is provided, Gaussian noise is added to
the system.
}
\examples{
# Simulate a 2D linear system with 100 time points and no noise
data <- linear2d_system(n = 100, init_conditions = c(-1, 1), dt = 0.01, snr = Inf)

}
