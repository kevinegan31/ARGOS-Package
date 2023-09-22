#' @title Simulate a Two-Dimensional Damped Harmonic Oscillator with Cubic Dynamics
#' @description This function simulates a two-dimensional cubic system. It is a wrapper for the Python function \code{cubic2d_ode}, which uses a set of ordinary differential equations (ODEs) to simulate the system. Noise can be added to the system.
#' @param n_obs A numeric value specifying the number of observations or time points for which you want to simulate the system.
#' @param dt A numeric value specifying the time step size for the simulation.
#' @param init_conditions A numeric vector specifying the initial conditions for the simulation. It should contain two elements corresponding to the initial values of the two state variables.
#' @param snr A numeric value specifying the signal-to-noise ratio in dB. This is used to add noise to the simulation. If \code{snr} equals 0, no noise is added.
#' @return A matrix where each row corresponds to a time point and each column corresponds to a state variable. The matrix represents the simulated state of the system over time.
#' @export
#' @examples
#' # Simulate a system for 100 time points with a time step size of 0.01,
#' # initial conditions of c(1, 0) and a signal-to-noise ratio of 49 dB.
#' sim <- cubic2d_system(n_obs=5000, 0.01, c(1, 0), 49)
#' @importFrom reticulate py_eval
#' @import reticulate
cubic2d_system <- function(n_obs, dt, init_conditions, snr) {
  py$cubic2d_ode(n_obs, dt, init_conditions, snr)
}
#' @title Simulate a Duffing Oscillator
#' @description This function simulates a duffing system. It is a wrapper for the Python function \code{duffing_ode}, which uses a set of ordinary differential equations (ODEs) to simulate the system. Noise can be added to the system.
#' @param n_obs A numeric value specifying the number of observations or time points for which you want to simulate the system.
#' @param dt A numeric value specifying the time step size for the simulation.
#' @param init_conditions A numeric vector specifying the initial conditions for the simulation. It should contain two elements corresponding to the initial values of the two state variables.
#' @param snr A numeric value specifying the signal-to-noise ratio in dB. This is used to add noise to the simulation. If \code{snr} equals 0, no noise is added.
#' @return A matrix where each row corresponds to a time point and each column corresponds to a state variable. The matrix represents the simulated state of the system over time.
#' @export
#' @examples
#' # Simulate a system for 100 time points with a time step size of 0.01,
#' # initial conditions of c(1, 0) and a signal-to-noise ratio of 49 dB.
#' sim <- duffing_oscillator(n_obs=5000, 0.01, c(1, 0), 49)
#' @importFrom reticulate py_eval
duffing_oscillator <- function(n_obs, dt, init_conditions, snr) {
  py$duffing_ode(n_obs, dt, init_conditions, snr)
}
#' @title Simulate a Two-Dimensional Damped Harmonic Oscillator with Linear Dynamics
#' @description This function simulates a two-dimensional linear system. It is a wrapper for the Python function \code{linear2d_ode}, which uses a set of ordinary differential equations (ODEs) to simulate the system. Noise can be added to the system.
#' @param n_obs A numeric value specifying the number of observations or time points for which you want to simulate the system.
#' @param dt A numeric value specifying the time step size for the simulation.
#' @param init_conditions A numeric vector specifying the initial conditions for the simulation. It should contain two elements corresponding to the initial values of the two state variables.
#' @param snr A numeric value specifying the signal-to-noise ratio in dB. This is used to add noise to the simulation. If \code{snr} equals 0, no noise is added.
#' @return A matrix where each row corresponds to a time point and each column corresponds to a state variable. The matrix represents the simulated state of the system over time.
#' @export
#' @examples
#' # Simulate a system for 100 time points with a time step size of 0.01,
#' # initial conditions of c(1, 0) and a signal-to-noise ratio of 49 dB.
#' sim <- linear2d_system(n_obs=5000, 0.01, c(1, 0), 49)
#' @importFrom reticulate py_eval
linear2d_system <- function(n_obs, dt, init_conditions, snr) {
  py$linear2d_ode(n_obs, dt, init_conditions, snr)
}
#' @title Simulate a Three-Dimensional Linear System
#' @description This function simulates a three-dimensional linear system. It is a wrapper for the Python function \code{linear3d_ode}, which uses a set of ordinary differential equations (ODEs) to simulate the system. Noise can be added to the system.
#' @param n_obs A numeric value specifying the number of observations or time points for which you want to simulate the system.
#' @param dt A numeric value specifying the time step size for the simulation.
#' @param init_conditions A numeric vector specifying the initial conditions for the simulation. It should contain two elements corresponding to the initial values of the two state variables.
#' @param snr A numeric value specifying the signal-to-noise ratio in dB. This is used to add noise to the simulation. If \code{snr} equals 0, no noise is added.
#' @return A matrix where each row corresponds to a time point and each column corresponds to a state variable. The matrix represents the simulated state of the system over time.
#' @export
#' @examples
#' # Simulate a system for 100 time points with a time step size of 0.01,
#' # initial conditions of c(2, 1, 0) and a signal-to-noise ratio of 49 dB.
#' sim <- linear3d_system(n_obs=5000, 0.01, c(2, 1, 0), 49)
#' @importFrom reticulate py_eval
linear3d_system <- function(n_obs, dt, init_conditions, snr) {
  py$linear3d_ode(n_obs, dt, init_conditions, snr)
}
#' @title Simulate the Lorenz Chaotic System
#' @description This function simulates the Lorenz chaotic system. It is a wrapper for the Python function \code{lorenz_ode}, which uses a set of ordinary differential equations (ODEs) to simulate the system. Noise can be added to the system.
#' @param n_obs A numeric value specifying the number of observations or time points for which you want to simulate the system.
#' @param dt A numeric value specifying the time step size for the simulation.
#' @param init_conditions A numeric vector specifying the initial conditions for the simulation. It should contain two elements corresponding to the initial values of the two state variables.
#' @param snr A numeric value specifying the signal-to-noise ratio in dB. This is used to add noise to the simulation. If \code{snr} equals 0, no noise is added.
#' @return A matrix where each row corresponds to a time point and each column corresponds to a state variable. The matrix represents the simulated state of the system over time.
#' @export
#' @examples
#' # Simulate a system for 100 time points with a time step size of 0.01,
#' # initial conditions of c(-8, 8, 7) and a signal-to-noise ratio of 49 dB.
#' sim <- lorenz_system(n_obs=5000, 0.01, c(-8, 8, 7), 49)
#' @importFrom reticulate py_eval
lorenz_system <- function(n_obs, dt, init_conditions, snr) {
  py$lorenz_ode(n_obs, dt, init_conditions, snr)
}
#' @title Simulate the Rossler System
#' @description This function simulates the Rossler system. It is a wrapper for the Python function \code{rossler_ode}, which uses a set of ordinary differential equations (ODEs) to simulate the system. Noise can be added to the system.
#' @param n_obs A numeric value specifying the number of observations or time points for which you want to simulate the system.
#' @param dt A numeric value specifying the time step size for the simulation.
#' @param init_conditions A numeric vector specifying the initial conditions for the simulation. It should contain two elements corresponding to the initial values of the two state variables.
#' @param snr A numeric value specifying the signal-to-noise ratio in dB. This is used to add noise to the simulation. If \code{snr} equals 0, no noise is added.
#' @return A matrix where each row corresponds to a time point and each column corresponds to a state variable. The matrix represents the simulated state of the system over time.
#' @export
#' @examples
#' # Simulate a system for 100 time points with a time step size of 0.01,
#' # initial conditions of c(2, 0, 1) and a signal-to-noise ratio of 49 dB.
#' sim <- rossler_system(n_obs=5000, 0.01, c(2, 0, 1), 49)
#' @importFrom reticulate py_eval
rossler_system <- function(n_obs, dt, init_conditions, snr) {
  py$rossler_ode(n_obs, dt, init_conditions, snr)
}
#' @title Simulate the Van der Pol Oscillator
#' @description This function simulates the Van der Pol Oscillator. It is a wrapper for the Python function \code{vdp_ode}, which uses a set of ordinary differential equations (ODEs) to simulate the system. Noise can be added to the system.
#' @param n_obs A numeric value specifying the number of observations or time points for which you want to simulate the system.
#' @param dt A numeric value specifying the time step size for the simulation.
#' @param init_conditions A numeric vector specifying the initial conditions for the simulation. It should contain two elements corresponding to the initial values of the two state variables.
#' @param mu A numeric value for specifying the damping of the system.
#' @param snr A numeric value specifying the signal-to-noise ratio in dB. This is used to add noise to the simulation. If \code{snr} equals 0, no noise is added.
#' @return A matrix where each row corresponds to a time point and each column corresponds to a state variable. The matrix represents the simulated state of the system over time.
#' @export
#' @examples
#' # Simulate a system for 100 time points with a time step size of 0.01,
#' # initial conditions of c(2, 0), mu = 1.2, and a signal-to-noise ratio of 49 dB.
#' sim <- vdp_oscillator(n_obs=5000, 0.01, c(2, 0), 1.2, 49)
#' @importFrom reticulate py_eval
vdp_oscillator <- function(n_obs, dt, init_conditions, mu, snr) {
  py$vdp_ode(n_obs, dt, init_conditions, mu, snr)
}
