"""dynamical_systems.py"""
import numpy as np
from scipy.integrate import odeint


def cubic2d_ode(n_obs, dt, init_conditions, snr):
    """cubic2d system"""
    cubic_matrix = np.array([-0.1, 2, -2, -0.1])

    def cubic2d(x_t, time, parameters):
        """Two-dimensional damped harmonic oscillator with cubic dynamics"""
        return [
            parameters[0] * x_t[0] ** 3 + parameters[1] * x_t[1] ** 3,
            parameters[2] * x_t[0] ** 3 + parameters[3] * x_t[1] ** 3,
        ]

    t_span = np.arange(0, float(n_obs) * dt, dt)
    x_total = odeint(cubic2d, init_conditions, t_span, args=(cubic_matrix,))
    snr_db = 10 ** -(snr / 20)
    # Add noise (dB)
    if snr_db != 0:
        x_init = x_total.copy()
        for i in range(int(x_total.shape[1])):
            x_total[:, i] = x_total[:, i] + snr_db * np.random.normal(
                scale=np.std(x_init[:, i]), size=x_init[:, i].shape
            )

    return x_total


# Duffing Oscillator
# Matrix parameters

def duffing_ode(n_obs, dt, init_conditions, snr):
    """Duffing oscillator"""
    duffing_params = np.array([0.1, 1, 5])


    def duffing(x_t, time, parameters):
        """Duffing Oscillator"""
        return [
            x_t[1],
            (
                (-parameters[0] * x_t[1])
                - (parameters[1] * x_t[0])
                - (parameters[2] * (x_t[0] ** 3))
            ),
        ]
        
    t_span = np.arange(0, float(n_obs) * dt, dt)
    x_total = odeint(duffing, init_conditions, t_span, args=(duffing_params,))
    snr_db = 10 ** -(snr / 20)
    # Add noise (dB)
    if snr_db != 0:
        x_init = x_total.copy()
        for i in range(int(x_total.shape[1])):
            x_total[:, i] = x_total[:, i] + snr_db * np.random.normal(
                scale=np.std(x_init[:, i]), size=x_init[:, i].shape
            )

    return x_total

# Linear2D
def linear2d_ode(n_obs, dt, init_conditions, snr):
    """Linear2D system"""
    linear2d_params = np.array([-0.1, 2, -2, -0.1])


    def linear2d(x_t, time, parameters):
        """Two-dimensional damped harmonic oscillator with linear dynamics"""
        return [
            parameters[0] * x_t[0] + parameters[1] * x_t[1],
            parameters[2] * x_t[0] + parameters[3] * x_t[1],
        ]
    t_span = np.arange(0, float(n_obs) * dt, dt)
    x_total = odeint(linear2d, init_conditions, t_span, args=(linear2d_params,))
    snr_db = 10 ** -(snr / 20)
    # Add noise (dB)
    if snr_db != 0:
        x_init = x_total.copy()
        for i in range(int(x_total.shape[1])):
            x_total[:, i] = x_total[:, i] + snr_db * np.random.normal(
                scale=np.std(x_init[:, i]), size=x_init[:, i].shape
            )

    return x_total

# Linear3D
def linear3d_ode(n_obs, dt, init_conditions, snr):
    """Linear3D system"""
    linear3d_params = np.array([-0.1, 2, -2, -0.1, -0.3])


    def linear3d(x_t, time, parameters):
        """Three-dimensional linear system"""
        return [
            parameters[0] * x_t[0] + parameters[1] * x_t[1],
            parameters[2] * x_t[0] + parameters[3] * x_t[1],
            parameters[4] * x_t[2],
        ]
        
    t_span = np.arange(0, float(n_obs) * dt, dt)
    x_total = odeint(linear3d, init_conditions, t_span, args=(linear3d_params,))
    snr_db = 10 ** -(snr / 20)
    # Add noise (dB)
    if snr_db != 0:
        x_init = x_total.copy()
        for i in range(int(x_total.shape[1])):
            x_total[:, i] = x_total[:, i] + snr_db * np.random.normal(
                scale=np.std(x_init[:, i]), size=x_init[:, i].shape
            )

    return x_total

# Lorenz system
def lorenz_ode(n_obs, dt, init_conditions, snr):
    """Lorenz system"""
    lorenz_params = np.array([10, 28, -8 / 3])


    def lorenz_eq(x_t, t, parameters):
        """Lorenz system"""
        return [
            parameters[0] * (x_t[1] - x_t[0]),
            x_t[0] * (parameters[1] - x_t[2]) - x_t[1],
            x_t[0] * x_t[1] + parameters[2] * x_t[2],
        ]
        
    t_span = np.arange(0, float(n_obs) * dt, dt)
    x_total = odeint(lorenz_eq, init_conditions, t_span, args=(lorenz_params,))
    snr_db = 10 ** -(snr / 20)
    # Add noise (dB)
    if snr_db != 0:
        x_init = x_total.copy()
        for i in range(int(x_total.shape[1])):
            x_total[:, i] = x_total[:, i] + snr_db * np.random.normal(
                scale=np.std(x_init[:, i]), size=x_init[:, i].shape
            )

    return x_total

# Rossler system
def rossler_ode(n_obs, dt, init_conditions, snr):
    """Rossler system"""
    ross_a = 0.2
    ross_b = 0.2
    ross_c = 5.7
    rossler_params = np.array([ross_a, ross_b, ross_c])

    def rossler_eq(x_t, time, parameters):
        """Rossler Equations"""
        return [
            -x_t[1] - x_t[2],
            x_t[0] + (parameters[0] * x_t[1]),
            parameters[1] + (x_t[2] * (x_t[0] - parameters[2])),
        ]
        
    t_span = np.arange(0, float(n_obs) * dt, dt)
    x_total = odeint(rossler_eq, init_conditions, t_span, args=(rossler_params,))
    snr_db = 10 ** -(snr / 20)
    # Add noise (dB)
    if snr_db != 0:
        x_init = x_total.copy()
        for i in range(int(x_total.shape[1])):
            x_total[:, i] = x_total[:, i] + snr_db * np.random.normal(
                scale=np.std(x_init[:, i]), size=x_init[:, i].shape
            )

    return x_total

# VdP
def vdp_ode(n_obs, dt, init_conditions, mu, snr):
    """Expand system"""
    def vdp_osc(x_t, time, mu):
        """Van der Pol Oscillator"""
        return [x_t[1], mu * (1 - x_t[0] ** 2) * x_t[1] - x_t[0]]
    
    t_span = np.arange(0, float(n_obs) * dt, dt)
    x_total = odeint(vdp_osc, init_conditions, t_span, args=(mu,))
    snr_db = 10 ** -(snr / 20)
    # Add noise (dB)
    if snr_db != 0:
        x_init = x_total.copy()
        for i in range(int(x_total.shape[1])):
            x_total[:, i] = x_total[:, i] + snr_db * np.random.normal(
                scale=np.std(x_init[:, i]), size=x_init[:, i].shape
            )

    return x_total

# Lotka-Volterra
def lotka_volterra_ode(n_obs, dt, init_conditions, snr):
    params = np.array([1, -1, -1, 1])
    def lotka_volterra(x, t, a):
        return[
            (a[0] * x[0]) + (a[1] * (x[0] * x[1])),
            (a[2] * x[1]) + (a[3] * (x[0] * x[1])),
        ]
    t_span = np.arange(0, float(n_obs) * dt, dt)
    x_total = odeint(lotka_volterra, init_conditions, t_span, args=(params,))
    snr_db = 10 ** -(snr / 20)
    # Add noise (dB)
    if snr_db != 0:
        x_init = x_total.copy()
        for i in range(int(x_total.shape[1])):
            x_total[:, i] = x_total[:, i] + snr_db * np.random.normal(
                scale=np.std(x_init[:, i]), size=x_init[:, i].shape
            )

    return x_total