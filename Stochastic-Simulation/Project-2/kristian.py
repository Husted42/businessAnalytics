import numpy as np
import matplotlib.pyplot as plt

# 1. Simulation Parameters
T = 120        # Total time (days)
dt = 0.1       # Time step size (small step for stochastic accuracy)
n_steps = int(T / dt)

N = 1000000        # Total population
I0 = 50            # Initial infections
R0 = 0             # Initial recoveries
S0 = N - I0 - R0   # Initial susceptibles

# Initialize tracking arrays over time
S_history = np.zeros(n_steps)
I_history = np.zeros(n_steps)
R_history = np.zeros(n_steps)

S_history[0] = S0
I_history[0] = I0
R_history[0] = R0

# 2. Distribution Parameters (The "Well-Implemented" Setup)
# We want a mean beta of 0.3 and mean gamma of 0.1 (R0 = 3.0)
beta_mean = 0.3
gamma_mean = 0.1

# Variance dictates how "noisy" or volatile the daily rates are
beta_var = 0.02
gamma_var = 0.005

# Mathematical conversion from (Mean, Variance) -> (Shape, Scale) for the Gamma Distribution
beta_shape = (beta_mean ** 2) / beta_var
beta_scale = beta_var / beta_mean

gamma_shape = (gamma_mean ** 2) / gamma_var
gamma_scale = gamma_var / gamma_mean

# 3. Time-Stepping Simulation Loop
for t in range(1, n_steps):
    S = S_history[t-1]
    I = I_history[t-1]
    R = R_history[t-1]
    
    # Draw stochastic daily rates from our Gamma distributions
    beta_t = np.random.gamma(beta_shape, beta_scale)
    gamma_t = np.random.gamma(gamma_shape, gamma_scale)
    
    # Calculate deterministic drift based on the randomly sampled rates
    dS = - (beta_t * S * I) / N
    dI = ((beta_t * S * I) / N) - (gamma_t * I)
    dR = gamma_t * I
    
    # Update states (with dt scaling) and clip to keep values physically bounded between 0 and N
    S_history[t] = np.clip(S + dS * dt, 0, N)
    I_history[t] = np.clip(I + dI * dt, 0, N)
    R_history[t] = np.clip(R + dR * dt, 0, N)

# 4. Plotting the Stochastic Run
time_axis = np.linspace(0, T, n_steps)

plt.figure(figsize=(10, 6))
plt.plot(time_axis, S_history, 'b-', alpha=0.6, label='Susceptible')
plt.plot(time_axis, I_history, 'r-', linewidth=2, label='Infected (Stochastic)')
plt.plot(time_axis, R_history, 'g-', alpha=0.6, label='Recovered')

plt.title(f"Stochastic SIR Model (Mean $R_0$ = {beta_mean/gamma_mean:.1f})")
plt.xlabel("Days")
plt.ylabel("Population")
plt.grid(True, linestyle=':', alpha=0.6)
plt.legend(loc='best')
plt.show()