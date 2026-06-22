import numpy as np
import matplotlib.pyplot as plt
from scipy.integrate import solve_ivp


#################### -------------------- SISD ODE Model -------------------- ####################

def sisd_ode(t, y, beta, gamma, mu):
    """
    Deterministic SISD model using differential equations.

    Compartments
    ------------
    S : Susceptible individuals
    I : Infected individuals
    D : Dead individuals

    Transitions
    -----------
    S -> I : infection
    I -> S : recovery
    I -> D : death
    """
    S, I, D = y

    # Alive population
    N = S + I

    # Avoid division by zero
    if N <= 0:
        infection_rate = 0
    else:
        infection_rate = beta * S * I / N

    recovery_rate = gamma * I
    death_rate = mu * I

    dSdt = -infection_rate + recovery_rate
    dIdt = infection_rate - recovery_rate - death_rate
    dDdt = death_rate

    return [dSdt, dIdt, dDdt]


def simulate_sisd_ode(N, I0, beta, gamma, mu, t_max=160):
    """
    Solve the SISD differential equation model.
    """
    S0 = N - I0
    D0 = 0

    y0 = [S0, I0, D0]

    t_eval = np.linspace(0, t_max, 1000)

    solution = solve_ivp(
        fun=sisd_ode,
        t_span=(0, t_max),
        y0=y0,
        args=(beta, gamma, mu),
        t_eval=t_eval
    )

    return solution.t, solution.y.T


def plot_sisd_ode(times, states, title="Deterministic SISD model"):
    """
    Plot S, I and D over time.
    """
    S = states[:, 0]
    I = states[:, 1]
    D = states[:, 2]

    plt.figure(figsize=(10, 5))

    plt.plot(times, S, label="Susceptible")
    plt.plot(times, I, label="Infected")
    plt.plot(times, D, label="Dead")

    plt.xlabel("Time")
    plt.ylabel("Number of individuals")
    plt.title(title)
    plt.legend()
    plt.grid(True)

    plt.tight_layout()
    plt.show()


def main():
    # Population settings
    N = 1_000_000
    I0 = 20

    # Disease parameters
    beta = 0.7
    gamma = 0.3
    mu = 0.15

    t_max = 160

    times, states = simulate_sisd_ode(
        N=N,
        I0=I0,
        beta=beta,
        gamma=gamma,
        mu=mu,
        t_max=t_max
    )

    plot_sisd_ode(
        times,
        states,
        title="Deterministic SISD model"
    )

    S_final = states[-1, 0]
    I_final = states[-1, 1]
    D_final = states[-1, 2]

    print("Final values")
    print("------------")
    print(f"Susceptible: {S_final:.2f}")
    print(f"Infected:    {I_final:.2f}")
    print(f"Dead:        {D_final:.2f}")


if __name__ == "__main__":
    main()