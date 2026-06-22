import numpy as np
import matplotlib.pyplot as plt
from scipy.integrate import solve_ivp


#################### -------------------- SIRD ODE Model -------------------- ####################

def sird_ode(t, y, beta, gamma, mu, N):
    """
    Deterministic SIRD model using differential equations.

    Compartments
    ------------
    S : Susceptible individuals
    I : Infected individuals
    R : Recovered individuals
    D : Dead / removed individuals
    """
    S, I, R, D = y

    infection_rate = beta * I * S / N
    recovery_rate = gamma * I
    death_rate = mu * I

    dSdt = -infection_rate
    dIdt = infection_rate - recovery_rate - death_rate
    dRdt = recovery_rate
    dDdt = death_rate

    return [dSdt, dIdt, dRdt, dDdt]


def simulate_sird_ode(N, I0, beta, gamma, mu, t_max=160):
    """
    Solve the deterministic SIRD model.
    """
    S0 = N - I0
    R0 = 0
    D0 = 0

    y0 = [S0, I0, R0, D0]

    t_eval = np.linspace(0, t_max, 1000)

    solution = solve_ivp(
        fun=sird_ode,
        t_span=(0, t_max),
        y0=y0,
        args=(beta, gamma, mu, N),
        t_eval=t_eval
    )

    return solution.t, solution.y.T


def plot_sird_ode(times, states, title="Deterministic SIRD model"):
    """
    Plot S, I, R and D over time.
    """
    S = states[:, 0]
    I = states[:, 1]
    R = states[:, 2]
    D = states[:, 3]

    plt.figure(figsize=(10, 5))
    plt.plot(times, S, label="Susceptible")
    plt.plot(times, I, label="Infected")
    plt.plot(times, R, label="Recovered")
    plt.plot(times, D, label="Dead / Removed")

    plt.xlabel("Time")
    plt.ylabel("Number of individuals")
    plt.title(title)
    plt.legend()
    plt.grid(True)

    plt.tight_layout()
    plt.savefig("assets_test/sird_ode.png")
    plt.show()


def main():
    # Population settings
    N = 1000000
    I0 = 20

    # Disease parameters
    beta = 0.7
    gamma = 0.3
    mu = 0.15

    t_max = 160

    times, states = simulate_sird_ode(
        N=N,
        I0=I0,
        beta=beta,
        gamma=gamma,
        mu=mu,
        t_max=t_max
    )

    plot_sird_ode(
        times,
        states,
        title="Deterministic SIRD model"
    )

    S_final = states[-1, 0]
    I_final = states[-1, 1]
    R_final = states[-1, 2]
    D_final = states[-1, 3]

    print("Final values")
    print("------------")
    print(f"Susceptible: {S_final:.2f}")
    print(f"Infected:    {I_final:.2f}")
    print(f"Recovered:   {R_final:.2f}")
    print(f"Dead:        {D_final:.2f}")

    print()
    print("Check total population")
    print("----------------------")
    print(f"S + I + R + D = {S_final + I_final + R_final + D_final:.2f}")


if __name__ == "__main__":
    main()