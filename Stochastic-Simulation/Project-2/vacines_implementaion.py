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

    # Only susceptible and infected individuals are alive
    N_alive = S + I

    # Avoid division by zero
    if N_alive <= 0:
        infection_rate = 0
    else:
        infection_rate = beta * S * I / N_alive

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


#################### -------------------- SISD Vaccination Model -------------------- ####################

def gillespie_vacc(N, I0, beta, gamma, mu, nu, eff, rng, t_max=60, max_events=400000):
    """
    Simulate a stochastic SISD model with vaccination using the Gillespie algorithm.

    Compartments
    ------------
    S : Susceptible individuals
    V : Vaccinated individuals
    I : Infected individuals
    D : Dead individuals

    Events
    ------
    Infection of susceptible: S -> I
    Infection of vaccinated:  V -> I
    Vaccination:              S -> V
    Recovery:                 I -> S
    Death:                    I -> D

    Parameters
    ----------
    nu : float
        Vaccination rate.
    eff : float
        Vaccine effectiveness.
        eff = 0 means no protection.
        eff = 1 means full protection.
    """
    S = N - I0
    V = 0
    I = I0
    D = 0

    t = 0.0
    ev = 0
    peak = I

    while I > 0 and t < t_max and ev < max_events:
        N_alive = S + V + I

        if N_alive > 0:
            infS = beta * S * I / N_alive
            infV = beta * (1 - eff) * V * I / N_alive
        else:
            infS = 0.0
            infV = 0.0

        vac = nu * S
        rec = gamma * I
        dth = mu * I

        total_rate = infS + infV + vac + rec + dth

        if total_rate == 0:
            break

        # Time to next event
        t += rng.exponential(1 / total_rate)

        # Choose which event happens
        u = rng.random() * total_rate

        if u < infS:
            S -= 1
            I += 1

        elif u < infS + infV:
            V -= 1
            I += 1

        elif u < infS + infV + vac:
            S -= 1
            V += 1

        elif u < infS + infV + vac + rec:
            I -= 1
            S += 1

        else:
            I -= 1
            D += 1

        peak = max(peak, I)
        ev += 1

    return peak, D


def outbreak_size(N, I0, beta, gamma, mu, nu, eff, rng, n_runs=400):
    """
    Run the vaccination model many times and return average peak infected
    and average total deaths.
    """
    results = [
        gillespie_vacc(N, I0, beta, gamma, mu, nu, eff, rng)
        for _ in range(n_runs)
    ]

    peaks = [r[0] for r in results]
    deaths = [r[1] for r in results]

    return np.mean(peaks), np.mean(deaths)


def plot_vaccine_experiments(N, I0, beta, gamma, mu):
    """
    Plot how vaccine effectiveness and vaccination rate affect the outbreak.
    """
    rng = np.random.default_rng(42)

    # Experiment 1: vary vaccine effectiveness
    nu = 0.6
    effs = [0.0, 0.2, 0.4, 0.6, 0.8, 1.0]

    pk_e, d_e = zip(*[
        outbreak_size(N, I0, beta, gamma, mu, nu, eff, rng)
        for eff in effs
    ])

    # Experiment 2: vary vaccination rate
    eff = 0.9
    nus = [0.0, 0.01, 0.05, 0.08, 0.1, 0.15, 0.2, 0.3, 0.5]

    pk_n, d_n = zip(*[
        outbreak_size(N, I0, beta, gamma, mu, nu_value, eff, rng)
        for nu_value in nus
    ])

    fig, ax = plt.subplots(1, 2, figsize=(12, 4))

    ax[0].plot(effs, pk_e, "o-", label="Peak infected")
    ax[0].plot(effs, d_e, "s-", label="Total deaths")
    ax[0].set_xlabel("Vaccine effectiveness")
    ax[0].set_ylabel("Count")
    ax[0].set_title(f"Effectiveness vs outbreak, nu={nu}")
    ax[0].legend()
    ax[0].grid(True)

    ax[1].plot(nus, pk_n, "o-", label="Peak infected")
    ax[1].plot(nus, d_n, "s-", label="Total deaths")
    ax[1].set_xlabel("Vaccination rate nu")
    ax[1].set_ylabel("Count")
    ax[1].set_title(f"Vaccination speed vs outbreak, eff={eff}")
    ax[1].legend()
    ax[1].grid(True)

    plt.tight_layout()
    plt.show()


def run_death_only(N, I0, beta, gamma, mu, nu, eff, rng, t_max=200, max_events=400000):
    """
    Run one stochastic vaccination simulation and return only the final deaths.
    """
    S = N - I0
    V = 0
    I = I0
    D = 0

    t = 0.0
    ev = 0

    while I > 0 and t < t_max and ev < max_events:
        N_alive = S + V + I

        if N_alive > 0:
            infS = beta * S * I / N_alive
            infV = beta * (1 - eff) * V * I / N_alive
        else:
            infS = 0.0
            infV = 0.0

        vac = nu * S
        rec = gamma * I
        dth = mu * I

        total_rate = infS + infV + vac + rec + dth

        if total_rate == 0:
            break

        t += rng.exponential(1 / total_rate)
        u = rng.random() * total_rate

        if u < infS:
            S -= 1
            I += 1

        elif u < infS + infV:
            V -= 1
            I += 1

        elif u < infS + infV + vac:
            S -= 1
            V += 1

        elif u < infS + infV + vac + rec:
            I -= 1
            S += 1

        else:
            I -= 1
            D += 1

        ev += 1

    return D


def print_vaccine_ci_table(N, I0, beta, gamma, mu):
    """
    Print mean deaths, standard deviation and 95% confidence intervals
    for different vaccine effectiveness values.
    """
    rng = np.random.default_rng(7)

    nu = 0.6
    n_runs = 500

    print()
    print("Vaccination effectiveness experiment")
    print("------------------------------------")
    print(f"{'eff':>5}{'mean':>9}{'std':>9}{'95% CI':>12}")

    for eff in [0.0, 0.2, 0.4, 0.6, 0.8]:
        deaths = np.array([
            run_death_only(N, I0, beta, gamma, mu, nu, eff, rng)
            for _ in range(n_runs)
        ])

        mean = deaths.mean()
        std = deaths.std(ddof=1)
        half_width = 1.96 * std / np.sqrt(n_runs)

        print(f"{eff:>5.1f}{mean:>9.1f}{std:>9.1f}  ±{half_width:>8.2f}")


#################### -------------------- Main Program -------------------- ####################

def main():
    # Population settings for deterministic ODE model
    N_ode = 1_000_000
    I0_ode = 20

    # Disease parameters for deterministic ODE model
    beta_ode = 0.7
    gamma_ode = 0.3
    mu_ode = 0.15

    t_max = 160

    times, states = simulate_sisd_ode(
        N=N_ode,
        I0=I0_ode,
        beta=beta_ode,
        gamma=gamma_ode,
        mu=mu_ode,
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
    total_final = S_final + I_final + D_final

    print("Final values")
    print("------------")
    print(f"Susceptible: {S_final:.2f}")
    print(f"Infected:    {I_final:.2f}")
    print(f"Dead:        {D_final:.2f}")
    print(f"Total:       {total_final:.2f}")

    # Population settings for stochastic vaccination model
    N_vacc = 1000
    I0_vacc = 20

    # Disease parameters for stochastic vaccination model
    beta_vacc = 0.8
    gamma_vacc = 0.3
    mu_vacc = 0.15

    plot_vaccine_experiments(
        N=N_vacc,
        I0=I0_vacc,
        beta=beta_vacc,
        gamma=gamma_vacc,
        mu=mu_vacc
    )

    print_vaccine_ci_table(
        N=N_vacc,
        I0=I0_vacc,
        beta=beta_vacc,
        gamma=gamma_vacc,
        mu=mu_vacc
    )


if __name__ == "__main__":
    main()