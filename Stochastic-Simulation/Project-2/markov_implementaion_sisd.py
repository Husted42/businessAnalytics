import numpy as np
import matplotlib.pyplot as plt


#################### -------------------- SISD Simulation -------------------- ####################

def simulate_sisd(N, I0, beta, gamma, mu, t_max=np.inf):
    """
    Simulate a stochastic SIS model with death using the Gillespie algorithm.

    Compartments
    ------------
    S : Susceptible players
    I : Infected players
    D : Dead players

    Events
    ------
    Infection: S + I -> I + I
    Recovery: I -> S
    Death:    I -> D

    Parameters
    ----------
    N : int
        Initial total population size.
    I0 : int
        Initial number of infected individuals.
    beta : float
        Infection rate.
    gamma : float
        Recovery rate.
    mu : float
        Death rate.
    t_max : float
        Maximum simulation time.

    Returns
    -------
    times : np.array
        Times at which events occur.
    states : np.array
        Array with columns [S, I, D].
    events : list
        List of event types.
    """
    S = N - I0
    I = I0
    D = 0
    t = 0.0

    times = [t]
    states = [[S, I, D]]
    events = []

    while I > 0 and t < t_max:
        N_alive = S + I

        if N_alive <= 0:
            break

        # Infection: S + I -> I + I
        infection_rate = beta * S * I / N_alive

        # Recovery: I -> S
        recovery_rate = gamma * I

        # Death: I -> D
        death_rate = mu * I

        total_rate = infection_rate + recovery_rate + death_rate

        if total_rate <= 0:
            break

        # Time until next event
        dt = np.random.exponential(1 / total_rate)
        t += dt

        if t > t_max:
            break

        # Decide event type
        u = np.random.rand()

        # print("u", u)
        # print("dt", dt)
        # print("infection", infection_rate / total_rate)
        # print("recovery", (recovery_rate) / total_rate)
        # print("Death", 1 - (infection_rate + recovery_rate) / total_rate)
        

        if u < infection_rate / total_rate:
            S -= 1
            I += 1
            events.append("infection")

        elif u < (infection_rate + recovery_rate) / total_rate:
            I -= 1
            S += 1
            events.append("recovery")

        else:
            I -= 1
            D += 1
            events.append("death")

        times.append(t)
        states.append([S, I, D])

    return np.array(times), np.array(states), events


def plot_sisd(times, states, title="Stochastic SISD simulation"):
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
    plt.savefig("assets_test/sisd_1.png")

def run_many_simulations(N, I0, beta, gamma, mu, n_simulations, major_outbreak_threshold=0.1):
    """
    Run many stochastic SISD simulations and collect summary statistics.
    """
    final_dead = []
    peak_infected = []
    epidemic_durations = []
    major_outbreaks = []

    for _ in range(n_simulations):
        print("Running simulation for summary", _)
        times, states, events = simulate_sisd(N, I0, beta, gamma, mu)

        S = states[:, 0]
        I = states[:, 1]
        D = states[:, 2]

        final_D = D[-1]
        peak_I = np.max(I)
        duration = times[-1]

        final_dead.append(final_D)
        peak_infected.append(peak_I)
        epidemic_durations.append(duration)

        major_outbreaks.append(peak_I > major_outbreak_threshold * N)

    results = {
        "final_dead": np.array(final_dead),
        "peak_infected": np.array(peak_infected),
        "epidemic_durations": np.array(epidemic_durations),
        "major_outbreaks": np.array(major_outbreaks)
    }

    return results

def print_summary(results, N):
    """
    Print summary statistics from many SISD simulations.
    """
    final_dead = results["final_dead"]
    peak_infected = results["peak_infected"]
    epidemic_durations = results["epidemic_durations"]
    major_outbreaks = results["major_outbreaks"]

    print("Summary of stochastic SISD simulations")
    print("--------------------------------------")
    print(f"Average final deaths: {np.mean(final_dead):.2f}")
    print(f"Average final death fraction: {np.mean(final_dead) / N:.4f}")
    print(f"Average peak infected: {np.mean(peak_infected):.2f}")
    print(f"Average epidemic duration: {np.mean(epidemic_durations):.2f}")
    print(f"Probability of major outbreak: {np.mean(major_outbreaks):.4f}")
    print(f"Probability of early extinction: {1 - np.mean(major_outbreaks):.4f}") 

def plot_final_death_histogram(results):
    """
    Plot histogram of final number of deaths.
    """
    final_dead = results["final_dead"]

    plt.figure(figsize=(8, 5))
    plt.hist(final_dead, bins=30, edgecolor="black")

    plt.xlabel("Final number of dead individuals")
    plt.ylabel("Frequency")
    plt.title("Distribution of final deaths")
    plt.grid(True)

    plt.tight_layout()
    plt.savefig("assets_test/final_deaths.png")


def plot_peak_infected_histogram(results):
    """
    Plot histogram of peak infected individuals.
    """
    peak_infected = results["peak_infected"]

    plt.figure(figsize=(8, 5))
    plt.hist(peak_infected, bins=30, edgecolor="black")

    plt.xlabel("Peak number of infected individuals")
    plt.ylabel("Frequency")
    plt.title("Distribution of peak infected")
    plt.grid(True)

    plt.tight_layout()
    plt.savefig("assets_test/peak_infections.png")

def plot_many_sisd_trajectories(N, I0, beta, gamma, mu, n_simulations, t_max=160):
    """
    Plot S, I and D trajectories from many stochastic SISD simulations.
    """
    plt.figure(figsize=(10, 6))

    # Fixed colors for each compartment
    colors = {
        "S": "tab:blue",
        "I": "tab:orange",
        "D": "tab:green"
    }

    for _ in range(n_simulations):
        print("Runing sim for plot", _)
        times, states, events = simulate_sisd(
            N=N,
            I0=I0,
            beta=beta,
            gamma=gamma,
            mu=mu,
            t_max=t_max
        )

        S = states[:, 0]
        I = states[:, 1]
        D = states[:, 2]

        plt.plot(times, S, color=colors["S"], alpha=0.15, linewidth=1)
        plt.plot(times, I, color=colors["I"], alpha=0.15, linewidth=1)
        plt.plot(times, D, color=colors["D"], alpha=0.15, linewidth=1)

    plt.xlabel("Time")
    plt.ylabel("Number of individuals")
    plt.title("Stochastic SISD trajectories")
    plt.grid(True, linestyle="--", alpha=0.5)

    # Dummy plots for legend with matching colors
    plt.plot([], [], color=colors["S"], label="Susceptible")
    plt.plot([], [], color=colors["I"], label="Infected")
    plt.plot([], [], color=colors["D"], label="Dead")
    plt.legend()

    plt.tight_layout()
    plt.savefig("assets_test/sisd_trajectories.png")
    plt.show()

def main():
    np.random.seed(42)

    # Population settings
    N = 100000
    I0 = 20

    # Disease parameters
    beta = 0.8        # Infection rate
    gamma = 0.3     # Recovery rate
    mu = 1 / 7      # Death rate

    # One simulation
    times, states, events = simulate_sisd(
        N=N,
        I0=I0,
        beta=beta,
        gamma=gamma,
        mu=mu
    )

    plot_sisd(
        times,
        states,
        title="Stochastic SISD simulation"
    )

    # Many simulations
    n_simulations = 100

    results = run_many_simulations(
        N=N,
        I0=I0,
        beta=beta,
        gamma=gamma,
        mu=mu,
        n_simulations=n_simulations
    )

    print_summary(results, N)

    plot_final_death_histogram(results)
    plot_peak_infected_histogram(results)

    plot_many_sisd_trajectories(
        N=N,
        I0=I0,
        beta=beta,
        gamma=gamma,
        mu=mu,
        n_simulations=100,
        t_max=160
    )


if __name__ == "__main__":
    main()