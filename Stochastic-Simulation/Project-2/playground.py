import numpy as np
import matplotlib.pyplot as plt


#################### -------------------- SIR Simulation -------------------- ####################

def simulate_sir(N, I0, beta, gamma, t_max=np.inf):
    """
    Simulate a stochastic SIR epidemic using the Gillespie algorithm.

    Parameters
    ----------
    N : int
        Total population size.
    I0 : int
        Initial number of infected individuals.
    beta : float
        Infection rate parameter.
    gamma : float
        Recovery rate parameter.
    t_max : float
        Maximum simulation time.

    Returns
    -------
    times : np.array
        Times at which events occur.
    states : np.array
        Array with columns [S, I, R].
    """
    S = N - I0
    I = I0
    R = 0
    t = 0.0

    times = [t]
    states = [[S, I, R]]

    while I > 0 and t < t_max:
        # Infection rate: S + I -> I + I
        infection_rate = beta * S * I / N

        # Recovery rate: I -> R
        recovery_rate = gamma * I

        total_rate = infection_rate + recovery_rate

        if total_rate <= 0:
            break

        # Time until next event
        dt = np.random.exponential(1 / total_rate)
        t += dt

        # Decide whether the next event is infection or recovery
        if np.random.rand() < infection_rate / total_rate:
            S -= 1
            I += 1
        else:
            I -= 1
            R += 1

        times.append(t)
        states.append([S, I, R])

    return np.array(times), np.array(states)


#################### -------------------- Plot One Simulation -------------------- ####################

def plot_sir(times, states, title="Stochastic SIR simulation"):
    """
    Plot S, I and R over time.
    """
    S = states[:, 0]
    I = states[:, 1]
    R = states[:, 2]

    plt.figure(figsize=(10, 5))
    plt.plot(times, S, label="Susceptible")
    plt.plot(times, I, label="Infected")
    plt.plot(times, R, label="Recovered")
    plt.xlabel("Time")
    plt.ylabel("Number of individuals")
    plt.title(title)
    plt.legend()
    plt.grid(True)
    plt.savefig("assets_test/sir_1")


#################### -------------------- Run Many Simulations -------------------- ####################

def run_many_simulations(N, I0, beta, gamma, n_simulations, major_outbreak_threshold=0.1):
    """
    Run many stochastic SIR simulations and collect summary statistics.
    """
    final_sizes = []
    peak_infected = []
    epidemic_durations = []
    major_outbreaks = []

    for _ in range(n_simulations):
        times, states = simulate_sir(N, I0, beta, gamma)

        S = states[:, 0]
        I = states[:, 1]
        R = states[:, 2]

        final_size = R[-1]
        peak_I = np.max(I)
        duration = times[-1]

        final_sizes.append(final_size)
        peak_infected.append(peak_I)
        epidemic_durations.append(duration)
        major_outbreaks.append(final_size > major_outbreak_threshold * N)

    results = {
        "final_sizes": np.array(final_sizes),
        "peak_infected": np.array(peak_infected),
        "epidemic_durations": np.array(epidemic_durations),
        "major_outbreaks": np.array(major_outbreaks)
    }

    return results


#################### -------------------- Summary Function -------------------- ####################

def print_summary(results, N):
    """
    Print summary statistics from many simulations.
    """
    final_sizes = results["final_sizes"]
    peak_infected = results["peak_infected"]
    epidemic_durations = results["epidemic_durations"]
    major_outbreaks = results["major_outbreaks"]

    print("Summary of stochastic SIR simulations")
    print("-------------------------------------")
    print(f"Average final epidemic size: {np.mean(final_sizes):.2f}")
    print(f"Average final epidemic fraction: {np.mean(final_sizes) / N:.4f}")
    print(f"Average peak infected: {np.mean(peak_infected):.2f}")
    print(f"Average epidemic duration: {np.mean(epidemic_durations):.2f}")
    print(f"Probability of major outbreak: {np.mean(major_outbreaks):.4f}")
    print(f"Probability of early extinction: {1 - np.mean(major_outbreaks):.4f}")


#################### -------------------- Histogram Functions -------------------- ####################

def plot_final_size_histogram(results, N):
    """
    Plot histogram of final epidemic sizes.
    """
    final_sizes = results["final_sizes"]

    plt.figure(figsize=(8, 5))
    plt.hist(final_sizes, bins=30, edgecolor="black")
    plt.xlabel("Final epidemic size")
    plt.ylabel("Frequency")
    plt.title("Distribution of final epidemic sizes")
    plt.grid(True)
    plt.savefig("assets_test/final_size")


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
    plt.savefig("assets_test/peak_infections")


def plot_many_sir_trajectories(N, I0, beta, gamma, n_simulations, t_max=160):
    """
    Plot S, I, and R trajectories from many stochastic SIR simulations.
    """
    plt.figure(figsize=(10, 6))

    for _ in range(n_simulations):
        times, states = simulate_sir(
            N=N,
            I0=I0,
            beta=beta,
            gamma=gamma,
            t_max=t_max
        )

        S = states[:, 0]
        I = states[:, 1]
        R = states[:, 2]

        plt.plot(times, S, color="blue", alpha=0.15, linewidth=1)
        plt.plot(times, I, color="red", alpha=0.15, linewidth=1)
        plt.plot(times, R, color="green", alpha=0.15, linewidth=1)

    plt.xlabel("Time")
    plt.ylabel("Number of individuals")
    plt.title("Stochastic SIR trajectories")
    plt.grid(True, linestyle="--", alpha=0.5)

    # Dummy lines for legend
    plt.plot([], [], color="blue", label="Susceptible")
    plt.plot([], [], color="red", label="Infected")
    plt.plot([], [], color="green", label="Recovered")
    plt.legend()

    plt.tight_layout()
    plt.savefig("assets_test/sir_trajectories.png")

#################### -------------------- Main -------------------- ####################
def main():
    np.random.seed(42)

    # Population settings
    N = 1000
    I0 = 20

    # Disease parameters
    gamma = 1 / 7       # Average infectious period is 7 days
    beta = 0.8 
    # One simulation
    times, states = simulate_sir(N, I0, beta, gamma)
    plot_sir(times, states, title=f"Stochastic SIR simulation")

    # Many simulations
    n_simulations = 1000
    results = run_many_simulations(
        N=N,
        I0=I0,
        beta=beta,
        gamma=gamma,
        n_simulations=n_simulations
    )

    print_summary(results, N)

    plot_final_size_histogram(results, N)
    plot_peak_infected_histogram(results)

    # Plot stochastic trajectories
    plot_many_sir_trajectories(
        N=N,
        I0=I0,
        beta=beta,
        gamma=gamma,
        n_simulations=100,
        t_max=160
    )

if __name__ == "__main__":
    main()