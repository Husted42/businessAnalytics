import numpy as np
import matplotlib.pyplot as plt
import os


#################### -------------------- Grouped SISD Simulation -------------------- ####################

def simulate_grouped_sisd(groups, t_max=np.inf):
    """
    Simulate a stochastic SISD model with multiple population groups.

    Groups
    ------
    Humans : Standard rates
    Orcs   : Higher infection rate, lower death rate
    Elves  : Higher recovery rate
    NPCs   : Fixed infected reservoir. They cannot recover or die.

    Each non-NPC group has compartments:
    S : Susceptible
    I : Infected
    D : Dead
    """

    group_names = list(groups.keys())

    S = np.array([groups[g]["S0"] for g in group_names], dtype=int)
    I = np.array([groups[g]["I0"] for g in group_names], dtype=int)
    D = np.array([0 for _ in group_names], dtype=int)

    beta = np.array([groups[g]["beta"] for g in group_names], dtype=float)
    gamma = np.array([groups[g]["gamma"] for g in group_names], dtype=float)
    mu = np.array([groups[g]["mu"] for g in group_names], dtype=float)
    infectiousness = np.array([groups[g]["infectiousness"] for g in group_names], dtype=float)
    active = np.array([groups[g]["active"] for g in group_names], dtype=bool)

    t = 0.0

    times = [t]
    states = [(S.copy(), I.copy(), D.copy())]
    events = []

    while np.sum(I[active]) > 0 and t < t_max:
        N_alive = np.sum(S[active] + I[active])

        if N_alive <= 0:
            break

        # Total infection pressure from all infected groups.
        # NPCs contribute here even though they cannot change state.
        infection_pressure = np.sum(infectiousness * I)

        infection_rates = np.zeros(len(group_names))
        recovery_rates = np.zeros(len(group_names))
        death_rates = np.zeros(len(group_names))

        for k, g in enumerate(group_names):
            if active[k]:
                infection_rates[k] = beta[k] * S[k] * infection_pressure / N_alive
                recovery_rates[k] = gamma[k] * I[k]
                death_rates[k] = mu[k] * I[k]

        total_rate = (
            np.sum(infection_rates)
            + np.sum(recovery_rates)
            + np.sum(death_rates)
        )

        if total_rate <= 0:
            break

        # Time until next event
        dt = np.random.exponential(1 / total_rate)
        t += dt

        if t > t_max:
            break

        # Choose event
        rates = []
        labels = []

        for k, g in enumerate(group_names):
            if infection_rates[k] > 0:
                rates.append(infection_rates[k])
                labels.append(("infection", k))

            if recovery_rates[k] > 0:
                rates.append(recovery_rates[k])
                labels.append(("recovery", k))

            if death_rates[k] > 0:
                rates.append(death_rates[k])
                labels.append(("death", k))

        rates = np.array(rates)
        probabilities = rates / np.sum(rates)

        event_index = np.random.choice(len(labels), p=probabilities)
        event_type, k = labels[event_index]

        if event_type == "infection":
            S[k] -= 1
            I[k] += 1

        elif event_type == "recovery":
            I[k] -= 1
            S[k] += 1

        elif event_type == "death":
            I[k] -= 1
            D[k] += 1

        events.append((event_type, group_names[k]))

        times.append(t)
        states.append((S.copy(), I.copy(), D.copy()))

    return np.array(times), states, events, group_names


def unpack_states(states):
    """
    Convert list of state tuples into arrays:
    S, I, D each have shape: time x groups
    """
    S = np.array([state[0] for state in states])
    I = np.array([state[1] for state in states])
    D = np.array([state[2] for state in states])

    return S, I, D


#################### -------------------- Plotting -------------------- ####################

def plot_grouped_sisd(times, states, group_names, savepath="assets_test/grouped_sisd.png"):
    """
    Plot total S, I and D over time.
    """
    os.makedirs(os.path.dirname(savepath), exist_ok=True)

    S, I, D = unpack_states(states)

    total_S = np.sum(S, axis=1)
    total_I = np.sum(I, axis=1)
    total_D = np.sum(D, axis=1)

    plt.figure(figsize=(10, 5))
    plt.plot(times, total_S, label="Total Susceptible")
    plt.plot(times, total_I, label="Total Infected")
    plt.plot(times, total_D, label="Total Dead")

    plt.xlabel("Time")
    plt.ylabel("Number of individuals")
    plt.title("Grouped SISD simulation")
    plt.legend()
    plt.grid(True)

    plt.tight_layout()
    plt.savefig(savepath)
    plt.show()


def plot_group_infections(times, states, group_names, savepath="assets_test/group_infections.png"):
    """
    Plot infected individuals by group.
    """
    os.makedirs(os.path.dirname(savepath), exist_ok=True)

    S, I, D = unpack_states(states)

    plt.figure(figsize=(10, 5))

    for k, group in enumerate(group_names):
        plt.plot(times, I[:, k], label=f"{group} infected")

    plt.xlabel("Time")
    plt.ylabel("Number of infected individuals")
    plt.title("Infected individuals by group")
    plt.legend()
    plt.grid(True)

    plt.tight_layout()
    plt.savefig(savepath)
    plt.show()


def plot_group_deaths(times, states, group_names, savepath="assets_test/group_deaths.png"):
    """
    Plot dead individuals by group.
    """
    os.makedirs(os.path.dirname(savepath), exist_ok=True)

    S, I, D = unpack_states(states)

    plt.figure(figsize=(10, 5))

    for k, group in enumerate(group_names):
        plt.plot(times, D[:, k], label=f"{group} dead")

    plt.xlabel("Time")
    plt.ylabel("Number of dead individuals")
    plt.title("Deaths by group")
    plt.legend()
    plt.grid(True)

    plt.tight_layout()
    plt.savefig(savepath)
    plt.show()


#################### -------------------- Summary -------------------- ####################

def run_many_grouped_simulations(groups, n_simulations, t_max=np.inf):
    """
    Run many grouped SISD simulations and collect summary statistics.
    """
    final_dead = []
    peak_infected = []
    epidemic_durations = []

    for i in range(n_simulations):
        print("Running grouped simulation", i)

        times, states, events, group_names = simulate_grouped_sisd(groups, t_max=t_max)

        S, I, D = unpack_states(states)

        total_I = np.sum(I, axis=1)
        total_D = np.sum(D, axis=1)

        final_dead.append(total_D[-1])
        peak_infected.append(np.max(total_I))
        epidemic_durations.append(times[-1])

    results = {
        "final_dead": np.array(final_dead),
        "peak_infected": np.array(peak_infected),
        "epidemic_durations": np.array(epidemic_durations)
    }

    return results


def print_grouped_summary(results, total_population):
    """
    Print summary statistics.
    """
    final_dead = results["final_dead"]
    peak_infected = results["peak_infected"]
    epidemic_durations = results["epidemic_durations"]

    print("Summary of grouped stochastic SISD simulations")
    print("---------------------------------------------")
    print(f"Average final deaths: {np.mean(final_dead):.2f}")
    print(f"Average final death fraction: {np.mean(final_dead) / total_population:.4f}")
    print(f"Average peak infected: {np.mean(peak_infected):.2f}")
    print(f"Average epidemic duration: {np.mean(epidemic_durations):.2f}")


#################### -------------------- Main -------------------- ####################

def main():
    np.random.seed(42)

    # Original standard disease parameters
    beta = 0.8
    gamma = 0.3
    mu = 0.15

    groups = {
        "Humans": {
            "S0": 60000,
            "I0": 20,
            "beta": beta,
            "gamma": gamma,
            "mu": mu,
            "infectiousness": 1.0,
            "active": True
        },

        "Orcs": {
            "S0": 25000,
            "I0": 0,
            "beta": beta * 1.4,      # higher infection rate
            "gamma": gamma,
            "mu": mu * 0.5,         # lower death rate
            "infectiousness": 1.2,
            "active": True
        },

        "Elves": {
            "S0": 15000,
            "I0": 0,
            "beta": beta,
            "gamma": gamma * 1.7,   # higher recovery rate
            "mu": mu,
            "infectiousness": 1.0,
            "active": True
        },

        "NPCs": {
            "S0": 0,
            "I0": 100,
            "beta": 0.0,
            "gamma": 0.0,
            "mu": 0.0,
            "infectiousness": 1.5,  # they infect others strongly
            "active": False         # cannot recover, die, or be infected
        }
    }

    total_population = sum(groups[g]["S0"] + groups[g]["I0"] for g in groups)

    # times, states, events, group_names = simulate_grouped_sisd(
    #     groups=groups,
    #     t_max=160
    # )

    # plot_grouped_sisd(times, states, group_names)
    # plot_group_infections(times, states, group_names)
    # plot_group_deaths(times, states, group_names)

    n_simulations = 1

    results = run_many_grouped_simulations(
        groups=groups,
        n_simulations=n_simulations,
        t_max=160
    )

    print_grouped_summary(results, total_population)


if __name__ == "__main__":
    main()