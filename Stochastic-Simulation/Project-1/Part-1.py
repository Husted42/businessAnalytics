import numpy as np
import matplotlib.pyplot as plt
from scipy.stats import chisquare



P_0 = [1, 0, 0, 0, 0]
np.random.seed(42)

#################### -------------------- Task 1 -------------------- ####################
# Transition probability matrix
P = np.array([
    [0.9915, 0.0050, 0.0025, 0.0000, 0.0010],
    [0.0000, 0.9860, 0.0050, 0.0040, 0.0050],
    [0.0000, 0.0000, 0.9920, 0.0030, 0.0050],
    [0.0000, 0.0000, 0.0000, 0.9910, 0.0090],
    [0.0000, 0.0000, 0.0000, 0.0000, 1.0000]
])


def simulate_woman(P):

    current_state = 0
    lifetime = 0
    visited_states = [current_state]


    while current_state != 4:
        current_state = np.random.choice(
            len(P),
            p=P[current_state]
        )

        visited_states.append(current_state)
        lifetime += 1

    return lifetime, visited_states


def plot_lines(stages, n_womens, t, savepath="Assets/P1_stage_dist"):
    # Find the longest simulated lifetime
    max_lifetime = t

    # Store the proportion of women in each state at every time step
    stage_distribution = np.zeros((max_lifetime, len(P)))

    for time_step in range(max_lifetime):
        states_at_time_step = []

        for woman_stages in stages:
            if time_step < len(woman_stages):
                # Woman's state at this time step
                states_at_time_step.append(woman_stages[time_step])
            else:
                # The woman has already reached the absorbing death state
                states_at_time_step.append(4)

        stage_counts = np.bincount(
            states_at_time_step,
            minlength=len(P)
        )

        stage_distribution[time_step] = stage_counts / n_womens

    # Plot the distribution over women
    state_names = [
        "State 0",
        "State 1",
        "State 2",
        "State 3",
        "Death"
    ]

    time_steps = np.arange(max_lifetime)

    plt.figure(figsize=(10, 6))

    for state in range(len(P)):
        plt.plot(
            time_steps,
            stage_distribution[:, state],
            label=state_names[state]
        )

    plt.xlabel("Time")
    plt.ylabel("Proportion of women")
    plt.title("Distribution of women across states over time")
    plt.ylim(0, 1)
    plt.grid(alpha=0.3)
    plt.legend()
    plt.tight_layout()
    plt.savefig(savepath)


def plot_dist(stages, n_womens, t, savepath="Assets/P1_hist"):
    # Find the longest simulated lifetime
    max_lifetime = t

    # Store the proportion of women in each state at every time step
    stage_distribution = np.zeros((max_lifetime, len(P)))

    for time_step in range(max_lifetime):
        states_at_time_step = []

        for woman_stages in stages:
            if time_step < len(woman_stages):
                # Woman's state at this time step
                states_at_time_step.append(woman_stages[time_step])
            else:
                # The woman has already reached the absorbing death state
                states_at_time_step.append(4)

        stage_counts = np.bincount(
            states_at_time_step,
            minlength=len(P)
        )

        stage_distribution[time_step] = stage_counts / n_womens

    state_names = [
        "State 0",
        "State 1",
        "State 2",
        "State 3",
        "Death"
    ]

    time_steps = np.arange(max_lifetime)

    plt.figure(figsize=(12, 6))

    bottom = np.zeros(max_lifetime)

    for state in range(len(P)):
        plt.bar(
            time_steps,
            stage_distribution[:, state],
            bottom=bottom,
            label=state_names[state],
            width=1.0
        )

        bottom += stage_distribution[:, state]

    plt.xlabel("Time")
    plt.ylabel("Proportion of women")
    plt.title("Distribution of women across states over time")
    plt.ylim(0, 1)
    plt.xlim(-0.5, max_lifetime - 0.5)
    plt.legend()
    plt.tight_layout()
    plt.savefig(savepath)

    return stage_distribution

def plot_lifetime_hist(lifetimes,savepath = "Assets/P1_hist_lifetimes"):
    plt.hist(lifetimes, bins=20, edgecolor='black')
    plt.title('Distribution of Patient Lifetimes')
    plt.xlabel('Lifetime (months)')
    plt.ylabel('Frequency')
    plt.grid(axis='y', alpha=0.75)
    plt.savefig(savepath)


#################### -------------------- Task 2 -------------------- ####################
def analytical_solution(P, P_0, timesteps):
    """
        distributions[t] contains:
            P_0 @ P^t
    """
    P = np.asarray(P, dtype=float)
    P_0 = np.asarray(P_0, dtype=float)

    n_states = P.shape[0]

    distributions = np.empty((timesteps + 1, n_states))
    distributions[0] = P_0

    for t in range(1, timesteps + 1):
        distributions[t] = distributions[t - 1] @ P

    return distributions


def chi_squared_test(
    observed_distributions,
    expected_distributions,
    n_womens
):
    """
    Perform a chi-squared goodness-of-fit test at each time step.
    """
    results = []

    for time_step in range(len(observed_distributions)):
        observed_counts = (
            observed_distributions[time_step] * n_womens
        )

        expected_counts = (
            expected_distributions[time_step] * n_womens
        )

        valid_states = expected_counts > 0

        observed_counts = observed_counts[valid_states]
        expected_counts = expected_counts[valid_states]

        if len(expected_counts) < 2:
            chi_squared_statistic = np.nan
            p_value = np.nan
        else:
            chi_squared_statistic, p_value = chisquare(
                f_obs=observed_counts,
                f_exp=expected_counts
            )

        results.append({
            "Time": time_step,
            "Chi-squared statistic": chi_squared_statistic,
            "p-value": p_value
        })

    return results
#################### -------------------- Task 3 -------------------- ####################
def lifetime_probability(P, P_0, t):
    """
    Calculate P(T = t).

    P(T = t) = pi @ Ps^(t - 1) @ ps
    """
    pi = np.asarray(P_0[:-1], dtype=float)
    Ps = np.asarray(P[:-1, :-1], dtype=float)
    ps = np.asarray(P[:-1, -1], dtype=float)

    return pi @ np.linalg.matrix_power(Ps, t - 1) @ ps


def expected_lifetime(P, P_0):
    """
    Calculate E[T].

    E[T] = pi @ (I - Ps)^(-1) @ 1
    """
    pi = np.asarray(P_0[:-1], dtype=float)
    Ps = np.asarray(P[:-1, :-1], dtype=float)

    identity_matrix = np.eye(Ps.shape[0])
    ones = np.ones(Ps.shape[0])

    return pi @ np.linalg.inv(identity_matrix - Ps) @ ones

def empirical_lifetime_probability(lifetimes, t):
    lifetimes = np.asarray(lifetimes)
    return np.mean(lifetimes == t)


def empirical_expected_lifetime(lifetimes):
    lifetimes = np.asarray(lifetimes)
    return np.mean(lifetimes)

#################### -------------------- Task 4 -------------------- ####################
#################### -------------------- Task 5 -------------------- ####################

def task_5(P, P_0, n_replications=100, n_women=200, limit=350):

    X = []  # Crude estimates
    Z = []  # Control variable: mean lifetime

    for _ in range(n_replications):

        lifetimes = np.array([
            simulate_woman(P)[0]
            for _ in range(n_women)
        ])

        # Xi: fraction dying within 350 months
        X_i = np.mean(lifetimes <= limit)

        # Zi: mean lifetime of the 200 women
        Z_i = np.mean(lifetimes)

        X.append(X_i)
        Z.append(Z_i)

    X = np.array(X)
    Z = np.array(Z)

    # Known mean of the control variable
    mu_Z = expected_lifetime(P, P_0)

    # Optimal coefficient:
    # c = -Cov(X, Z) / Var(Z)
    c = -np.cov(X, Z, ddof=1)[0, 1] / np.var(Z, ddof=1)

    # Yi = Xi + c(Zi - mu_Z)
    Y = X + c * (Z - mu_Z)

    variance_X = np.var(X, ddof=1)
    variance_Y = np.var(Y, ddof=1)

    variance_reduction = 100 * (
        1 - variance_Y / variance_X
    )

    print("Crude estimate:", np.mean(X))
    print("Control variate estimate:", np.mean(Y))
    print("c:", c)
    print("Crude variance:", variance_X)
    print("Control variate variance:", variance_Y)
    print(f"Variance reduction: {variance_reduction:.2f}%")

def main():
    #################### -------------------- Task 1 -------------------- ####################
    n_womens = 1000
    t = 120

    # Simulate the women
    lifetimes, stages = [], []
    for i in range(n_womens):
        lifetime, visited_states = simulate_woman(P, )
        lifetimes.append(lifetime)
        stages.append(visited_states)

    plot_lifetime_hist(lifetimes)

    # Count the number of occurances in each stage for all women
    lst_stage_distribution = []
    for i in range(n_womens):
        stage_counts = np.bincount(stages[i])

    plot_lines(stages, n_womens, t)
    lst_stage_distribution = plot_dist(stages, n_womens, t)
    timesteps = len(lst_stage_distribution)

    #################### -------------------- Task 2 -------------------- ####################
    # Caluclate the expected distributions
    lst_stage_distribution_expected = analytical_solution(P, P_0, timesteps)
    lst_stage_distribution = np.insert(
        lst_stage_distribution,
        0,
        P_0,
        axis=0
    )
    print(
        "\nPart 2 : Shape of distribution",
        lst_stage_distribution.shape,
        lst_stage_distribution_expected.shape,
        type(lst_stage_distribution)
    )

    chi_squared_results = chi_squared_test(
        lst_stage_distribution,
        lst_stage_distribution_expected,
        n_womens
    )
    print(chi_squared_results[120])

    #################### -------------------- Task 3 -------------------- ####################
    print("\n\nTask 3 : ")
    probability_at_t = lifetime_probability(P, P_0, t)
    mean_lifetime = expected_lifetime(P, P_0)

    print(f"P(T = {t}) = {probability_at_t}")
    print(f"E[T] = {mean_lifetime}")

    empirical_probability_at_t = empirical_lifetime_probability(lifetimes,t)
    empirical_mean_lifetime = empirical_expected_lifetime(lifetimes)

    print(f"Empirical P(T = {t}) = {empirical_probability_at_t}")
    print(f"Empirical E[T] = {empirical_mean_lifetime}")

    #################### -------------------- Task 4 -------------------- ####################
    print("\n\nTask 4 : ")
    total_women = 0
    lst_accepted_lifetimes =  []
    lst_accepted_states = []
    while len(lst_accepted_lifetimes) < 100: # Todo : Set this to 1000 for report
        total_women += 1
        lifetime, visited_states = simulate_woman(P)
        if lifetime < 12:
            continue
        if (visited_states[11] == 1 or visited_states[11] == 2):
            lst_accepted_lifetimes.append(lifetime)
            lst_accepted_states.append(visited_states)
        else: continue
    
    print("Accepted: ", len(lst_accepted_lifetimes), " Total : ", total_women, " Percentage: ",  len(lst_accepted_lifetimes) / total_women)
    print("Mean lifetime of accepted: ", np.mean(lst_accepted_lifetimes))

    #################### -------------------- Task 5 -------------------- ####################
    print("\nTask 5:")
    task_5(P, P_0)

if __name__ == "__main__":
    main()