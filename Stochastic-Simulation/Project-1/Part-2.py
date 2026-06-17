import numpy as np
import matplotlib.pyplot as plt
from scipy.stats import t, chi2, kstest
from scipy.linalg import expm

#################### -------------------- Task 7 -------------------- ####################
Q = np.array([
    [-0.0085, 0.0050, 0.0025, 0.0000, 0.0010],
    [ 0.0000, -0.0140, 0.0050, 0.0040, 0.0050],
    [ 0.0000, 0.0000, -0.0080, 0.0030, 0.0050],
    [ 0.0000, 0.0000, 0.0000, -0.0090, 0.0090],
    [ 0.0000, 0.0000, 0.0000, 0.0000, 0.0000]
])



def theoretical_ctmc(Q, t):
    Q_s = Q[:-1, :-1]
    p_0 = np.array([1, 0, 0, 0])
    ones = np.ones(4)

    # F_T(t) = 1 - p_0 exp(Q_s t) 1
    # Follows form the project 
    F_T = 1 - p_0 @ expm(Q_s * t) @ ones

    # E[T] = p_0 (-Q_s)^(-1) 1
    # This is given on https://en.wikipedia.org/wiki/Phase-type_distribution
    E_T = p_0 @ np.linalg.inv(-Q_s) @ ones

    return F_T, E_T

'''
    sojourn_time : exponentially distributed with rate −qii
'''
def simulate_woman_ctmc(Q, observation_time=30.5):
    state = 0
    time = 0
    state_at_observation = None

    while state != 4:
        # Sojourn time in state i
        rate = -Q[state, state]
        sojourn_time = np.random.exponential(scale=1 / rate)

        # State at 30.5 months
        if time <= observation_time < time + sojourn_time:
            state_at_observation = state

        time += sojourn_time

        # Jump probabilities: P(i -> j) = -q_ij / q_ii
        probabilities = np.zeros(len(Q))

        for next_state in range(len(Q)):
            if next_state != state:
                probabilities[next_state] = (
                    Q[state, next_state]
                    / -Q[state, state]
                )

        state = np.random.choice(len(Q), p=probabilities)

    if state_at_observation is None:
        state_at_observation = 4

    return time, state_at_observation

def summaries(lifetimes, observation_states, n, savepath = "Assets/P2_task7"):

    mean = np.mean(lifetimes)
    sd = np.std(lifetimes, ddof=1)

    # 95% confidence interval for the mean
    mean_margin = t.ppf(0.975, n - 1) * sd / np.sqrt(n)
    mean_ci = (mean - mean_margin, mean + mean_margin)

    # 95% confidence interval for the standard deviation
    sd_ci = (
        np.sqrt((n - 1) * sd**2 / chi2.ppf(0.975, n - 1)),
        np.sqrt((n - 1) * sd**2 / chi2.ppf(0.025, n - 1))
    )

    # Distant recurrence: states 3 or 4
    distant_count = 0

    for state in observation_states:
        if state == 2 or state == 3:
            distant_count += 1

    distant_fraction = distant_count / len(observation_states)

    print(f"Mean lifetime: {mean:.2f}")
    print(f"95% CI for mean: [{mean_ci[0]:.2f}, {mean_ci[1]:.2f}]")
    print(f"Standard deviation: {sd:.2f}")
    print(f"95% CI for SD: [{sd_ci[0]:.2f}, {sd_ci[1]:.2f}]")
    print(f"Distant recurrence after 30.5 months: {distant_fraction:.4f}")

    plt.hist(lifetimes, bins=30, edgecolor="black")
    plt.xlabel("Lifetime in months")
    plt.ylabel("Frequency")
    plt.title("Lifetime distribution after surgery")
    plt.tight_layout()
    plt.savefig(savepath)
    plt.close()

def plot_state_distribution(observation_states, t, savepath="Assets/P2_task7_state_distribution"):
    state_names = [
        "No recurrence",
        "Local recurrence",
        "Distant recurrence",
        "Local and distant",
        "Death"
    ]

    state_counts = [0, 0, 0, 0, 0]

    for state in observation_states:
        if state == 0:
            state_counts[0] += 1
        elif state == 1:
            state_counts[1] += 1
        elif state == 2:
            state_counts[2] += 1
        elif state == 3:
            state_counts[3] += 1
        elif state == 4:
            state_counts[4] += 1
    print(state_counts)

    plt.bar(state_names, state_counts)
    plt.xlabel("State")
    plt.ylabel("Number of women")
    plt.title(f"Distribution of states after {t} months")
    plt.xticks(rotation=20)
    plt.tight_layout()
    plt.savefig(savepath)
    plt.close()

#################### -------------------- Task 8 -------------------- ####################
def ctmc_cdf(times, Q):
    Q_s = Q[:-1, :-1]
    p_0 = np.array([1, 0, 0, 0])
    ones = np.ones(4)

    times = np.atleast_1d(times)

    probabilities = [
        1 - p_0 @ expm(Q_s * time) @ ones
        for time in times
    ]

    return np.array(probabilities)

def test_lifetime_distribution(lifetimes, Q):
    statistic, p_value = kstest(
        lifetimes,
        lambda times: ctmc_cdf(times, Q)
    )

    print(f"KS statistic: {statistic:.4f}")
    print(f"p-value: {p_value:.4f}")

    if p_value < 0.05:
        print("Reject: simulated lifetimes do not follow the theoretical distribution.")
    else:
        print("Do not reject: simulated lifetimes are consistent with the theoretical distribution.")

#################### -------------------- Task 9 -------------------- ####################
def survival_function(lifetimes, savepath="Assets/P2_task9_survival"):
    lifetimes = np.sort(np.array(lifetimes))
    n = len(lifetimes)

    survival = []

    for t in lifetimes:
        deaths = np.sum(lifetimes <= t)
        S_t = (n - deaths) / n
        survival.append(S_t)

    plt.step(lifetimes, survival, where="post")
    plt.xlabel("Time in months")
    plt.ylabel("Survival probability")
    plt.title("Kaplan-Meier survival function")
    plt.ylim(0, 1)
    plt.tight_layout()
    plt.savefig(savepath)
    plt.close()
    
def main():
    np.random.seed(42)
    #################### -------------------- Task 7 -------------------- ####################
    print("Task 7 - Simulation")
    deaths = 0
    n = 10000
    lifetimes = []
    observation_states = []
    oberservation_time = 30.5
    
    for i in range(n):
        lifetime, state_at_obeservation = simulate_woman_ctmc(Q, oberservation_time)

        if state_at_obeservation == 4: deaths += 1
        lifetimes.append(lifetime)
        observation_states.append(state_at_obeservation)
    
    print(deaths / n)
    print(np.mean(lifetimes))

    summaries(lifetimes, observation_states, n)
    plot_state_distribution(observation_states, oberservation_time)

    #################### -------------------- Task 8 -------------------- ####################
    print("\n\nTask 8 - Theoretical")
    death_probability, mean_lifetime = theoretical_ctmc(Q, 30.5)
    print("death_probability", death_probability,)
    print("mean lifetime", mean_lifetime,)

    test_lifetime_distribution(lifetimes, Q)

    #################### -------------------- Task 9 -------------------- ####################
    survival_function(lifetimes)
    return None


    
if __name__ == "__main__":
    main()