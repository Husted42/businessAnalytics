import numpy as np
import matplotlib.pyplot as plt
from scipy.stats import t, chi2, kstest
from scipy.linalg import expm
import sys
from contextlib import redirect_stdout


class Tee:
    def __init__(self, *files):
        self.files = files

    def write(self, text):
        for file in self.files:
            file.write(text)

    def flush(self):
        for file in self.files:
            file.flush()


#################### -------------------- Task 7 -------------------- ####################
Q = np.array([
    [-0.0085, 0.0050, 0.0025, 0.0000, 0.0010],
    [ 0.0000, -0.0140, 0.0050, 0.0040, 0.0050],
    [ 0.0000, 0.0000, -0.0080, 0.0030, 0.0050],
    [ 0.0000, 0.0000, 0.0000, -0.0090, 0.0090],
    [ 0.0000, 0.0000, 0.0000, 0.0000, 0.0000]
])

Q_treat = np.array([
    [-0.00475, 0.00250, 0.00125, 0.00000, 0.00100],
    [ 0.00000, -0.00700, 0.00000, 0.00200, 0.00500],
    [ 0.00000, 0.00000, -0.00800, 0.00300, 0.00500],
    [ 0.00000, 0.00000, 0.00000, -0.00900, 0.00900],
    [ 0.00000, 0.00000, 0.00000, 0.00000, 0.00000]
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

    print("\nSummary statistics")
    print("------------------")
    print(f"Mean lifetime:              {mean:.2f} months")
    print(f"95% CI for mean:            [{mean_ci[0]:.2f}, {mean_ci[1]:.2f}] months")
    print(f"Standard deviation:         {sd:.2f} months")
    print(f"95% CI for SD:              [{sd_ci[0]:.2f}, {sd_ci[1]:.2f}] months")
    print(f"Distant recurrence rate:    {distant_fraction:.2%}")

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

    print(f"\nState distribution after {t} months:")
    for name, count in zip(state_names, state_counts):
        print(f"  {name:22}: {count}")

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

    print("\nKolmogorov-Smirnov test")
    print("------------------------")
    print(f"KS statistic: {statistic:.4f}")
    print(f"p-value:       {p_value:.4f}")

    if p_value < 0.05:
        print("Conclusion: reject the null hypothesis. Simulated lifetimes do not match the theoretical distribution.")
    else:
        print("Conclusion: do not reject the null hypothesis. Simulated lifetimes are consistent with the theoretical distribution.")

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
    
def survival_function_compare(baseline, treatment, savepath="Assets/P2_task9_survival_compare"):
    baseline = np.sort(np.array(baseline))
    treatment = np.sort(np.array(treatment))
    n = len(treatment)

    survival_baseline, survival_treatment = [], []

    if len(baseline) != len(treatment): raise ValueError ("Not the same length")
    
    for t in baseline:
        deaths_baseline = np.sum(baseline <= t)
        deaths_treatment = np.sum(treatment <= t)

        survival_baseline.append((n - deaths_baseline) / n)
        survival_treatment.append((n - deaths_treatment) / n)

    plt.step(baseline, survival_baseline, where="post")
    plt.step(baseline, survival_treatment, where="post")
    plt.xlabel("Time in months")
    plt.ylabel("Survival probability")
    plt.title("Kaplan-Meier survival function")
    plt.ylim(0, 1)
    plt.tight_layout()
    plt.savefig(savepath)
    plt.close()

    return survival_baseline, survival_treatment

def log_rank_test(baseline, treatment):
    baseline = np.asarray(baseline)
    treatment = np.asarray(treatment)

    event_times = np.unique(np.concatenate([baseline, treatment]))

    observed = 0
    expected = 0
    variance = 0

    for time in event_times:
        risk_baseline = np.sum(baseline >= time)
        risk_treatment = np.sum(treatment >= time)
        total_risk = risk_baseline + risk_treatment

        deaths_baseline = np.sum(baseline == time)
        total_deaths = deaths_baseline + np.sum(treatment == time)

        if total_risk <= 1:
            continue

        observed += deaths_baseline
        expected += total_deaths * risk_baseline / total_risk

        variance += (
            risk_baseline
            * risk_treatment
            * total_deaths
            * (total_risk - total_deaths)
            / (total_risk**2 * (total_risk - 1))
        )

    statistic = (observed - expected) ** 2 / variance
    p_value = chi2.sf(statistic, df=1)

    return statistic, p_value

def main():
    np.random.seed(42)
    #################### -------------------- Task 7 -------------------- ####################
    print("Task 7 - Simulation")
    deaths = 0
    n = 1000
    lifetimes = []
    observation_states = []
    oberservation_time = 30.5
    
    for i in range(n):
        lifetime, state_at_obeservation = simulate_woman_ctmc(Q, oberservation_time)

        if state_at_obeservation == 4: deaths += 1
        lifetimes.append(lifetime)
        observation_states.append(state_at_obeservation)
    
    print("\nTask 7 summary")
    print("--------------")
    print(f"Proportion dead by {oberservation_time} months: {deaths / n:.4f}")
    print(f"Mean simulated lifetime:            {np.mean(lifetimes):.2f} months")

    summaries(lifetimes, observation_states, n)
    plot_state_distribution(observation_states, oberservation_time)

    #################### -------------------- Task 8 -------------------- ####################
    print("\n\nTask 8 - Theoretical results")
    death_probability, mean_lifetime = theoretical_ctmc(Q, 30.5)
    print(f"Death probability by {oberservation_time} months: {death_probability:.4f}")
    print(f"Mean lifetime:                         {mean_lifetime:.2f} months")

    test_lifetime_distribution(lifetimes, Q)

    #################### -------------------- Task 9 --------------------
    print("\n\nTask 9 - Treatment analysis")
    # This is just to see how the function works
    survival_function(lifetimes)
    death_probability, mean_lifetime = theoretical_ctmc(Q_treat, 30.5)
    print(f"Treatment death probability by {oberservation_time} months: {death_probability:.4f}")
    print(f"Treatment mean lifetime:                         {mean_lifetime:.2f} months")

    # Running the actual simulation
    treat_lifetimes = []
    treat_observation_states = []

    for i in range(n):
        lifetime, state_at_obeservation = simulate_woman_ctmc(Q_treat, oberservation_time)
        treat_lifetimes.append(lifetime)
        treat_observation_states.append(state_at_obeservation)

    survival_baseline, survival_treatment = survival_function_compare(lifetimes, treat_lifetimes)

    # Does the treatment have an effect?
    statistic, p_value = log_rank_test(lifetimes, treat_lifetimes)
    print("\nLog-rank test comparing baseline and treatment")
    print("--------------------------------------------")
    print(f"Statistic: {statistic:.4f}")
    print(f"p-value:   {p_value:.4f}")
    if p_value < 0.05:
        print("Conclusion: significant difference in survival curves (reject H0).")
    else:
        print("Conclusion: no significant difference detected (do not reject H0).")

    return None


    
if __name__ == "__main__":
    with open("Log/P2_results.txt", "w") as file:
        with redirect_stdout(Tee(sys.stdout, file)):
            main()