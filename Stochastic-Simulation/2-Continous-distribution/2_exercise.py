################ ----- Imports ----- ################
import numpy as np
import matplotlib.pyplot as plt
from scipy.stats import chisquare
import sys
import time

################ ----- Exercise 2-a ----- ################
def simulate_geometric(p, n=10_000, seed=42):
    """
    Simulate n observations from a geometric distribution.

    X = number of trials until first success.
    """
    rng = np.random.default_rng(seed)
    return rng.geometric(p, size=n)


def theoretical_geometric_pmf(k_values, p):
    """
    Compute theoretical geometric probabilities.

    P(X = k) = (1 - p)^(k - 1) * p
    """
    return (1 - p) ** (k_values - 1) * p

def compare_geometric_distribution(samples, p, max_k=None, save_path="assets/2a_geometric_comparison.png"):
    """
    Compare simulated and theoretical geometric distributions
    using a bar plot.
    """
    if max_k is None:
        max_k = np.percentile(samples, 99).astype(int)

    k_values = np.arange(1, max_k + 1)

    simulated_probs = np.array([
        np.mean(samples == k)
        for k in k_values
    ])

    print((k_values, simulated_probs))
    theoretical_probs = theoretical_geometric_pmf(k_values, p)

    plt.figure(figsize=(10, 5))
    plt.bar(k_values - 0.2, simulated_probs, width=0.4, label="Simulated")
    plt.bar(k_values + 0.2, theoretical_probs, width=0.4, label="Theoretical")

    plt.xlabel("k")
    plt.ylabel("Probability")
    plt.title(f"Geometric Distribution Comparison, p = {p}")
    plt.legend()
    plt.savefig(save_path)

def chi_square_test_geometric(samples, p, max_k=None):
    """
    Perform chi-squared test to check if samples match theoretical geometric distribution.
    
    Returns:
        statistic: chi-squared test statistic
        p_value: p-value from the test
    """
    if max_k is None:
        max_k = np.percentile(samples, 99).astype(int)
    
    k_values = np.arange(1, max_k + 1)
    
    # Observed frequencies
    observed_counts = np.array([np.sum(samples == k) for k in k_values])
    tail_observed = np.sum(samples > max_k)
    observed_counts = np.append(observed_counts, tail_observed)
    
    # Expected frequencies
    theoretical_probs = theoretical_geometric_pmf(k_values, p)
    tail_prob = 1 - np.sum(theoretical_probs)  # Remaining probability mass
    theoretical_probs = np.append(theoretical_probs, tail_prob)
    
    expected_counts = theoretical_probs * len(samples)
    
    # Perform chi-squared test
    statistic, p_value = chisquare(f_obs=observed_counts, f_exp=expected_counts)
    
    return statistic, p_value

################ ----- Exercise 2-b ----- ################

def sample_direct(x_values, probabilities, n=10_000, seed=42):
    """
    Generate samples from a discrete distribution using the direct method.
    """
    rng = np.random.default_rng(seed)

    cumulative_probs = np.cumsum(probabilities)
    U = rng.uniform(0, 1, n)

    indices = np.searchsorted(cumulative_probs, U)

    return x_values[indices]

def sample_rejection(x_values, probabilities, n=10_000, seed=42):
    """
    Generate samples from a discrete distribution using rejection sampling.

    Proposal:
        Uniform distribution over x_values.
    """
    rng = np.random.default_rng(seed)

    m = len(x_values)
    proposal_prob = 1 / m

    c = np.max(probabilities / proposal_prob)

    samples = []

    while len(samples) < n:
        # Propose Y uniformly from x_values
        proposed_index = rng.integers(0, m)
        Y = x_values[proposed_index]

        # Acceptance probability
        acceptance_prob = probabilities[proposed_index] / (c * proposal_prob)

        U = rng.uniform(0, 1)

        if U <= acceptance_prob:
            samples.append(Y)

    return np.array(samples)

def create_alias_table(probabilities):
    """
    Create probability and alias tables for the alias method.
    """
    n = len(probabilities)

    scaled_probs = probabilities * n

    prob_table = np.zeros(n)
    alias_table = np.zeros(n, dtype=int)

    small = []
    large = []

    for i, prob in enumerate(scaled_probs):
        if prob < 1:
            small.append(i)
        else:
            large.append(i)

    while small and large:
        small_index = small.pop()
        large_index = large.pop()

        prob_table[small_index] = scaled_probs[small_index]
        alias_table[small_index] = large_index

        scaled_probs[large_index] = (
            scaled_probs[large_index]
            - (1 - scaled_probs[small_index])
        )

        if scaled_probs[large_index] < 1:
            small.append(large_index)
        else:
            large.append(large_index)

    for index in large:
        prob_table[index] = 1

    for index in small:
        prob_table[index] = 1

    return prob_table, alias_table


def sample_alias(x_values, probabilities, n=10_000, seed=42):
    """
    Generate samples from a discrete distribution using the alias method.
    """
    rng = np.random.default_rng(seed)

    prob_table, alias_table = create_alias_table(probabilities)

    m = len(x_values)

    samples = np.zeros(n, dtype=int)

    for i in range(n):
        column = rng.integers(0, m)
        U = rng.uniform(0, 1)

        if U <= prob_table[column]:
            samples[i] = x_values[column]
        else:
            samples[i] = x_values[alias_table[column]]

    return samples

def sample_alias(x_values, probabilities, n=10_000, seed=42):
    """
    Generate samples from a discrete distribution using the alias method.
    """
    rng = np.random.default_rng(seed)

    prob_table, alias_table = create_alias_table(probabilities)

    m = len(x_values)

    samples = np.zeros(n, dtype=int)

    for i in range(n):
        column = rng.integers(0, m)
        U = rng.uniform(0, 1)

        if U <= prob_table[column]:
            samples[i] = x_values[column]
        else:
            samples[i] = x_values[alias_table[column]]

    return samples

def plot_histogram(samples, x_values, probabilities, title, save_path):
    plt.figure(figsize=(10, 5))
    plt.bar(x_values - 0.2, probabilities, width=0.4, label="Theoretical")
    plt.bar(x_values + 0.2, [np.mean(samples == x) for x in x_values], width=0.4, label="Simulated")

    plt.xlabel("x")
    plt.ylabel("Probability")
    plt.title(title)
    plt.legend()
    plt.savefig(save_path)


################ --------------- Exercise 2-b --------------- ################
def empirical_probabilities(samples, x_values):
    return np.array([np.mean(samples == x) for x in x_values])


def chi_square_accuracy(samples, x_values, probabilities):
    observed_counts = np.array([np.sum(samples == x) for x in x_values])
    expected_counts = probabilities * len(samples)

    statistic, p_value = chisquare(
        f_obs=observed_counts,
        f_exp=expected_counts
    )

    return statistic, p_value


def total_absolute_error(samples, x_values, probabilities):
    simulated_probs = empirical_probabilities(samples, x_values)
    return np.sum(np.abs(simulated_probs - probabilities))


def mean_squared_error_distribution(samples, x_values, probabilities):
    simulated_probs = empirical_probabilities(samples, x_values)
    return np.mean((simulated_probs - probabilities) ** 2)

def time_between_methods(methods, x_values, probabilities, n=10_000, seed=42):
    timings = {}

    for method_name, method_func in methods:
        start_time = time.time()
        method_func(x_values, probabilities, n=n, seed=seed)
        end_time = time.time()
        timings[method_name] = end_time - start_time

    return timings



def main():
    ################ ----- Exercise 2-a ----- ################
    p = [0.1, 0.5, 0.9]
    for i, p_val in enumerate(p):
        samples = simulate_geometric(p_val)
        compare_geometric_distribution(samples, p_val, save_path=f"assets/2a_geometric_comparison_{p_val}.png")
        
        # Perform chi-squared test
        chi_stat, p_val_test = chi_square_test_geometric(samples, p_val)
        print(f"Geometric Distribution (p={p_val}):")
        print(f"  Chi-Square Statistic: {chi_stat:.4f}")
        print(f"  p-value: {p_val_test:.4f}\n")


    ################ ----- Exercise 2-b ----- ################

    x_values = np.array([1, 2, 3, 4, 5, 6])

    probabilities = np.array([
        7 / 48,
        5 / 48,
        1 / 8,
        1 / 16,
        1 / 4,
        5 / 16
    ])

    direct_samples = sample_direct(x_values, probabilities)
    rejection_samples = sample_rejection(x_values, probabilities)
    alias_samples = sample_alias(x_values, probabilities)

    for method, samples in zip(
        ["Direct", "Rejection", "Alias"],
        [direct_samples, rejection_samples, alias_samples]
    ):
        plot_histogram(
            samples,
            x_values,
            probabilities,
            title=f"{method} Sampling",
            save_path=f"assets/2b_{method.lower()}_sampling.png"
        )

    ################ ----- Exercise 2-c ----- ################
    # Accuracy Metrics
    for method, samples in zip(
        ["Direct", "Rejection", "Alias"],
        [direct_samples, rejection_samples, alias_samples]
    ):
        chi_statistic, p_value = chi_square_accuracy(samples, x_values, probabilities)
        total_error = total_absolute_error(samples, x_values, probabilities)
        mse = mean_squared_error_distribution(samples, x_values, probabilities)

        print(f"{method} Sampling:")
        print(f"  Chi-Square Statistic: {chi_statistic:.4f}, p-value: {p_value:.4f}")
        print(f"  Total Absolute Error: {total_error:.4f}")
        print(f"  Mean Squared Error: {mse:.6f}\n")

    # Effeciency Metrics
    methods = [
        ("Direct", sample_direct),
        ("Rejection", sample_rejection),
        ("Alias", sample_alias)
    ]
    timings = time_between_methods(methods, x_values, probabilities)

    print("Efficiency Metrics:")
    for method, time_taken in timings.items():
        print(f"  {method}: {time_taken:.4f} seconds")

if __name__ == "__main__":
    main()