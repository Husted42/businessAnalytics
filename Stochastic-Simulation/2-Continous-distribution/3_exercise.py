
import numpy as np
from scipy.stats import kstest
from scipy.stats import expon, norm, pareto
import matplotlib.pyplot as plt
from scipy.stats import t, chi2

################ ----- Exercise 3-a ----- ################

def sample_exponential(lam, n=10_000, seed=42):
    """
    Generate samples from Exponential(lambda) using inverse transformation.

    F(x) = 1 - exp(-lambda*x)

    Therefore:
        X = -log(U) / lambda
    """
    rng = np.random.default_rng(seed)
    U = rng.uniform(0, 1, n)

    return -np.log(U) / lam


def sample_normal_box_muller(mu=0, sigma=1, n=10_000, seed=42):
    """
    Generate samples from N(mu, sigma^2) using the Box-Muller method.

    Z1 = sqrt(-2 log(U1)) cos(2 pi U2)
    Z2 = sqrt(-2 log(U1)) sin(2 pi U2)
    """
    rng = np.random.default_rng(seed)

    n_pairs = int(np.ceil(n / 2))

    U1 = rng.uniform(0, 1, n_pairs)
    U2 = rng.uniform(0, 1, n_pairs)

    R = np.sqrt(-2 * np.log(U1))

    Z1 = R * np.cos(2 * np.pi * U2)
    Z2 = R * np.sin(2 * np.pi * U2)

    Z = np.concatenate([Z1, Z2])[:n]

    return mu + sigma * Z


def sample_pareto(k, beta=1, n=10_000, seed=42):
    """
    Generate samples from Pareto(k, beta) with support [beta, infinity).

    F(x) = 1 - (beta / x)^k

    Therefore:
        X = beta * U^(-1/k)
    """
    rng = np.random.default_rng(seed)
    U = rng.uniform(0, 1, n)

    return beta * U ** (-1 / k)

def test_exponential(samples, lam):
    """
    Kolmogorov-Smirnov test for Exponential(lambda).
    """
    statistic, p_value = kstest(
        samples,
        "expon",
        args=(0, 1 / lam)
    )

    return statistic, p_value


def test_normal(samples, mu, sigma):
    """
    Kolmogorov-Smirnov test for N(mu, sigma^2).
    """
    statistic, p_value = kstest(
        samples,
        "norm",
        args=(mu, sigma)
    )

    return statistic, p_value


def test_pareto(samples, k, beta=1):
    """
    Kolmogorov-Smirnov test for Pareto(k, beta).
    """
    statistic, p_value = kstest(
        samples,
        "pareto",
        args=(k, 0, beta)
    )

    return statistic, p_value

def plot_exponential(samples, lam, save_path="assets/3a_exponential_comparison.png"):
    """
    Compare exponential simulation with theoretical density.
    """
    x = np.linspace(0, np.percentile(samples, 99.5), 500)
    theoretical_density = expon.pdf(x, scale=1 / lam)

    plt.figure(figsize=(8, 5))
    plt.hist(samples, bins=60, density=True, alpha=0.6, label="Simulated")
    plt.plot(x, theoretical_density, linewidth=2, label="Theoretical")
    plt.xlabel("x")
    plt.ylabel("Density")
    plt.title(f"Exponential Distribution, lambda = {lam}")
    plt.legend()
    plt.savefig(save_path)


def plot_normal(samples, mu, sigma, save_path="assets/3a_normal_comparison.png"):
    """
    Compare normal simulation with theoretical density.
    """
    x = np.linspace(
        np.percentile(samples, 0.5),
        np.percentile(samples, 99.5),
        500
    )

    theoretical_density = norm.pdf(x, loc=mu, scale=sigma)

    plt.figure(figsize=(8, 5))
    plt.hist(samples, bins=60, density=True, alpha=0.6, label="Simulated")
    plt.plot(x, theoretical_density, linewidth=2, label="Theoretical")
    plt.xlabel("x")
    plt.ylabel("Density")
    plt.title(f"Normal Distribution, mu = {mu}, sigma = {sigma}")
    plt.legend()
    plt.savefig(save_path)


def plot_pareto(samples, k, beta=1, save_path="assets/3a_pareto_comparison.png" ):
    """
    Compare Pareto simulation with theoretical density.
    """
    x = np.linspace(beta, np.percentile(samples, 99.5), 500)

    # scipy.stats.pareto uses shape parameter b = k and scale = beta
    theoretical_density = pareto.pdf(x, b=k, scale=beta)

    plt.figure(figsize=(8, 5))
    plt.hist(samples, bins=80, density=True, alpha=0.6, label="Simulated")
    plt.plot(x, theoretical_density, linewidth=2, label="Theoretical")
    plt.xlabel("x")
    plt.xlim(beta, np.percentile(samples, 95))
    plt.ylabel("Density")
    plt.title(f"Pareto Distribution, k = {k}, beta = {beta}")
    plt.legend()
    plt.savefig(save_path)

################ ----- Exercise 3-b ----- ################

def theoretical_pareto_mean(k, beta=1):
    """
    Theoretical mean of Pareto(k, beta), support [beta, infinity).

    E[X] = beta * k / (k - 1), for k > 1
    """
    if k <= 1:
        return np.inf

    return beta * k / (k - 1)


def theoretical_pareto_variance(k, beta=1):
    """
    Theoretical variance of Pareto(k, beta), support [beta, infinity).

    V[X] = beta^2 * k / ((k - 1)^2 * (k - 2)), for k > 2
    """
    if k <= 2:
        return np.inf

    return beta**2 * k / ((k - 1)**2 * (k - 2))


def compare_pareto_mean_variance(k_values, beta=1, n=10_000, seed=42):
    """
    Compare simulated sample mean and variance with theoretical values
    for different Pareto shape parameters k.
    """
    print("Pareto mean and variance comparison")
    print("-" * 80)

    for k in k_values:
        samples = sample_pareto(
            k=k,
            beta=beta,
            n=n,
            seed=seed
        )

        sample_mean = np.mean(samples)
        sample_variance = np.var(samples, ddof=1)

        theoretical_mean = theoretical_pareto_mean(k, beta)
        theoretical_variance = theoretical_pareto_variance(k, beta)

        print(f"k = {k}")
        print(f"Sample mean:          {sample_mean:.4f}")
        print(f"Theoretical mean:     {theoretical_mean:.4f}")
        print(f"Mean difference:      {sample_mean - theoretical_mean:.4f}")

        print(f"Sample variance:      {sample_variance:.4f}")
        print(f"Theoretical variance: {theoretical_variance:.4f}")
        print(f"Variance difference:  {sample_variance - theoretical_variance:.4f}")

        print("-" * 80)

################ ----- Exercise 3-c ----- ################



def confidence_interval_mean(samples, confidence_level=0.95):
    """
    95% confidence interval for the mean of a normal distribution
    when sigma is unknown.

    CI:
        x_bar +/- t_(1-alpha/2, n-1) * s / sqrt(n)
    """
    n = len(samples)
    alpha = 1 - confidence_level

    sample_mean = np.mean(samples)
    sample_std = np.std(samples, ddof=1)

    t_critical = t.ppf(1 - alpha / 2, df=n - 1)

    lower = sample_mean - t_critical * sample_std / np.sqrt(n)
    upper = sample_mean + t_critical * sample_std / np.sqrt(n)

    return lower, upper


def confidence_interval_variance(samples, confidence_level=0.95):
    """
    95% confidence interval for the variance of a normal distribution.

    CI:
        ((n-1)s^2 / chi2_(1-alpha/2),
         (n-1)s^2 / chi2_(alpha/2))
    """
    n = len(samples)
    alpha = 1 - confidence_level

    sample_variance = np.var(samples, ddof=1)

    chi2_lower = chi2.ppf(alpha / 2, df=n - 1)
    chi2_upper = chi2.ppf(1 - alpha / 2, df=n - 1)

    lower = (n - 1) * sample_variance / chi2_upper
    upper = (n - 1) * sample_variance / chi2_lower

    return lower, upper


def generate_confidence_intervals_normal(
    mu=0,
    sigma=1,
    n=10,
    number_of_intervals=100,
    confidence_level=0.95,
    seed=42
):
    """
    Generate confidence intervals for the mean and variance
    from repeated normal samples.
    """
    rng = np.random.default_rng(seed)

    mean_intervals = []
    variance_intervals = []

    for _ in range(number_of_intervals):
        # Generate n normal observations using Box-Muller.
        # Use a new seed from rng each time to avoid identical samples.
        sample_seed = rng.integers(0, 1_000_000_000)

        samples = sample_normal_box_muller(
            mu=mu,
            sigma=sigma,
            n=n,
            seed=sample_seed
        )

        mean_ci = confidence_interval_mean(
            samples,
            confidence_level=confidence_level
        )

        variance_ci = confidence_interval_variance(
            samples,
            confidence_level=confidence_level
        )

        mean_intervals.append(mean_ci)
        variance_intervals.append(variance_ci)

    return np.array(mean_intervals), np.array(variance_intervals)

def interval_coverage(intervals, true_value):
    """
    Calculate how many confidence intervals contain the true value.
    """
    contains_true_value = (
        (intervals[:, 0] <= true_value)
        & (true_value <= intervals[:, 1])
    )

    coverage = np.mean(contains_true_value)

    return coverage, contains_true_value


def print_confidence_interval_summary(
    mean_intervals,
    variance_intervals,
    true_mu,
    true_variance
):
    """
    Print coverage results for mean and variance confidence intervals.
    """
    mean_coverage, mean_contains = interval_coverage(
        mean_intervals,
        true_mu
    )

    variance_coverage, variance_contains = interval_coverage(
        variance_intervals,
        true_variance
    )

    print("Confidence interval results")
    print("-" * 40)

    print(f"Mean intervals containing true mean: {np.sum(mean_contains)} / {len(mean_intervals)}")
    print(f"Observed mean coverage:             {mean_coverage:.4f}")

    print()

    print(f"Variance intervals containing true variance: {np.sum(variance_contains)} / {len(variance_intervals)}")
    print(f"Observed variance coverage:             {variance_coverage:.4f}")

    print("-" * 40)

def plot_confidence_intervals(intervals, true_value, title, ylabel, save_path):
    """
    Plot confidence intervals and show which intervals contain the true value.
    """
    coverage, contains_true_value = interval_coverage(intervals, true_value)

    plt.figure(figsize=(10, 6))

    for i, (lower, upper) in enumerate(intervals):
        if contains_true_value[i]:
            plt.plot([i, i], [lower, upper], linewidth=1)
        else:
            plt.plot([i, i], [lower, upper], linewidth=2)

    plt.axhline(true_value, linestyle="--", label="True value")

    plt.xlabel("Interval number")
    plt.ylabel(ylabel)
    plt.title(f"{title}, observed coverage = {coverage:.2f}")
    plt.legend()
    plt.savefig(save_path)
    plt.close()

################ ----- Exercise 3-d ----- ################

import time


def sample_pareto_composition(mu=1, n=10_000, seed=42):
    """
    Generate Pareto observations using the composition method.

    From the slides:
        Y ~ Exp(mu)
        X | Y = y ~ Exp(y)

    This gives a Pareto-type distribution with support [0, infinity):

        F_X(x) = 1 - (1 + x / mu)^(-1)

    So this corresponds to k = 1 and beta = mu
    for the Pareto distribution on [0, infinity).
    """
    rng = np.random.default_rng(seed)

    # Step 1: Generate Y ~ Exp(mu)
    U1 = rng.uniform(0, 1, n)
    Y = -np.log(U1) / mu

    # Step 2: Given Y = y, generate X | Y = y ~ Exp(y)
    U2 = rng.uniform(0, 1, n)
    X = -np.log(U2) / Y

    return X


def sample_pareto_direct_nonnegative(k=1, beta=1, n=10_000, seed=42):
    """
    Generate Pareto-type observations on [0, infinity)
    using direct inversion.

    Formula from the slides:

        F_X(x) = 1 - (1 + x / beta)^(-k)

    Therefore:

        X = beta * (U^(-1/k) - 1)
    """
    rng = np.random.default_rng(seed)
    U = rng.uniform(0, 1, n)

    return beta * (U ** (-1 / k) - 1)

def test_pareto_nonnegative(samples, k=1, beta=1):
    """
    KS test for Pareto-type distribution on [0, infinity).

    CDF:
        F(x) = 1 - (1 + x / beta)^(-k)

    This is equivalent to scipy's Lomax distribution.
    """
    from scipy.stats import lomax

    statistic, p_value = kstest(
        samples,
        "lomax",
        args=(k, 0, beta)
    )

    return statistic, p_value


def plot_pareto_composition_comparison(
    composition_samples,
    direct_samples,
    k=1,
    beta=1,
    save_path="assets/3d_pareto_composition_vs_direct.png"
):
    """
    Compare Pareto composition method with direct inversion method.
    """
    from scipy.stats import lomax

    upper = min(
        np.percentile(composition_samples, 99),
        np.percentile(direct_samples, 99)
    )

    x = np.linspace(0, upper, 500)
    theoretical_density = lomax.pdf(x, c=k, scale=beta)

    plt.figure(figsize=(8, 5))

    plt.hist(
        composition_samples,
        bins=80,
        density=True,
        alpha=0.5,
        label="Composition method"
    )

    plt.hist(
        direct_samples,
        bins=80,
        density=True,
        alpha=0.5,
        label="Direct inversion"
    )

    plt.plot(
        x,
        theoretical_density,
        linewidth=2,
        label="Theoretical density"
    )

    plt.xlabel("x")
    plt.ylabel("Density")
    plt.title("Pareto on [0, infinity): composition vs direct inversion")
    plt.xlim(0, upper)
    plt.legend()
    plt.savefig(save_path)
    plt.close()

def main():
    ################ ----- Exercise 3-a ----- ################
    # Exponential
    lam = 0.5
    exp_samples = sample_exponential(lam)
    exp_statistic, exp_p_value = test_exponential(exp_samples, lam)
    print(f"Exponential: KS Statistic = {exp_statistic:.4f}, p-value = {exp_p_value:.4f}")
    plot_exponential(exp_samples, lam, save_path="assets/3a_exponential_comparison.png")

    # Normal
    mu, sigma = 0, 1
    normal_samples = sample_normal_box_muller(mu, sigma)
    normal_statistic, normal_p_value = test_normal(normal_samples, mu, sigma)
    print(f"Normal: KS Statistic = {normal_statistic:.4f}, p-value = {normal_p_value:.4f}")
    plot_normal(normal_samples, mu, sigma, save_path="assets/3a_normal_comparison.png")

    # Pareto
    k, beta = 2, 1
    pareto_samples = sample_pareto(k, beta)
    pareto_statistic, pareto_p_value = test_pareto(pareto_samples, k, beta)
    print(f"Pareto: KS Statistic = {pareto_statistic:.4f}, p-value = {pareto_p_value:.4f}")
    plot_pareto(pareto_samples, k, beta, save_path="assets/3a_pareto_comparison.png")
    
    # Try wth different k-values
    k_values = [2.05, 2.5, 3, 4]
    for k in k_values:
        pareto_samples = sample_pareto(k, beta)
        pareto_statistic, pareto_p_value = test_pareto(pareto_samples, k, beta)
        print(f"Pareto (k={k}): KS Statistic = {pareto_statistic:.4f}, p-value = {pareto_p_value:.4f}")
        plot_pareto(
            pareto_samples,
            k,
            beta,
            save_path=f"assets/3a_pareto_comparison_k_{k}.png"
        )

    ################ ----- Exercise 3-b ----- ################

    beta = 1
    k_values = [2.05, 2.5, 3, 4]

    compare_pareto_mean_variance(
        k_values=k_values,
        beta=beta,
        n=10_000,
        seed=42
    )

    ################ ----- Exercise 3-c ----- ################

    mu = 0
    sigma = 1
    true_variance = sigma**2

    mean_intervals, variance_intervals = generate_confidence_intervals_normal(
        mu=mu,
        sigma=sigma,
        n=10,
        number_of_intervals=100,
        confidence_level=0.95,
        seed=42
    )

    print_confidence_interval_summary(
        mean_intervals=mean_intervals,
        variance_intervals=variance_intervals,
        true_mu=mu,
        true_variance=true_variance
    )

    plot_confidence_intervals(
        intervals=mean_intervals,
        true_value=mu,
        title="95% confidence intervals for the mean",
        ylabel="Mean",
        save_path="assets/3c_mean_confidence_intervals.png"
    )

    plot_confidence_intervals(
        intervals=variance_intervals,
        true_value=true_variance,
        title="95% confidence intervals for the variance",
        ylabel="Variance",
        save_path="assets/3c_variance_confidence_intervals.png"
    )

    ################ ----- Exercise 3-d ----- ################

    mu = 1
    beta = mu
    k = 1
    n = 10_000

    start = time.perf_counter()
    pareto_composition_samples = sample_pareto_composition(
        mu=mu,
        n=n,
        seed=42
    )
    composition_time = time.perf_counter() - start

    start = time.perf_counter()
    pareto_direct_samples = sample_pareto_direct_nonnegative(
        k=k,
        beta=beta,
        n=n,
        seed=42
    )
    direct_time = time.perf_counter() - start

    comp_statistic, comp_p_value = test_pareto_nonnegative(
        pareto_composition_samples,
        k=k,
        beta=beta
    )

    direct_statistic, direct_p_value = test_pareto_nonnegative(
        pareto_direct_samples,
        k=k,
        beta=beta
    )

    print("Pareto composition method")
    print("-" * 40)
    print(f"KS Statistic: {comp_statistic:.4f}")
    print(f"p-value:      {comp_p_value:.4f}")
    print(f"Runtime:      {composition_time:.6f} seconds")
    print()

    print("Pareto direct inversion method")
    print("-" * 40)
    print(f"KS Statistic: {direct_statistic:.4f}")
    print(f"p-value:      {direct_p_value:.4f}")
    print(f"Runtime:      {direct_time:.6f} seconds")
    print()

    plot_pareto_composition_comparison(
        composition_samples=pareto_composition_samples,
        direct_samples=pareto_direct_samples,
        k=k,
        beta=beta,
        save_path="assets/3d_pareto_composition_vs_direct.png"
    )

    print(f"Composition sample mean: {np.mean(pareto_composition_samples):.4f}")
    print(f"Direct sample mean:      {np.mean(pareto_direct_samples):.4f}")
    print("Theoretical mean:        infinite for k = 1")

if __name__ == "__main__":
    main()