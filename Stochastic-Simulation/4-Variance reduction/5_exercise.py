import numpy as np
from scipy.stats import norm, t
from scipy.optimize import minimize_scalar

################ ----- Exercise 4-a ----- ################

def crude_monte_carlo_integral(n=100, seed=42, confidence=0.95):
    rng = np.random.default_rng(seed)

    # Generate U_i ~ Uniform(0, 1)
    U = rng.uniform(0, 1, n)

    # Compute X_i = exp(U_i)
    X = np.exp(U)

    # Point estimate
    estimate = np.mean(X)

    # Sample standard deviation and standard error
    sample_std = np.std(X, ddof=1)
    standard_error = sample_std / np.sqrt(n)

    # t-based confidence interval
    alpha = 1 - confidence
    t_value = t.ppf(1 - alpha / 2, df=n - 1)

    lower = estimate - t_value * standard_error
    upper = estimate + t_value * standard_error

    return estimate, lower, upper

################ ----- Exercise 4-b ----- ################

def antithetic_integral(n=100, seed=42, confidence=0.95):
    rng = np.random.default_rng(seed)

    # Generate U_i ~ Uniform(0, 1)
    U = rng.uniform(0, 1, n)

    # Antithetic estimator:
    # Y_i = (exp(U_i) + exp(1 - U_i)) / 2
    Y = (np.exp(U) + np.exp(1 - U)) / 2

    # Point estimate
    estimate = np.mean(Y)

    # Sample standard deviation and standard error
    sample_std = np.std(Y, ddof=1)
    standard_error = sample_std / np.sqrt(n)

    # t-based confidence interval
    alpha = 1 - confidence
    t_value = t.ppf(1 - alpha / 2, df=n - 1)

    lower = estimate - t_value * standard_error
    upper = estimate + t_value * standard_error

    return estimate, lower, upper

################ ----- Exercise 4-c ----- ################

def control_variate_integral(n=100, seed=42, confidence=0.95):
    rng = np.random.default_rng(seed)

    # Generate U_i ~ Uniform(0, 1)
    U = rng.uniform(0, 1, n)

    # Original variable
    X = np.exp(U)

    # Control variate: U has known expectation E[U] = 1/2
    mu_U = 0.5

    # Approximately optimal coefficient from the slides
    c = -1.69

    # Control variate estimator
    Y = X + c * (U - mu_U)

    # Point estimate
    estimate = np.mean(Y)

    # Sample standard deviation and standard error
    sample_std = np.std(Y, ddof=1)
    standard_error = sample_std / np.sqrt(n)

    # t-based confidence interval
    alpha = 1 - confidence
    t_value = t.ppf(1 - alpha / 2, df=n - 1)

    lower = estimate - t_value * standard_error
    upper = estimate + t_value * standard_error

    return estimate, lower, upper

################ ----- Exercise 4-d ----- ################

def stratified_sampling_integral(n=100, n_strata=10, seed=42, confidence=0.95):
    rng = np.random.default_rng(seed)

    # Number of samples in each stratum
    samples_per_stratum = n // n_strata

    stratum_estimates = []

    for j in range(n_strata):
        lower_bound = j / n_strata
        upper_bound = (j + 1) / n_strata

        # Generate samples from stratum j
        U = rng.uniform(lower_bound, upper_bound, samples_per_stratum)

        # Estimate contribution from this stratum
        stratum_mean = np.mean(np.exp(U))
        stratum_estimates.append(stratum_mean)

    stratum_estimates = np.array(stratum_estimates)

    # Since all strata have equal width, average the stratum means
    estimate = np.mean(stratum_estimates)

    # For confidence interval:
    # Repeat the stratified estimate several times by treating each row as one replication.
    # Here we restructure the simulation into samples_per_stratum independent stratified replications.
    Y = []

    for i in range(samples_per_stratum):
        values = []

        for j in range(n_strata):
            lower_bound = j / n_strata
            upper_bound = (j + 1) / n_strata

            U = rng.uniform(lower_bound, upper_bound)
            values.append(np.exp(U))

        Y_i = np.mean(values)
        Y.append(Y_i)

    Y = np.array(Y)

    estimate = np.mean(Y)

    sample_std = np.std(Y, ddof=1)
    standard_error = sample_std / np.sqrt(samples_per_stratum)

    alpha = 1 - confidence
    t_value = t.ppf(1 - alpha / 2, df=samples_per_stratum - 1)

    lower = estimate - t_value * standard_error
    upper = estimate + t_value * standard_error

    return estimate, lower, upper

################ ----- Exercise 5-7 ----- ################

def confidence_interval(values, confidence=0.95):
    """
    Computes a t-based confidence interval for the mean of the given values.
    """
    n = len(values)
    estimate = np.mean(values)
    sample_std = np.std(values, ddof=1)
    standard_error = sample_std / np.sqrt(n)

    alpha = 1 - confidence
    t_value = t.ppf(1 - alpha / 2, df=n - 1)

    lower = estimate - t_value * standard_error
    upper = estimate + t_value * standard_error

    return estimate, lower, upper, standard_error


def crude_mc_tail_probability(a, n=10_000, seed=42, confidence=0.95):
    rng = np.random.default_rng(seed)

    # Generate Z_i ~ N(0, 1)
    Z = rng.normal(loc=0, scale=1, size=n)

    # Indicator I(Z_i > a)
    indicators = (Z > a).astype(float)

    estimate, lower, upper, standard_error = confidence_interval(
        indicators,
        confidence=confidence
    )

    return estimate, lower, upper, standard_error


def importance_sampling_tail_probability(a, n=10_000, sigma=1, seed=42, confidence=0.95):
    rng = np.random.default_rng(seed)

    # Generate X_i ~ N(a, sigma^2)
    Y = rng.normal(loc=a, scale=sigma, size=n)
    f = norm.pdf(Y, loc=0, scale=1)
    g = norm.pdf(Y, loc=a, scale=sigma)
    values = (Y > a).astype(float) * f / g

    estimate, lower, upper, standard_error = confidence_interval(
        values,
        confidence=confidence
    )

    return estimate, lower, upper, standard_error

################ ----- Exercise 5-h ----- ################ 

def confidence_interval(values, confidence=0.95):
    n = len(values)

    estimate = np.mean(values)
    sample_std = np.std(values, ddof=1)
    standard_error = sample_std / np.sqrt(n)

    alpha = 1 - confidence
    t_value = t.ppf(1 - alpha / 2, df=n - 1)

    lower = estimate - t_value * standard_error
    upper = estimate + t_value * standard_error

    return estimate, lower, upper, standard_error


def second_moment(lambda_value):
    """
    Analytical second moment of the importance sampling estimator.
    We minimize this to find the optimal lambda.
    """
    return (np.exp(lambda_value + 2) - 1) / (
        lambda_value * (lambda_value + 2)
    )


def importance_sampling_integral_exponential(
    n=10_000,
    lambda_value=1.0,
    seed=42,
    confidence=0.95
):
    rng = np.random.default_rng(seed)

    # Generate samples from the proposal distribution:
    # X ~ Exp(lambda)
    X = rng.exponential(scale=1 / lambda_value, size=n)

    # Proposal density:
    # g(x) = lambda * exp(-lambda*x)
    g_X = lambda_value * np.exp(-lambda_value * X)

    # Importance sampling values:
    # e^X / g(X), but only when X is in [0, 1]
    values = np.exp(X) / g_X * (X <= 1)

    estimate, lower, upper, standard_error = confidence_interval(
        values,
        confidence=confidence
    )

    return estimate, lower, upper, standard_error

import numpy as np


def confidence_interval(values, confidence=0.95):
    values = np.asarray(values)

    estimate = np.mean(values)
    standard_error = np.std(values, ddof=1) / np.sqrt(len(values))

    z = 1.96
    lower = estimate - z * standard_error
    upper = estimate + z * standard_error

    return estimate, lower, upper, standard_error


def importance_sampling_integral_optimal_g(n=10_000, seed=42, confidence=0.95):
    rng = np.random.default_rng(seed)

    # Generate U ~ Uniform(0, 1)
    U = rng.uniform(0, 1, size=n)

    # Generate X from g*(x) = e^x / (e - 1), 0 <= x <= 1
    X = np.log(1 + U * (np.e - 1))

    # Evaluate g*(X)
    g_X = np.exp(X) / (np.e - 1)

    # Importance sampling values:
    # e^X / g*(X)
    values = np.exp(X) / g_X

    estimate, lower, upper, standard_error = confidence_interval(
        values,
        confidence=confidence
    )

    return estimate, lower, upper, standard_error

def main():
    n = 100
    confidence = 0.95

    # Exercise 4-1: Crude Monte Carlo
    print("Crude Monte Carlo:")
    estimate, lower, upper = crude_monte_carlo_integral(n=n, confidence=confidence)
    print(f"Point estimate: {estimate:.4f}")
    print(f"95% confidence interval: [{lower:.4f}, {upper:.4f}]")

    # Exercise 4-2: Antithetic Variates
    print("\nAntithetic Variates:")
    estimate, lower, upper = antithetic_integral(n=n, confidence=confidence)
    print(f"Point estimate: {estimate:.4f}")
    print(f"95% confidence interval : [{lower:.4f}, {upper:.4f}]")

    # Exercise 4-3: Control Variates
    print("\nControl Variates:")
    estimate, lower, upper = control_variate_integral(n=n, confidence=confidence)
    print(f"Point estimate: {estimate:.4f}")
    print(f"95% confidence interval : [{lower:.4f}, {upper:.4f}]")

    # Exercise 4-4: Stratified Sampling
    print("\nStratified Sampling:")
    estimate, lower, upper = stratified_sampling_integral(n=n, n_strata=10, confidence=confidence)
    print(f"Point estimate: {estimate:.4f}")
    print(f"95% confidence interval : [{lower:.4f}, {upper:.4f}]")

    # TODO : Task 4-5 I need code from my laptop to do this part, I will add it later.

    # Exercise 4-7: Comparison of Variance Reduction Techniques
    a_values = [2, 4]
    n_values = [100, 1_000, 10_000, 100_000]
    sigma_values = [1]

    confidence = 0.95

    for a in a_values:
        true_value = 1 - norm.cdf(a)

        print()
        print(f"a = {a}")
        print("-" * 60)
        print(f"True value: {true_value:.8f}")
        print()

        for n in n_values:
            mc_estimate, mc_lower, mc_upper, mc_se = crude_mc_tail_probability(
                a=a,
                n=n,
                seed=42,
                confidence=confidence
            )

            print(f"Crude MC, n = {n}")
            print(f"Estimate: {mc_estimate:.8f}")
            print(f"95% CI:   [{mc_lower:.8f}, {mc_upper:.8f}]")
            print(f"SE:       {mc_se:.8f}")
            print()

            for sigma in sigma_values:
                is_estimate, is_lower, is_upper, is_se = importance_sampling_tail_probability(
                    a=a,
                    n=n,
                    sigma=sigma,
                    seed=42,
                    confidence=confidence
                )

                print(f"Importance sampling, n = {n}, sigma = {sigma}")
                print(f"Estimate: {is_estimate:.8f}")
                print(f"95% CI:   [{is_lower:.8f}, {is_upper:.8f}]")
                print(f"SE:       {is_se:.8f}")
                print()

        print("=" * 60)

    # Exercise 4-8: Comparison of Variance Reduction Techniques
    print("\nExercise 4-8: Comparison of Variance Reduction Techniques")
    n = 10_000
    confidence = 0.95

    true_value = np.e - 1

    # Find optimal lambda numerically
    result = minimize_scalar(
        second_moment,
        bounds=(0.001, 10),
        method="bounded"
    )

    optimal_lambda = result.x

    print(f"Exact value: {true_value:.6f}")
    print(f"Optimal lambda: {optimal_lambda:.4f}")
    print()

    lambda_values = [optimal_lambda - 0.2, optimal_lambda - 0.1, optimal_lambda, optimal_lambda + 0.1, optimal_lambda + 0.2]

    for lambda_value in lambda_values:
        estimate, lower, upper, se = importance_sampling_integral_exponential(
            n=n,
            lambda_value=lambda_value,
            confidence=confidence
        )

        analytical_variance = second_moment(lambda_value) - true_value**2
        true_value = np.e - 1

        print(f"lambda = {lambda_value:.4f}")
        print(f"Estimate: {estimate:.6f}")
        print(f"Exact value: {true_value:.6f}")
        print(f"Delta: {estimate - true_value:.6f}")
        print(f"95% CI:   [{lower:.6f}, {upper:.6f}]")
        print(f"SE:       {se:.6f}")
        print(f"Analytical variance of one observation: {analytical_variance:.6f}")
        print()

    # Exercise 4-9: importance_sampling_integral_optimal_g
    print("\nExercise 4-9: Importance Sampling with Optimal g")
    estimate, lower, upper, se = importance_sampling_integral_optimal_g(
        n=n,
        confidence=confidence
    )
    true_value = np.e - 1
    print(f"Estimate: {estimate:.6f}")
    print(f"Exact value: {true_value:.6f}")
    print(f"Delta: {estimate - true_value:.6f}")
    print(f"95% CI:   [{lower:.6f}, {upper:.6f}]")
    print(f"SE:       {se:.6f}")



if __name__ == "__main__":
    main()

