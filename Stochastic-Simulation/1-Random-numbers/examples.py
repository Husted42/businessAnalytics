import numpy as np


# ------------------------------------------------------------
# 1. Random number generation
# ------------------------------------------------------------

def demonstrate_random_number_generation(seed: int = 42, n: int = 10):
    
    rng = np.random.default_rng(seed)

    uniform_numbers = rng.uniform(0, 1, size=n)
    normal_numbers = rng.normal(loc=0, scale=1, size=n)
    integer_numbers = rng.integers(low=1, high=7, size=n)

    print("\n--- 1. Random number generation ---")
    print("Uniform(0, 1) numbers:")
    print(uniform_numbers)

    print("\nNormal(0, 1) numbers:")
    print(normal_numbers)

    print("\nSimulated dice rolls:")
    print(integer_numbers)

    return uniform_numbers, normal_numbers, integer_numbers


# ------------------------------------------------------------
# 2. Stochastic simulation
# ------------------------------------------------------------

def simulate_dice_experiment(seed: int = 42, n_rolls: int = 10_000):
    
    rng = np.random.default_rng(seed)
    rolls = rng.integers(1, 7, size=n_rolls)

    values, counts = np.unique(rolls, return_counts=True)
    empirical_probabilities = counts / n_rolls

    print("\n--- 2. Stochastic simulation: dice experiment ---")
    print(f"Number of dice rolls: {n_rolls}")

    for value, probability in zip(values, empirical_probabilities):
        print(f"P(roll = {value}) ≈ {probability:.4f}")

    print("Theoretical probability for each side = 1/6 ≈", round(1 / 6, 4))

    return rolls, empirical_probabilities


# ------------------------------------------------------------
# 3. Basic probability rules
# ------------------------------------------------------------

def demonstrate_basic_probability_rules(seed: int = 42, n: int = 100_000):
    
    rng = np.random.default_rng(seed)
    rolls = rng.integers(1, 7, size=n)

    A = rolls % 2 == 0
    B = rolls > 4

    p_A = np.mean(A)
    p_not_A = np.mean(~A)
    p_B = np.mean(B)
    p_A_and_B = np.mean(A & B)
    p_A_or_B = np.mean(A | B)

    inclusion_exclusion = p_A + p_B - p_A_and_B

    print("\n--- 3. Basic probability rules ---")
    print("A = roll is even")
    print("B = roll is greater than 4")

    print(f"P(A) ≈ {p_A:.4f}")
    print(f"P(A^c) ≈ {p_not_A:.4f}")
    print(f"1 - P(A) ≈ {1 - p_A:.4f}")

    print(f"\nP(B) ≈ {p_B:.4f}")
    print(f"P(A ∩ B) ≈ {p_A_and_B:.4f}")
    print(f"P(A ∪ B) directly ≈ {p_A_or_B:.4f}")
    print(f"P(A) + P(B) - P(A ∩ B) ≈ {inclusion_exclusion:.4f}")

    return {
        "P(A)": p_A,
        "P(A_complement)": p_not_A,
        "P(B)": p_B,
        "P(A_and_B)": p_A_and_B,
        "P(A_or_B)": p_A_or_B,
    }


# ------------------------------------------------------------
# 4. Conditional probability
# ------------------------------------------------------------

def demonstrate_conditional_probability(seed: int = 42, n: int = 100_000):
    
    rng = np.random.default_rng(seed)
    rolls = rng.integers(1, 7, size=n)

    A = rolls % 2 == 0
    B = rolls > 3

    p_B = np.mean(B)
    p_A_and_B = np.mean(A & B)
    p_A_given_B = p_A_and_B / p_B

    direct_conditional = np.mean(A[B])

    print("\n--- 4. Conditional probability ---")
    print("A = roll is even")
    print("B = roll is greater than 3")

    print(f"P(B) ≈ {p_B:.4f}")
    print(f"P(A ∩ B) ≈ {p_A_and_B:.4f}")
    print(f"P(A | B) = P(A ∩ B) / P(B) ≈ {p_A_given_B:.4f}")
    print(f"Direct estimate from samples satisfying B ≈ {direct_conditional:.4f}")

    return p_A_given_B


# ------------------------------------------------------------
# 5. Law of total probability
# ------------------------------------------------------------

def demonstrate_law_of_total_probability(seed: int = 42, n: int = 100_000):
    
    rng = np.random.default_rng(seed)

    machine_probabilities = np.array([0.50, 0.30, 0.20])
    defect_probabilities = np.array([0.01, 0.03, 0.05])

    machines = rng.choice([0, 1, 2], size=n, p=machine_probabilities)
    random_uniforms = rng.uniform(0, 1, size=n)

    defective = random_uniforms < defect_probabilities[machines]

    empirical_p_defective = np.mean(defective)
    theoretical_p_defective = np.sum(defect_probabilities * machine_probabilities)

    print("\n--- 5. Law of total probability ---")
    print("Machine probabilities:", machine_probabilities)
    print("Defect probabilities:", defect_probabilities)

    print(f"Empirical P(defective) ≈ {empirical_p_defective:.4f}")
    print(f"Theoretical P(defective) = sum P(defective | machine)P(machine)")
    print(f"Theoretical P(defective) = {theoretical_p_defective:.4f}")

    return empirical_p_defective, theoretical_p_defective


# ------------------------------------------------------------
# 6. Bayes' theorem
# ------------------------------------------------------------

def demonstrate_bayes_theorem():
    
    prevalence = 0.01
    sensitivity = 0.95
    false_positive_rate = 0.05

    p_disease = prevalence
    p_no_disease = 1 - prevalence

    p_positive_given_disease = sensitivity
    p_positive_given_no_disease = false_positive_rate

    p_positive = (
        p_positive_given_disease * p_disease
        + p_positive_given_no_disease * p_no_disease
    )

    p_disease_given_positive = (
        p_positive_given_disease * p_disease / p_positive
    )

    print("\n--- 6. Bayes' theorem ---")
    print("A = person has disease")
    print("B = test is positive")

    print(f"P(A) = prevalence = {p_disease:.4f}")
    print(f"P(B | A) = sensitivity = {p_positive_given_disease:.4f}")
    print(f"P(B | A^c) = false positive rate = {p_positive_given_no_disease:.4f}")
    print(f"P(B) = total probability of positive test = {p_positive:.4f}")
    print(f"P(A | B) = probability of disease given positive test = {p_disease_given_positive:.4f}")

    return p_disease_given_positive


# ------------------------------------------------------------
# 7. Independence of events
# ------------------------------------------------------------

def demonstrate_event_independence(seed: int = 42, n: int = 100_000):
    
    rng = np.random.default_rng(seed)

    coin1 = rng.choice(["H", "T"], size=n)
    coin2 = rng.choice(["H", "T"], size=n)

    A = coin1 == "H"
    B = coin2 == "H"

    p_A = np.mean(A)
    p_B = np.mean(B)
    p_A_and_B = np.mean(A & B)

    print("\n--- 7. Independence of events ---")
    print("Example 1: two coin flips")
    print("A = first coin is heads")
    print("B = second coin is heads")

    print(f"P(A) ≈ {p_A:.4f}")
    print(f"P(B) ≈ {p_B:.4f}")
    print(f"P(A ∩ B) ≈ {p_A_and_B:.4f}")
    print(f"P(A)P(B) ≈ {p_A * p_B:.4f}")

    rolls = rng.integers(1, 7, size=n)

    C = rolls % 2 == 0
    D = rolls > 3

    p_C = np.mean(C)
    p_D = np.mean(D)
    p_C_and_D = np.mean(C & D)

    print("\nExample 2: one die roll")
    print("C = roll is even")
    print("D = roll is greater than 3")

    print(f"P(C) ≈ {p_C:.4f}")
    print(f"P(D) ≈ {p_D:.4f}")
    print(f"P(C ∩ D) ≈ {p_C_and_D:.4f}")
    print(f"P(C)P(D) ≈ {p_C * p_D:.4f}")

    return {
        "coin_independence_difference": abs(p_A_and_B - p_A * p_B),
        "die_independence_difference": abs(p_C_and_D - p_C * p_D),
    }


# ------------------------------------------------------------
# 8. Random variables
# ------------------------------------------------------------

def demonstrate_random_variable(seed: int = 42, n: int = 20):
    
    rng = np.random.default_rng(seed)

    coin1 = rng.choice(["H", "T"], size=n)
    coin2 = rng.choice(["H", "T"], size=n)

    X = (coin1 == "H").astype(int) + (coin2 == "H").astype(int)

    print("\n--- 8. Random variables ---")
    print("Experiment: flip two coins")
    print("Random variable X = number of heads")

    for i in range(n):
        outcome = f"{coin1[i]}{coin2[i]}"
        print(f"Outcome {outcome} -> X = {X[i]}")

    return X


# ------------------------------------------------------------
# 9. Discrete probability distributions
# ------------------------------------------------------------

def demonstrate_discrete_distribution(seed: int = 42, n: int = 100_000):
    
    rng = np.random.default_rng(seed)

    coin_flips = rng.choice([0, 1], size=(n, 3))
    X = np.sum(coin_flips, axis=1)

    possible_values = np.array([0, 1, 2, 3])
    pmf = np.array([np.mean(X == value) for value in possible_values])
    cdf = np.array([np.mean(X <= value) for value in possible_values])

    print("\n--- 9. Discrete probability distribution ---")
    print("X = number of heads in 3 coin flips")

    print("\nProbability mass function:")
    for value, probability in zip(possible_values, pmf):
        print(f"f_X({value}) = P(X = {value}) ≈ {probability:.4f}")

    print("\nDistribution function:")
    for value, probability in zip(possible_values, cdf):
        print(f"F_X({value}) = P(X <= {value}) ≈ {probability:.4f}")

    return possible_values, pmf, cdf


# ------------------------------------------------------------
# 10. Continuous probability distributions
# ------------------------------------------------------------

def demonstrate_continuous_distribution(seed: int = 42, n: int = 100_000):
    
    rng = np.random.default_rng(seed)
    X = rng.normal(loc=0, scale=1, size=n)

    p_less_than_zero = np.mean(X <= 0)
    p_between_minus_one_and_one = np.mean((-1 <= X) & (X <= 1))
    p_greater_than_two = np.mean(X > 2)

    print("\n--- 10. Continuous probability distribution ---")
    print("X ~ Normal(0, 1)")

    print(f"P(X <= 0) ≈ {p_less_than_zero:.4f}")
    print(f"P(-1 <= X <= 1) ≈ {p_between_minus_one_and_one:.4f}")
    print(f"P(X > 2) ≈ {p_greater_than_two:.4f}")

    return X


# ------------------------------------------------------------
# 11. Expectation
# ------------------------------------------------------------

def demonstrate_expectation(seed: int = 42, n: int = 100_000):
    
    rng = np.random.default_rng(seed)
    X = rng.integers(1, 7, size=n)

    empirical_expectation = np.mean(X)
    theoretical_expectation = np.sum(np.arange(1, 7) * (1 / 6))

    print("\n--- 11. Expectation ---")
    print("X = fair die roll")

    print(f"Empirical E[X] ≈ {empirical_expectation:.4f}")
    print(f"Theoretical E[X] = {theoretical_expectation:.4f}")

    return empirical_expectation, theoretical_expectation


# ------------------------------------------------------------
# 12. Expectation of a function
# ------------------------------------------------------------

def demonstrate_expectation_of_function(seed: int = 42, n: int = 100_000):
    
    rng = np.random.default_rng(seed)
    X = rng.integers(1, 7, size=n)

    g_X = X ** 2

    empirical_expectation = np.mean(g_X)
    theoretical_expectation = np.sum((np.arange(1, 7) ** 2) * (1 / 6))

    print("\n--- 12. Expectation of a function ---")
    print("X = fair die roll")
    print("g(X) = X^2")

    print(f"Empirical E[g(X)] = E[X^2] ≈ {empirical_expectation:.4f}")
    print(f"Theoretical E[X^2] = {theoretical_expectation:.4f}")

    return empirical_expectation, theoretical_expectation


# ------------------------------------------------------------
# 13. Moments and variance
# ------------------------------------------------------------

def demonstrate_moments_and_variance(seed: int = 42, n: int = 100_000):
    
    rng = np.random.default_rng(seed)
    X = rng.integers(1, 7, size=n)

    empirical_E_X = np.mean(X)
    empirical_E_X2 = np.mean(X ** 2)

    empirical_variance_direct = np.mean((X - empirical_E_X) ** 2)
    empirical_variance_formula = empirical_E_X2 - empirical_E_X ** 2

    values = np.arange(1, 7)
    probabilities = np.ones(6) / 6

    theoretical_E_X = np.sum(values * probabilities)
    theoretical_E_X2 = np.sum(values ** 2 * probabilities)
    theoretical_variance = theoretical_E_X2 - theoretical_E_X ** 2

    print("\n--- 13. Moments and variance ---")
    print("X = fair die roll")

    print(f"Empirical E[X] ≈ {empirical_E_X:.4f}")
    print(f"Empirical E[X^2] ≈ {empirical_E_X2:.4f}")
    print(f"Empirical Var[X] direct ≈ {empirical_variance_direct:.4f}")
    print(f"Empirical Var[X] from E[X^2] - E[X]^2 ≈ {empirical_variance_formula:.4f}")
    print(f"Theoretical Var[X] = {theoretical_variance:.4f}")

    return empirical_variance_direct, theoretical_variance


# ------------------------------------------------------------
# 14. Joint, marginal, and conditional distributions
# ------------------------------------------------------------

def demonstrate_joint_marginal_conditional_discrete(seed: int = 42, n: int = 100_000):
    
    rng = np.random.default_rng(seed)

    die1 = rng.integers(1, 7, size=n)
    die2 = rng.integers(1, 7, size=n)

    X = die1
    Y = die1 + die2

    chosen_y = 7

    possible_x_values = np.arange(1, 7)

    marginal_X = {
        x: np.mean(X == x)
        for x in possible_x_values
    }

    conditional_X_given_Y = {
        x: np.mean(X[Y == chosen_y] == x)
        for x in possible_x_values
    }

    print("\n--- 14. Joint, marginal, and conditional discrete distributions ---")
    print("X = first die")
    print("Y = sum of two dice")

    print("\nMarginal distribution of X:")
    for x, probability in marginal_X.items():
        print(f"P(X = {x}) ≈ {probability:.4f}")

    print(f"\nConditional distribution of X given Y = {chosen_y}:")
    for x, probability in conditional_X_given_Y.items():
        print(f"P(X = {x} | Y = {chosen_y}) ≈ {probability:.4f}")

    print("\nExample joint probability:")
    example_x = 3
    example_y = 7
    joint_probability = np.mean((X == example_x) & (Y == example_y))
    print(f"P(X = {example_x}, Y = {example_y}) ≈ {joint_probability:.4f}")

    return marginal_X, conditional_X_given_Y


# ------------------------------------------------------------
# 15. Covariance and correlation
# ------------------------------------------------------------

def demonstrate_covariance_and_correlation(seed: int = 42, n: int = 100_000):
    
    rng = np.random.default_rng(seed)

    X = rng.normal(0, 1, size=n)
    noise = rng.normal(0, 1, size=n)

    Y = 2 * X + noise

    empirical_E_X = np.mean(X)
    empirical_E_Y = np.mean(Y)
    empirical_E_XY = np.mean(X * Y)

    covariance_formula = empirical_E_XY - empirical_E_X * empirical_E_Y
    covariance_numpy = np.cov(X, Y, ddof=0)[0, 1]

    correlation_numpy = np.corrcoef(X, Y)[0, 1]

    print("\n--- 15. Covariance and correlation ---")
    print("X ~ Normal(0, 1)")
    print("Y = 2X + noise")

    print(f"E[X] ≈ {empirical_E_X:.4f}")
    print(f"E[Y] ≈ {empirical_E_Y:.4f}")
    print(f"E[XY] ≈ {empirical_E_XY:.4f}")
    print(f"Cov(X, Y) = E[XY] - E[X]E[Y] ≈ {covariance_formula:.4f}")
    print(f"Cov(X, Y) from NumPy ≈ {covariance_numpy:.4f}")
    print(f"Corr(X, Y) ≈ {correlation_numpy:.4f}")

    return covariance_formula, correlation_numpy


# ------------------------------------------------------------
# 16. Variance of sums
# ------------------------------------------------------------

def demonstrate_variance_of_sum(seed: int = 42, n: int = 100_000):
    
    rng = np.random.default_rng(seed)

    X = rng.normal(0, 1, size=n)

    Y_independent = rng.normal(0, 1, size=n)

    noise = rng.normal(0, 1, size=n)
    Y_correlated = X + noise

    var_X = np.var(X)
    var_Y_independent = np.var(Y_independent)
    cov_independent = np.cov(X, Y_independent, ddof=0)[0, 1]

    direct_var_sum_independent = np.var(X + Y_independent)
    formula_var_sum_independent = var_X + var_Y_independent + 2 * cov_independent

    var_Y_correlated = np.var(Y_correlated)
    cov_correlated = np.cov(X, Y_correlated, ddof=0)[0, 1]

    direct_var_sum_correlated = np.var(X + Y_correlated)
    formula_var_sum_correlated = var_X + var_Y_correlated + 2 * cov_correlated

    print("\n--- 16. Variance of sums ---")

    print("\nCase 1: independent variables")
    print(f"Var(X + Y) directly ≈ {direct_var_sum_independent:.4f}")
    print(f"Var(X) + Var(Y) + 2Cov(X,Y) ≈ {formula_var_sum_independent:.4f}")
    print(f"Cov(X,Y) ≈ {cov_independent:.4f}")

    print("\nCase 2: correlated variables")
    print(f"Var(X + Y) directly ≈ {direct_var_sum_correlated:.4f}")
    print(f"Var(X) + Var(Y) + 2Cov(X,Y) ≈ {formula_var_sum_correlated:.4f}")
    print(f"Cov(X,Y) ≈ {cov_correlated:.4f}")

    return {
        "independent_direct": direct_var_sum_independent,
        "independent_formula": formula_var_sum_independent,
        "correlated_direct": direct_var_sum_correlated,
        "correlated_formula": formula_var_sum_correlated,
    }


# ------------------------------------------------------------
# 17. Independent random variables
# ------------------------------------------------------------

def demonstrate_independent_random_variables(seed: int = 42, n: int = 100_000):
    
    rng = np.random.default_rng(seed)

    X = rng.normal(0, 1, size=n)
    Y = rng.normal(0, 1, size=n)

    E_X = np.mean(X)
    E_Y = np.mean(Y)
    E_XY = np.mean(X * Y)

    covariance = np.cov(X, Y, ddof=0)[0, 1]
    correlation = np.corrcoef(X, Y)[0, 1]

    print("\n--- 17. Independent random variables ---")
    print("X and Y are simulated independently")

    print(f"E[X] ≈ {E_X:.4f}")
    print(f"E[Y] ≈ {E_Y:.4f}")
    print(f"E[X]E[Y] ≈ {E_X * E_Y:.4f}")
    print(f"E[XY] ≈ {E_XY:.4f}")
    print(f"Cov(X, Y) ≈ {covariance:.4f}")
    print(f"Corr(X, Y) ≈ {correlation:.4f}")

    return E_XY, E_X * E_Y, covariance, correlation


# ------------------------------------------------------------
# 18. Simple stochastic model
# ------------------------------------------------------------

def simulate_simple_queue(seed: int = 42, n_customers: int = 20):
    
    rng = np.random.default_rng(seed)

    interarrival_times = rng.exponential(scale=2.0, size=n_customers)
    service_times = rng.exponential(scale=1.5, size=n_customers)

    arrival_times = np.cumsum(interarrival_times)

    service_start_times = np.zeros(n_customers)
    service_end_times = np.zeros(n_customers)
    waiting_times = np.zeros(n_customers)

    for i in range(n_customers):
        if i == 0:
            service_start_times[i] = arrival_times[i]
        else:
            service_start_times[i] = max(arrival_times[i], service_end_times[i - 1])

        service_end_times[i] = service_start_times[i] + service_times[i]
        waiting_times[i] = service_start_times[i] - arrival_times[i]

    print("\n--- 18. Simple stochastic model: queue simulation ---")
    print("Customer | Arrival | Service start | Service end | Waiting time")

    for i in range(n_customers):
        print(
            f"{i + 1:8d} | "
            f"{arrival_times[i]:7.2f} | "
            f"{service_start_times[i]:13.2f} | "
            f"{service_end_times[i]:11.2f} | "
            f"{waiting_times[i]:12.2f}"
        )

    print(f"\nAverage waiting time ≈ {np.mean(waiting_times):.4f}")

    return {
        "arrival_times": arrival_times,
        "service_start_times": service_start_times,
        "service_end_times": service_end_times,
        "waiting_times": waiting_times,
    }


def test_demonstrations():
    """
    Simple test function for the stochastic simulation demo.

    The tests use the outputs returned by each function and check:
    - shapes
    - probability sums
    - approximate theoretical values
    - dictionary keys
    - variance/covariance identities

    These are not strict unit tests because many functions are simulation-based.
    Therefore, we use tolerances.
    """

    print("\n" + "=" * 70)
    print("Running tests")
    print("=" * 70)

    # ------------------------------------------------------------
    # 1. Random number generation
    # ------------------------------------------------------------
    uniform_numbers, normal_numbers, integer_numbers = demonstrate_random_number_generation(
        seed=42,
        n=10
    )

    assert len(uniform_numbers) == 10
    assert len(normal_numbers) == 10
    assert len(integer_numbers) == 10

    assert np.all(uniform_numbers >= 0)
    assert np.all(uniform_numbers <= 1)

    assert np.all(integer_numbers >= 1)
    assert np.all(integer_numbers <= 6)

    print("Test 1 passed: random number generation")

    # ------------------------------------------------------------
    # 2. Dice simulation
    # ------------------------------------------------------------
    rolls, empirical_probabilities = simulate_dice_experiment(
        seed=42,
        n_rolls=100_000
    )

    assert len(rolls) == 100_000
    assert np.all(rolls >= 1)
    assert np.all(rolls <= 6)

    assert len(empirical_probabilities) == 6
    assert np.isclose(np.sum(empirical_probabilities), 1.0)

    # Each side should be close to 1/6
    assert np.all(np.abs(empirical_probabilities - 1 / 6) < 0.01)

    print("Test 2 passed: dice simulation")

    # ------------------------------------------------------------
    # 3. Basic probability rules
    # ------------------------------------------------------------
    probability_results = demonstrate_basic_probability_rules(
        seed=42,
        n=100_000
    )

    p_A = probability_results["P(A)"]
    p_A_complement = probability_results["P(A_complement)"]
    p_B = probability_results["P(B)"]
    p_A_and_B = probability_results["P(A_and_B)"]
    p_A_or_B = probability_results["P(A_or_B)"]

    assert np.isclose(p_A + p_A_complement, 1.0, atol=0.01)

    inclusion_exclusion = p_A + p_B - p_A_and_B
    assert np.isclose(p_A_or_B, inclusion_exclusion, atol=0.01)

    print("Test 3 passed: basic probability rules")

    # ------------------------------------------------------------
    # 4. Conditional probability
    # ------------------------------------------------------------
    p_A_given_B = demonstrate_conditional_probability(
        seed=42,
        n=100_000
    )

    assert 0 <= p_A_given_B <= 1

    # Theoretical value:
    # A = even = {2, 4, 6}
    # B = greater than 3 = {4, 5, 6}
    # A and B = {4, 6}
    # P(A | B) = 2 / 3
    assert np.isclose(p_A_given_B, 2 / 3, atol=0.02)

    print("Test 4 passed: conditional probability")

    # ------------------------------------------------------------
    # 5. Law of total probability
    # ------------------------------------------------------------
    empirical_p_defective, theoretical_p_defective = demonstrate_law_of_total_probability(
        seed=42,
        n=100_000
    )

    assert 0 <= empirical_p_defective <= 1
    assert 0 <= theoretical_p_defective <= 1
    assert np.isclose(empirical_p_defective, theoretical_p_defective, atol=0.005)

    print("Test 5 passed: law of total probability")

    # ------------------------------------------------------------
    # 6. Bayes' theorem
    # ------------------------------------------------------------
    p_disease_given_positive = demonstrate_bayes_theorem()

    assert 0 <= p_disease_given_positive <= 1

    # Expected value:
    # numerator = 0.95 * 0.01 = 0.0095
    # denominator = 0.95 * 0.01 + 0.05 * 0.99 = 0.059
    # result = 0.0095 / 0.059
    expected_bayes_value = 0.0095 / 0.059

    assert np.isclose(p_disease_given_positive, expected_bayes_value)

    print("Test 6 passed: Bayes' theorem")

    # ------------------------------------------------------------
    # 7. Independence of events
    # ------------------------------------------------------------
    independence_results = demonstrate_event_independence(
        seed=42,
        n=100_000
    )

    assert "coin_independence_difference" in independence_results
    assert "die_independence_difference" in independence_results

    # The coin flips should be approximately independent
    assert independence_results["coin_independence_difference"] < 0.01

    print("Test 7 passed: event independence")

    # ------------------------------------------------------------
    # 8. Random variables
    # ------------------------------------------------------------
    X = demonstrate_random_variable(seed=42, n=20)

    assert len(X) == 20
    assert np.all(X >= 0)
    assert np.all(X <= 2)

    print("Test 8 passed: random variable")

    # ------------------------------------------------------------
    # 9. Discrete distribution
    # ------------------------------------------------------------
    possible_values, pmf, cdf = demonstrate_discrete_distribution(
        seed=42,
        n=100_000
    )

    assert np.array_equal(possible_values, np.array([0, 1, 2, 3]))
    assert np.isclose(np.sum(pmf), 1.0, atol=0.01)

    # CDF should be increasing
    assert np.all(np.diff(cdf) >= 0)

    # Last CDF value should be 1
    assert np.isclose(cdf[-1], 1.0, atol=0.01)

    print("Test 9 passed: discrete distribution")

    # ------------------------------------------------------------
    # 10. Continuous distribution
    # ------------------------------------------------------------
    X_normal = demonstrate_continuous_distribution(
        seed=42,
        n=100_000
    )

    assert len(X_normal) == 100_000

    # For a standard normal, mean should be close to 0
    assert np.isclose(np.mean(X_normal), 0, atol=0.02)

    # Variance should be close to 1
    assert np.isclose(np.var(X_normal), 1, atol=0.05)

    print("Test 10 passed: continuous distribution")

    # ------------------------------------------------------------
    # 11. Expectation
    # ------------------------------------------------------------
    empirical_expectation, theoretical_expectation = demonstrate_expectation(
        seed=42,
        n=100_000
    )

    assert np.isclose(theoretical_expectation, 3.5)
    assert np.isclose(empirical_expectation, theoretical_expectation, atol=0.02)

    print("Test 11 passed: expectation")

    # ------------------------------------------------------------
    # 12. Expectation of a function
    # ------------------------------------------------------------
    empirical_E_X2, theoretical_E_X2 = demonstrate_expectation_of_function(
        seed=42,
        n=100_000
    )

    assert np.isclose(empirical_E_X2, theoretical_E_X2, atol=0.1)

    print("Test 12 passed: expectation of a function")

    # ------------------------------------------------------------
    # 13. Moments and variance
    # ------------------------------------------------------------
    empirical_variance, theoretical_variance = demonstrate_moments_and_variance(
        seed=42,
        n=100_000
    )

    assert np.isclose(empirical_variance, theoretical_variance, atol=0.05)

    print("Test 13 passed: moments and variance")

    # ------------------------------------------------------------
    # 14. Joint, marginal, and conditional distributions
    # ------------------------------------------------------------
    marginal_X, conditional_X_given_Y = demonstrate_joint_marginal_conditional_discrete(
        seed=42,
        n=100_000
    )

    assert np.isclose(sum(marginal_X.values()), 1.0, atol=0.01)
    assert np.isclose(sum(conditional_X_given_Y.values()), 1.0, atol=0.01)

    print("Test 14 passed: joint, marginal, and conditional distributions")

    # ------------------------------------------------------------
    # 15. Covariance and correlation
    # ------------------------------------------------------------
    covariance, correlation = demonstrate_covariance_and_correlation(
        seed=42,
        n=100_000
    )

    # Since Y = 2X + noise, covariance and correlation should be positive
    assert covariance > 0
    assert correlation > 0

    print("Test 15 passed: covariance and correlation")

    # ------------------------------------------------------------
    # 16. Variance of sums
    # ------------------------------------------------------------
    variance_sum_results = demonstrate_variance_of_sum(
        seed=42,
        n=100_000
    )

    assert np.isclose(
        variance_sum_results["independent_direct"],
        variance_sum_results["independent_formula"],
        atol=0.0001
    )

    assert np.isclose(
        variance_sum_results["correlated_direct"],
        variance_sum_results["correlated_formula"],
        atol=0.0001
    )

    print("Test 16 passed: variance of sums")

    # ------------------------------------------------------------
    # 17. Independent random variables
    # ------------------------------------------------------------
    E_XY, E_X_times_E_Y, covariance, correlation = demonstrate_independent_random_variables(
        seed=42,
        n=100_000
    )

    assert np.isclose(E_XY, E_X_times_E_Y, atol=0.01)
    assert np.isclose(covariance, 0, atol=0.01)
    assert np.isclose(correlation, 0, atol=0.01)

    print("Test 17 passed: independent random variables")

    # ------------------------------------------------------------
    # 18. Queue simulation
    # ------------------------------------------------------------
    queue_results = simulate_simple_queue(
        seed=42,
        n_customers=20
    )

    arrival_times = queue_results["arrival_times"]
    service_start_times = queue_results["service_start_times"]
    service_end_times = queue_results["service_end_times"]
    waiting_times = queue_results["waiting_times"]

    assert len(arrival_times) == 20
    assert len(service_start_times) == 20
    assert len(service_end_times) == 20
    assert len(waiting_times) == 20

    # Arrival times should be increasing
    assert np.all(np.diff(arrival_times) > 0)

    # Service cannot start before arrival
    assert np.all(service_start_times >= arrival_times)

    # Service end must be after service start
    assert np.all(service_end_times >= service_start_times)

    # Waiting times must be non-negative
    assert np.all(waiting_times >= 0)

    print("Test 18 passed: simple queue simulation")

    print("\n" + "=" * 70)
    print("All tests passed")
    print("=" * 70)

def main():

    print("=" * 70)
    print("02443 Stochastic Simulation - Introduction Demonstration")
    print("=" * 70)

    demonstrate_random_number_generation()
    simulate_dice_experiment()
    demonstrate_basic_probability_rules()
    demonstrate_conditional_probability()
    demonstrate_law_of_total_probability()
    demonstrate_bayes_theorem()
    demonstrate_event_independence()
    demonstrate_random_variable()
    demonstrate_discrete_distribution()
    demonstrate_continuous_distribution()
    demonstrate_expectation()
    demonstrate_expectation_of_function()
    demonstrate_moments_and_variance()
    demonstrate_joint_marginal_conditional_discrete()
    demonstrate_covariance_and_correlation()
    demonstrate_variance_of_sum()
    demonstrate_independent_random_variables()
    simulate_simple_queue()

    test_demonstrations()

    print("\n" + "=" * 70)
    print("Finished all stochastic simulation demonstrations.")
    print("=" * 70)

if __name__ == "__main__":
    main()