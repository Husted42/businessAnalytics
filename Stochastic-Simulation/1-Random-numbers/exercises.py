from scipy import stats
import numpy as np
import matplotlib.pyplot as plt

np.random.seed(42)

# ------------------------
# Task 1 a)
# ------------------------

def q1_gen_random_numbers(n,seed=42, a=1664525, c=1013904223, m=2**32):
    """Implement a linear congruential generator (LCG)"""
    # LCG parameters (example values)

    # Initialize the seed
    numbers = []

    for _ in range(n):
        seed = (a * seed + c) % m
        numbers.append(seed / m)

    return np.array(numbers)

def q1_gen_random_numbers_with_init(n, seed, a, c, m, x_0=0):
    """Generates random numbers using LCG with an initial seed"""
    numbers = []
    x = x_0
    for _ in range(n):
        x = (a * x + c) % m
        numbers.append(x)
    return np.array(numbers)

def q1_histogram(numbers, prefix=""):
    # Histogram 
    plt.clf()
    plt.hist(numbers, bins=10, edgecolor='black')
    plt.title('Histogram of Generated Random Numbers')
    plt.xlabel('Value')
    plt.ylabel('Frequency')
    plt.savefig(f"assets/{prefix}_q1_histogram.png", dpi=300, bbox_inches="tight")

def q1_scatter_plot(numbers, prefix=""):
    # Scatter plot
    plt.clf()
    plt.scatter(range(len(numbers)), numbers, s=10)
    plt.title('Scatter Plot of Generated Random Numbers')
    plt.xlabel('Index')
    plt.ylabel('Value')
    plt.savefig(f"assets/{prefix}_q1_scatter_plot.png", dpi=300, bbox_inches="tight")

# ------------------------
# Task 1 b)
# ------------------------
'''
Evaluate the quality of the generator by graphical
descriptive statistics (histogrammes, scatter plots) and
statistical tests - χ2,Kolmogorov-Smirnov, run-tests
preferably but not necessarily all 3, and correlation test for some h-value
'''

def q1_chi_square_test(numbers, low, high, bins):
    """Performs a chi-square test on the generated numbers"""
    observed, _ = np.histogram(numbers, bins=bins, range=(low, high))
    expected = [len(numbers) / bins] * bins
    chi2_statistic, p_value = stats.chisquare(observed, f_exp=expected)
    print(f"Chi-square statistic: {chi2_statistic}, p-value: {p_value}")

def q1_ks_test(numbers, low, high):
    """Performs a Kolmogorov-Smirnov test on the generated numbers"""
    d_statistic, p_value = stats.kstest(numbers, 'uniform', args=(low, high - low))
    print(f"KS statistic: {d_statistic}, p-value: {p_value}")

def q1_correlation_test(numbers, h):
    """Performs a correlation test for some h-value"""
    x = numbers[:-h]
    y = numbers[h:]
    correlation_coefficient = np.corrcoef(x, y)[0, 1]
    print(f"Correlation coefficient for h={h}: {correlation_coefficient}")

def q2_system_random_numbers(n):
    """Generates random numbers using the system's random number generator"""
    return np.random.rand(n)

def main():
    # Task 1a)
    numbers = q1_gen_random_numbers(n=10000, seed=42, a=15, c=5, m=2**32)
    q1_histogram(numbers, prefix="task1a")
    q1_scatter_plot(numbers, prefix="task1a")

    # Task 1b)
    # Chi-square test
    q1_chi_square_test(numbers, low=0, high=1, bins=10)
    # Kolmogorov-Smirnov testStatistical
    q1_ks_test(numbers, low=0, high=1)
    # Correlation test for h=1
    q1_correlation_test(numbers, h=1)
    # Correlation test for h=5
    q1_correlation_test(numbers, h=5)

    # Task 2b)
    a_values = [15, 23, 69]
    b_values = [5, 1, 420]
    M_values = [2**32, 10**8 + 1, 2**4]
    prefix_values = ["LCG1", "LCG2", "LCG3"]
    print("\nTesting different LCG parameters:")
    for i in range(len(a_values)):
        print(f"\nTesting LCG with parameters: a={a_values[i]}, c={b_values[i]}, m={M_values[i]}")
        numbers = q1_gen_random_numbers(n=10000, seed=42, a=a_values[i], c=b_values[i], m=M_values[i])
        q1_chi_square_test(numbers, low=0, high=1, bins=10)
        q1_ks_test(numbers, low=0, high=1)
        q1_correlation_test(numbers, h=1)
        q1_correlation_test(numbers, h=5)
        q1_histogram(numbers, prefix=prefix_values[i])

    # Task 3
    system_numbers = q2_system_random_numbers(n=10000)
    q1_histogram(system_numbers, prefix="system_random")
    q1_chi_square_test(system_numbers, low=0, high=1, bins=10)
    q1_ks_test(system_numbers, low=0, high=1)
    q1_correlation_test(system_numbers, h=1)
    q1_correlation_test(system_numbers, h=5)

    # Test task
    test_number = q1_gen_random_numbers_with_init(n=10, seed=42, a=5, c=1, m=16, x_0=3)
    print(test_number[:10])



if __name__ == "__main__":
    main()