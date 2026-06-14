import numpy as np
from scipy.stats import norm, t
from scipy.optimize import minimize_scalar
from scipy.stats import chisquare
import matplotlib.pyplot as plt
import math
from mpl_toolkits.mplot3d import Axes3D
np.random.seed(42) 

################################ --------------- Exercise 5-1 --------------- ################################

'''
The number of busy lines in a trunk group (Erlang system) follows a truncated Poisson
distribution
    P(i) = c * (A^i / i!), i = 0, 1, ..., m
# Generate samples from this distribution using the Metropolis-Hastings algorithm.
'''

def truncated_poisson_pmf(i, A, m):
    c = 1 / sum((A**k) / math.factorial(k) for k in range(m + 1))
    return c * (A**i / math.factorial(i))

def metropolis_hastings_truncated_poisson(A, m, num_samples):
    """Generate samples from a truncated Poisson target distribution using Metropolis-Hastings.

    Suppose we wish to sample from a target density f(x) = c g(x), where the normalizing constant
    c is unknown. Starting from the current state x:
      1. Propose a candidate state y.
      2. Decide whether to accept or reject the proposal.
      3. If accepted, move to y.
      4. Otherwise remain at x.

    Repeating this procedure generates a Markov chain whose stationary distribution is the target.
    """
    samples = []
    current_state = 0  # Start at state 0

    for _ in range(num_samples):
        # 1. Propose a candidate state y
        proposed_state = np.random.randint(0, m + 1)

        # 2. Decide whether to accept or reject the proposal
        current_pmf = truncated_poisson_pmf(current_state, A, m)
        proposed_pmf = truncated_poisson_pmf(proposed_state, A, m)
        acceptance_prob = min(1, proposed_pmf / current_pmf)

        # 3. If accepted, move to y; otherwise remain at x
        if np.random.rand() < acceptance_prob:
            current_state = proposed_state

        samples.append(current_state)

    return samples

def chi_squared_test(samples, expected_freq):
    observed_freq = np.bincount(samples, minlength=len(expected_freq))
    chi2_statistic, p_value = chisquare(observed_freq, f_exp=expected_freq)
    return chi2_statistic, p_value

def plot_running_mean(samples):
    samples = np.array(samples)

    running_mean = np.cumsum(samples) / np.arange(1, len(samples) + 1)

    plt.figure(figsize=(10, 5))
    plt.plot(running_mean)
    plt.xlabel("Iteration")
    plt.ylabel("Running mean")
    plt.title("Running mean of Metropolis-Hastings samples")
    plt.grid(True)
    plt.show()

def truncated_poisson_mean(A, m):
    probs = np.array([
        truncated_poisson_pmf(i, A, m)
        for i in range(m + 1)
    ])

    states = np.arange(m + 1)

    return np.sum(states * probs)

def plot_running_mean(samples, A, m, filename='Assets/51_running_mean.png'):
    samples = np.array(samples)

    running_mean = np.cumsum(samples) / np.arange(1, len(samples) + 1)
    theoretical_mean = truncated_poisson_mean(A, m)

    plt.figure(figsize=(10, 5))
    plt.plot(running_mean, label="Running mean")
    plt.axhline(theoretical_mean, linestyle="--", label="Theoretical mean")
    plt.xlabel("Iteration")
    plt.ylabel("Mean")
    plt.title("Running mean compared with theoretical mean")
    plt.legend()
    plt.grid(True)
    plt.savefig(filename)

def plot_expected_vs_observed(samples, expected_freq, filename='Assets/51_expected_vs_observed.png'):
    # Sample are a list of 8000 integers between 0 and 5
    # expected_freq is a list of 6 floats representing the expected frequencies for each state (0 to 5)
    observed_freq = np.bincount(samples, minlength=len(expected_freq))
    states = np.arange(len(expected_freq))
    plt.figure(figsize=(10, 5))
    plt.bar(states - 0.2, observed_freq, width=0.4, label="Observed", alpha=0.7)
    plt.bar(states + 0.2, expected_freq, width=0.4, label="Expected", alpha=0.7)
    plt.xlabel("State")
    plt.ylabel("Frequency")
    plt.title("Observed vs Expected Frequencies")
    plt.xticks(states)
    plt.legend()
    plt.grid(True)
    plt.savefig(filename)


################################ --------------- Exercise 5-2 --------------- ################################

def valid_states_2d(m):
    return [
        (i, j)
        for i in range(m + 1)
        for j in range(m + 1 - i)
    ]


def unnormalized_pmf_2d(i, j, A_1, A_2):
    return (A_1**i / math.factorial(i)) * (A_2**j / math.factorial(j))


def truncated_poisson_pmf_2d(i, j, A_1, A_2, m):
    normalizing_sum = sum(
        unnormalized_pmf_2d(k, l, A_1, A_2)
        for k, l in valid_states_2d(m)
    )

    c = 1 / normalizing_sum

    return c * unnormalized_pmf_2d(i, j, A_1, A_2)


# ---------------- Method (a): Direct Metropolis-Hastings ----------------

def metropolis_hastings_2d_direct(A_1, A_2, m, num_samples, start_state=(0, 0)):
    samples = []
    valid_states = valid_states_2d(m)

    current_state = start_state

    for _ in range(num_samples):
        proposed_state = valid_states[np.random.randint(len(valid_states))]

        current_weight = unnormalized_pmf_2d(
            current_state[0], current_state[1], A_1, A_2
        )

        proposed_weight = unnormalized_pmf_2d(
            proposed_state[0], proposed_state[1], A_1, A_2
        )

        acceptance_prob = min(1, proposed_weight / current_weight)

        if np.random.rand() < acceptance_prob:
            current_state = proposed_state

        samples.append(current_state)

    return np.array(samples)


# ---------------- Method (b): Coordinate-wise Metropolis-Hastings ----------------

def metropolis_hastings_2d_coordinatewise(A_1, A_2, m, num_samples, start_state=(0, 0)):
    samples = []

    current_i, current_j = start_state

    for _ in range(num_samples):
        # Update i while keeping j fixed
        proposed_i = np.random.randint(0, m - current_j + 1)

        current_weight = unnormalized_pmf_2d(
            current_i, current_j, A_1, A_2
        )

        proposed_weight = unnormalized_pmf_2d(
            proposed_i, current_j, A_1, A_2
        )

        acceptance_prob = min(1, proposed_weight / current_weight)

        if np.random.rand() < acceptance_prob:
            current_i = proposed_i

        # Update j while keeping i fixed
        proposed_j = np.random.randint(0, m - current_i + 1)

        current_weight = unnormalized_pmf_2d(
            current_i, current_j, A_1, A_2
        )

        proposed_weight = unnormalized_pmf_2d(
            current_i, proposed_j, A_1, A_2
        )

        acceptance_prob = min(1, proposed_weight / current_weight)

        if np.random.rand() < acceptance_prob:
            current_j = proposed_j

        samples.append((current_i, current_j))

    return np.array(samples)


# ---------------- Method (c): Gibbs sampling ----------------

def sample_from_discrete_probs(values, probs):
    probs = np.array(probs, dtype=float)
    probs = probs / np.sum(probs)

    return np.random.choice(values, p=probs)


def gibbs_sampling_2d(A_1, A_2, m, num_samples, start_state=(0, 0)):
    """
    Gibbs sampler using the analytical conditional distributions:

        P(i | j) proportional to A_1^i / i!,  i = 0, ..., m-j

        P(j | i) proportional to A_2^j / j!,  j = 0, ..., m-i
    """

    samples = []

    current_i, current_j = start_state

    for _ in range(num_samples):
        # Sample i from P(i | j)
        possible_i_values = np.arange(m - current_j + 1)

        probs_i_given_j = np.array([
            A_1**i / math.factorial(i)
            for i in possible_i_values
        ])

        current_i = sample_from_discrete_probs(
            possible_i_values,
            probs_i_given_j
        )

        # Sample j from P(j | i)
        possible_j_values = np.arange(m - current_i + 1)

        probs_j_given_i = np.array([
            A_2**j / math.factorial(j)
            for j in possible_j_values
        ])

        current_j = sample_from_discrete_probs(
            possible_j_values,
            probs_j_given_i
        )

        samples.append((current_i, current_j))

    return np.array(samples)

def chi_squared_test_2d(samples, A_1, A_2, m):
    valid_states = valid_states_2d(m)

    observed_freq = np.array([
        np.sum((samples[:, 0] == i) & (samples[:, 1] == j))
        for i, j in valid_states
    ])

    expected_freq = np.array([
        truncated_poisson_pmf_2d(i, j, A_1, A_2, m) * len(samples)
        for i, j in valid_states
    ])

    chi2_statistic, p_value = chisquare(
        f_obs=observed_freq,
        f_exp=expected_freq
    )

    return chi2_statistic, p_value, observed_freq, expected_freq

def plot_3d_distribution(samples, A_1, A_2, m, filename="Assets/52_3d_distribution.png"):
    valid_states = valid_states_2d(m)

    empirical_probs = []
    theoretical_probs = []

    for i, j in valid_states:
        observed_count = np.sum((samples[:, 0] == i) & (samples[:, 1] == j))
        empirical_probs.append(observed_count / len(samples))

        theoretical_probs.append(
            truncated_poisson_pmf_2d(i, j, A_1, A_2, m)
        )

    empirical_probs = np.array(empirical_probs)
    theoretical_probs = np.array(theoretical_probs)

    x = np.array([state[0] for state in valid_states])
    y = np.array([state[1] for state in valid_states])

    fig = plt.figure(figsize=(12, 6))

    ax = fig.add_subplot(111, projection="3d")

    width = 0.3
    depth = 0.3

    # Empirical bars
    ax.bar3d(
        x - 0.15,
        y - 0.15,
        np.zeros_like(empirical_probs),
        width,
        depth,
        empirical_probs,
        alpha=0.7,
        label="Empirical"
    )

    # Theoretical bars
    ax.bar3d(
        x + 0.15,
        y + 0.15,
        np.zeros_like(theoretical_probs),
        width,
        depth,
        theoretical_probs,
        alpha=0.7,
        label="Theoretical"
    )

    ax.set_xlabel("i: Call type 1")
    ax.set_ylabel("j: Call type 2")
    ax.set_zlabel("Probability")
    ax.set_title("Empirical vs theoretical 2D truncated Poisson distribution")

    plt.savefig(filename)
    plt.close()

################################ --------------- Exercise 5-3 --------------- ################################
def bivariate_lognormal_pdf(x, y, rho=0.5):
    """
    Joint density of (X, Y), where (log(X), log(Y)) follows
    a bivariate normal distribution with mean zero, variance one,
    and correlation rho.

    Valid only for x > 0 and y > 0.
    """

    if x <= 0 or y <= 0:
        return 0

    coefficient = 1 / (2 * np.pi * x * y * np.sqrt(1 - rho**2))

    exponent = -(
        np.log(x)**2
        - 2 * rho * np.log(x) * np.log(y)
        + np.log(y)**2
    ) / (2 * (1 - rho**2))

    return coefficient * np.exp(exponent)

def generate_prior_sample(rho=0.5):
    mean = np.array([0, 0])

    covariance = np.array([
        [1, rho],
        [rho, 1]
    ])

    xi, gamma = np.random.multivariate_normal(mean, covariance)

    theta = np.exp(xi)
    psi = np.exp(gamma)

    return xi, gamma, theta, psi

def simulate_observations(theta, psi, n=10):
    return np.random.normal(
        loc=theta,
        scale=np.sqrt(psi),
        size=n
    )

def plot_x_values(samples, filename="Assets/53_x_values.png"):
    samples = np.array(samples)

    plt.figure(figsize=(10, 5))

    plt.plot(
        np.arange(1, len(samples) + 1),
        samples,
        marker="o",
        linestyle="-",
        label="Simulated observations"
    )

    plt.xlabel("Observation number")
    plt.ylabel("X value")
    plt.title("Simulated observations $X_1, \\ldots, X_n$")
    plt.grid(True)
    plt.legend()
    plt.savefig(filename)
    plt.close()


def log_posterior_xi_gamma(xi, gamma, observations, rho=0.5):
    """
    Log posterior of (xi, gamma), where:
        theta = exp(xi)
        psi   = exp(gamma)

    We sample in log-space to ensure theta > 0 and psi > 0.
    Constants are omitted because they cancel in the MH ratio.
    """

    observations = np.array(observations)
    n = len(observations)

    theta = np.exp(xi)
    psi = np.exp(gamma)

    # Log-likelihood: Xi ~ N(theta, psi)
    sum_squared_errors = np.sum((observations - theta) ** 2)

    log_likelihood = (
        -(n / 2) * np.log(psi)
        - sum_squared_errors / (2 * psi)
    )

    # Prior for (xi, gamma): bivariate normal with correlation rho
    log_prior = -(
        xi**2
        - 2 * rho * xi * gamma
        + gamma**2
    ) / (2 * (1 - rho**2))

    return log_likelihood + log_prior

def metropolis_hastings_posterior(
    observations,
    num_samples=10000,
    rho=0.5,
    proposal_std=0.25,
    start_xi=0,
    start_gamma=0
):
    """
    Generate posterior samples of (theta, psi) using Metropolis-Hastings.

    The chain is run in log-space:
        xi = log(theta)
        gamma = log(psi)
    """

    samples = []

    current_xi = start_xi
    current_gamma = start_gamma

    current_log_posterior = log_posterior_xi_gamma(
        current_xi,
        current_gamma,
        observations,
        rho
    )

    accepted = 0

    for _ in range(num_samples):
        # Random-walk proposal in log-space
        proposed_xi = np.random.normal(current_xi, proposal_std)
        proposed_gamma = np.random.normal(current_gamma, proposal_std)

        proposed_log_posterior = log_posterior_xi_gamma(
            proposed_xi,
            proposed_gamma,
            observations,
            rho
        )

        # MH acceptance probability on log-scale
        log_acceptance_ratio = proposed_log_posterior - current_log_posterior

        if np.log(np.random.rand()) < log_acceptance_ratio:
            current_xi = proposed_xi
            current_gamma = proposed_gamma
            current_log_posterior = proposed_log_posterior
            accepted += 1

        theta = np.exp(current_xi)
        psi = np.exp(current_gamma)

        samples.append((theta, psi))

    acceptance_rate = accepted / num_samples

    return np.array(samples), acceptance_rate

def plot_posterior_traces(samples, burn_in=2000, filename="Assets/53_posterior_traces.png"):
    samples = np.array(samples)

    theta_samples = samples[:, 0]
    psi_samples = samples[:, 1]

    plt.figure(figsize=(10, 6))

    plt.plot(theta_samples, label=r"$\Theta$")
    plt.plot(psi_samples, label=r"$\Psi$")

    plt.axvline(burn_in, linestyle="--", label="Burn-in")

    plt.xlabel("Iteration")
    plt.ylabel("Parameter value")
    plt.title("Posterior trace plots for $\\Theta$ and $\\Psi$")
    plt.legend()
    plt.grid(True)

    plt.savefig(filename)
    plt.close()

def plot_posterior_histograms(samples, filename="Assets/53_posterior_histograms.png"):
    samples = np.array(samples)

    theta_samples = samples[:, 0]
    psi_samples = samples[:, 1]

    plt.figure(figsize=(10, 5))

    plt.hist(theta_samples, bins=40, alpha=0.6, density=True, label=r"$\Theta$")
    plt.hist(psi_samples, bins=40, alpha=0.6, density=True, label=r"$\Psi$")

    plt.xlabel("Parameter value")
    plt.ylabel("Density")
    plt.title("Posterior samples of $\\Theta$ and $\\Psi$")
    plt.legend()
    plt.grid(True)

    plt.savefig(filename)
    plt.close()

def run_posterior_experiment(
    theta,
    psi,
    n,
    rho=0.5,
    num_samples=10000,
    burn_in=2000,
    proposal_std=0.25
):
    print(f"\n##### ----- Posterior sampling with n = {n} ----- #####")

    # Generate observations using the SAME theta and psi from part (a)
    X = simulate_observations(theta, psi, n)

    posterior_samples, acceptance_rate = metropolis_hastings_posterior(
        observations=X,
        num_samples=num_samples,
        rho=rho,
        proposal_std=proposal_std
    )

    posterior_samples_after_burn_in = posterior_samples[burn_in:]

    theta_samples = posterior_samples_after_burn_in[:, 0]
    psi_samples = posterior_samples_after_burn_in[:, 1]

    print(f"True theta from prior: {theta:.4f}")
    print(f"True psi from prior:   {psi:.4f}")

    print(f"Sample mean of X:      {np.mean(X):.4f}")
    print(f"Sample variance of X:  {np.var(X, ddof=1):.4f}")

    print(f"Acceptance rate:       {acceptance_rate:.4f}")

    print("\nPosterior mean estimates:")
    print(f"E[Theta | X] ≈ {np.mean(theta_samples):.4f}")
    print(f"E[Psi | X] ≈   {np.mean(psi_samples):.4f}")

    print("\nPosterior median estimates:")
    print(f"Median Theta ≈ {np.median(theta_samples):.4f}")
    print(f"Median Psi ≈   {np.median(psi_samples):.4f}")

    print("\n95% posterior intervals:")
    print(f"Theta: [{np.percentile(theta_samples, 2.5):.4f}, {np.percentile(theta_samples, 97.5):.4f}]")
    print(f"Psi:   [{np.percentile(psi_samples, 2.5):.4f}, {np.percentile(psi_samples, 97.5):.4f}]")

    plot_posterior_traces(
        samples=posterior_samples,
        burn_in=burn_in,
        filename=f"Assets/53d_traces_n{n}.png"
    )

    plot_posterior_histograms(
        samples=posterior_samples_after_burn_in,
        filename=f"Assets/53d_histogram_n{n}.png"
    )

    return X, posterior_samples_after_burn_in, acceptance_rate

################################ --------------- Main --------------- ################################
def main():
    ################ ----- Exercise 5-1 ----- ################
    print("Exercise 5-1: Metropolis-Hastings for truncated Poisson distribution")
    A = 3  # Average number of busy lines
    m = 5  # Maximum number of lines
    burn_in = 2000  # Number of initial samples to discard
    num_samples = 10000
    thin = 10  # chi-sqared test requires independent samples, so we thin the chain by taking every 10th sample after burn-in
    samples = metropolis_hastings_truncated_poisson(
        A=A,
        m=m,
        num_samples=num_samples
    )

    plot_running_mean(samples, A, m)

    # Discard burn-in
    samples = np.array(samples)
    samples_after_burn_in = samples[burn_in::thin]

    expected_freq = np.array([
        truncated_poisson_pmf(i, A, m) * len(samples_after_burn_in)
        for i in range(m + 1)
    ])
    print("\nExpected frequencies:",)
    print(samples_after_burn_in[:10], samples_after_burn_in.shape)
    print(expected_freq[:10], expected_freq.shape)


    plot_expected_vs_observed(samples_after_burn_in, expected_freq)

    chi2_statistic, p_value = chi_squared_test(
        samples_after_burn_in,
        expected_freq
    )

    print(f"Chi-squared statistic: {chi2_statistic:.4f}, p-value: {p_value:.4f}")

    if p_value > 0.05:
        print("The samples follow the expected distribution (fail to reject H0).")
    else:
        print("The samples do not follow the expected distribution (reject H0).")

    ################ ----- Exercise 5-2 ----- ################
    print("\n##### ----- Exercise 5-2: Truncated Poisson distribution ----- #####")
    A_1 = 4
    A_2 = 4
    m = 10
    num_samples = 10000
    burn_in = 2000
    thin = 5

    methods = {
        "Direct Metropolis-Hastings": metropolis_hastings_2d_direct,
        "Coordinate-wise Metropolis-Hastings": metropolis_hastings_2d_coordinatewise,
        "Gibbs sampling": gibbs_sampling_2d
    }

    print("\n##### ----- Exercise 5-2: Comparison of 2D sampling methods ----- #####")

    for method_name, sampler in methods.items():
        samples = sampler(
            A_1=A_1,
            A_2=A_2,
            m=m,
            num_samples=num_samples
        )

        samples_after_burn_in = samples[burn_in::thin]

        chi2_statistic, p_value, observed_freq, expected_freq = chi_squared_test_2d(
            samples=samples_after_burn_in,
            A_1=A_1,
            A_2=A_2,
            m=m
        )

        print(f"\nMethod: {method_name}")
        print(f"Number of samples after burn-in and thinning: {len(samples_after_burn_in)}")
        print(f"Chi-squared statistic: {chi2_statistic:.4f}")
        print(f"p-value: {p_value:.4f}")

        if p_value > 0.05:
            print("Result: Fail to reject H0. The samples are consistent with the target distribution.")
        else:
            print("Result: Reject H0. The samples differ significantly from the target distribution.")

        plot_3d_distribution(
            samples=samples_after_burn_in,
            A_1=A_1,
            A_2=A_2,
            m=m,
            filename=f"Assets/52_{method_name}_3d_plot"
        )

    ################ ----- Exercise 5-3 ----- ################
    print("\n##### ----- Exercise 5-3-b: Bayesia model")
    print("Test: ", bivariate_lognormal_pdf(2,2, rho = 0.5))
    '''
        We know that Xi and Gamma is normal distributed and have correlation Phi  
            Xi ~ N(0,1)
            Gamma ~ N(0,1)
            corr(Xi, Gamma) = Phi
            st. Cov(Xi, Gamma) = [[1, Phi], [Phi, 1]]

        Then because, we have
            (Xi, Gamma) = (log(Theta), log(Psi)) =>
            (Theta, psi) = e^Xi, e^Gamma
    '''
    rho = 0.5
    n = 10

    xi, gamma, theta, psi = generate_prior_sample(rho)

    X = simulate_observations(theta, psi, n)

    print("\nGenerate N sample")
    print(f"xi = {xi:.4f}")
    print(f"gamma = {gamma:.4f}")
    print(f"theta = {theta:.4f}")
    print(f"psi = {psi:.4f}")
    print(X)

    plot_x_values(X, "Assets/53_x_values.png")    

    print("\n##### ----- Exercise 5-3-c: Bayesia model")
    # Look at notes

    print("\n##### ----- Exercise 5-3-d: Bayesia model")
    rho = 0.5
    num_samples = 10000
    burn_in = 2000

    posterior_samples, acceptance_rate = metropolis_hastings_posterior(
        observations=X,
        num_samples=num_samples,
        rho=rho,
        proposal_std=0.25
    )

    posterior_samples_after_burn_in = posterior_samples[burn_in:]

    theta_samples = posterior_samples_after_burn_in[:, 0]
    psi_samples = posterior_samples_after_burn_in[:, 1]

    print(f"Acceptance rate: {acceptance_rate:.4f}")

    print("\nPosterior mean estimates:")
    print(f"E[Theta | X] ≈ {np.mean(theta_samples):.4f}")
    print(f"E[Psi | X] ≈ {np.mean(psi_samples):.4f}")

    print("\nPosterior median estimates:")
    print(f"Median Theta ≈ {np.median(theta_samples):.4f}")
    print(f"Median Psi ≈ {np.median(psi_samples):.4f}")
    
    plot_posterior_traces(
        samples=posterior_samples,
        burn_in=burn_in,
        filename="Assets/53d_traces"
    )
    plot_posterior_histograms(
        samples=posterior_samples_after_burn_in,
        filename="Assets/53d_histogram"
    )

    print("\n##### ----- Exercise 5-3-e: Bayesia model")

    rho = 0.5
    num_samples = 10000
    burn_in = 2000

    # Use different proposal standard deviations.
    # For larger n, the posterior becomes more concentrated, so smaller steps are usually better.
    proposal_stds = {
        10: 0.25,
        100: 0.10,
        1000: 0.03
    }

    posterior_results = {}

    for n_value in [10, 100, 1000]:
        X_n, posterior_samples_n, acceptance_rate_n = run_posterior_experiment(
            theta=theta,
            psi=psi,
            n=n_value,
            rho=rho,
            num_samples=num_samples,
            burn_in=burn_in,
            proposal_std=proposal_stds[n_value]
        )

        posterior_results[n_value] = {
            "X": X_n,
            "posterior_samples": posterior_samples_n,
            "acceptance_rate": acceptance_rate_n
        }



if __name__ == "__main__":
    main()