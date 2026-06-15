import numpy as np
from scipy.stats import t
import matplotlib.pyplot as plt

from scipy.stats import norm
import numpy as np

import math


def erlang_b(m, A):
    numerator = A**m / math.factorial(m)
    denominator = sum(A**i / math.factorial(i) for i in range(m + 1))
    return numerator / denominator


#################### -------------------- Exercise 4.1 -------------------- ####################
def simulate_blocking_system_erlang(
    m=10,
    mean_service_time=8,
    n_customers=10_000,
    erlang_k=2,
    seed=None
):
    """
    Simulate a blocking system with:
        - m parallel servers
        - no waiting room
        - Erlang inter-arrival times with mean 1
        - exponential service times

    Returns:
        blocking_fraction : float
            Fraction of customers that were blocked.

        times : np.ndarray
            Arrival times of the customers.

        servers_in_use : np.ndarray
            Number of busy servers after each arrival.

        blocked_customers : int
            Total number of blocked customers.
    """

    rng = np.random.default_rng(seed)

    current_time = 0.0
    blocked_customers = 0

    # Stores the departure time of each currently busy server
    busy_until = []

    # Values used for the stairs plot
    times = []
    servers_in_use = []
    inter_arrival_times = []

    for _ in range(n_customers):

        # Generate next arrival time
        # Erlang(k, rate=k) has mean k / k = 1
        inter_arrival = rng.gamma(shape=erlang_k, scale=1 / erlang_k)
        inter_arrival_times.append(inter_arrival)
        current_time += inter_arrival

        # Remove customers whose service has finished
        busy_until = [
            departure_time
            for departure_time in busy_until
            if departure_time > current_time
        ]

        # Check whether a server is available
        if len(busy_until) >= m:
            blocked_customers += 1
        else:
            service_time = rng.exponential(scale=mean_service_time)
            departure_time = current_time + service_time
            busy_until.append(departure_time)

        # Save system state after this arrival
        times.append(current_time)
        servers_in_use.append(len(busy_until))

    blocking_fraction = blocked_customers / n_customers

    if np.mean(inter_arrival_times) > 1.5 or np.mean(inter_arrival_times) < 0.5:
        print("Warning: Mean inter-arrival time is not close to 1, check the Erlang generation.")

    return (
        blocking_fraction,
        np.array(times),
        np.array(servers_in_use),
        blocked_customers
    )


def plot_servers_in_use(times, servers_in_use, m=10, n_plot=300, save_path="Assets/4a_servers_in_use.png"):
    """
    Draw a stairs plot of how many servers are in use over time.

    Parameters
    ----------
    times : array-like
        Arrival times from the simulation.
    servers_in_use : array-like
        Number of busy servers after each arrival.
    m : int
        Total number of servers.
    n_plot : int
        Number of arrival events to plot.
    save_path : str
        Path to save the plot.
    """

    plt.figure(figsize=(12, 5))

    plt.step(
        times[:n_plot],
        servers_in_use[:n_plot],
        where="post",
        label="Servers in use"
    )

    plt.axhline(
        y=m,
        linestyle="--",
        label=f"Maximum servers = {m}"
    )

    plt.xlabel("Simulation time")
    plt.ylabel("Number of servers in use")
    plt.title("Number of Busy Servers Over Time")
    plt.ylim(0, m + 1)
    plt.grid(True)
    plt.legend()
    plt.savefig(save_path, dpi=300, bbox_inches="tight")
    plt.close()

def normal_confidence_interval(theta_hat_values, alpha=0.05):

    theta_hat_values = np.array(theta_hat_values)

    n = len(theta_hat_values)

    theta_bar = np.mean(theta_hat_values)
    S_theta = np.std(theta_hat_values, ddof=1)

    u_alpha_2 = norm.ppf(alpha / 2)
    u_1_alpha_2 = norm.ppf(1 - alpha / 2)

    lower = theta_bar + (S_theta / np.sqrt(n)) * u_alpha_2
    upper = theta_bar + (S_theta / np.sqrt(n)) * u_1_alpha_2

    return theta_bar, S_theta, lower, upper

def plot_confidence_intervals(
    k_values,
    theta_bars,
    lower_bounds,
    upper_bounds,
    save_path="Assets/4a_confidence_intervals.png"
):

    k_values = np.array(k_values)
    theta_bars = np.array(theta_bars)
    lower_bounds = np.array(lower_bounds)
    upper_bounds = np.array(upper_bounds)

    lower_errors = theta_bars - lower_bounds
    upper_errors = upper_bounds - theta_bars

    plt.figure(figsize=(10, 5))

    plt.errorbar(
        k_values,
        theta_bars,
        yerr=[lower_errors, upper_errors],
        fmt="o",
        capsize=5,
        label="95% confidence interval"
    )

    plt.xlabel("Erlang shape parameter k")
    plt.ylabel("Blocking fraction")
    plt.title("Blocking Fraction with 95% Confidence Intervals")
    plt.grid(True)
    plt.legend()

    plt.savefig(save_path, dpi=300, bbox_inches="tight")
    plt.close()

################### -------------------- Exercise 4.b) -------------------- ####################

def simulate_blocking_system_hyperexponential(
    m=10,
    mean_service_time=8,
    n_customers=10_000,
    p1=0.8,
    lambda1=0.8333,
    p2=0.2,
    lambda2=5.0,
    seed=None
):

    rng = np.random.default_rng(seed)

    current_time = 0.0
    blocked_customers = 0

    busy_until = []

    times = []
    servers_in_use = []
    inter_arrival_times = []

    for _ in range(n_customers):

        # Generate hyperexponential inter-arrival time
        U = rng.uniform()

        if U < p1:
            inter_arrival = rng.exponential(scale=1 / lambda1)
        else:
            inter_arrival = rng.exponential(scale=1 / lambda2)

        inter_arrival_times.append(inter_arrival)
        current_time += inter_arrival

        # Remove customers whose service has finished
        busy_until = [
            departure_time
            for departure_time in busy_until
            if departure_time > current_time
        ]

        # Check whether a server is available
        if len(busy_until) >= m:
            blocked_customers += 1
        else:
            service_time = rng.exponential(scale=mean_service_time)
            departure_time = current_time + service_time
            busy_until.append(departure_time)

        times.append(current_time)
        servers_in_use.append(len(busy_until))

    blocking_fraction = blocked_customers / n_customers

    mean_inter_arrival = np.mean(inter_arrival_times)

    if mean_inter_arrival > 1.5 or mean_inter_arrival < 0.5:
        print("Warning: Mean inter-arrival time is not close to 1.")

    return (
        blocking_fraction,
        np.array(times),
        np.array(servers_in_use),
        blocked_customers
    )

################### -------------------- Exercise 4.3) -------------------- ####################

def run_simulation(func = simulate_blocking_system_erlang, n_replications=10, **kwargs):
    blocking_fractions = []

    for i in range(n_replications):
        blocking_fraction, times, servers_in_use, blocked_customers = func(
            seed=kwargs.get("seed", 42) + i,
            **{k: v for k, v in kwargs.items() if k != "seed"}
        )

        blocking_fractions.append(blocking_fraction)

    blocking_fractions = np.array(blocking_fractions)

    mean_blocking_fraction = np.mean(blocking_fractions)

    return blocking_fractions, mean_blocking_fraction, times, servers_in_use, blocked_customers

def simulate_blocking_system_erlang_constant(
    m=10,
    mean_service_time=8,
    n_customers=10_000,
    erlang_k=2,
    seed=None
):
    """
    Simulate a blocking system with:
        - m parallel servers
        - no waiting room
        - Erlang inter-arrival times with mean 1
        - exponential service times

    Returns:
        blocking_fraction : float
            Fraction of customers that were blocked.

        times : np.ndarray
            Arrival times of the customers.

        servers_in_use : np.ndarray
            Number of busy servers after each arrival.

        blocked_customers : int
            Total number of blocked customers.
    """

    rng = np.random.default_rng(seed)

    current_time = 0.0
    blocked_customers = 0

    # Stores the departure time of each currently busy server
    busy_until = []

    # Values used for the stairs plot
    times = []
    servers_in_use = []
    inter_arrival_times = []

    for _ in range(n_customers):

        # Generate next arrival time
        # Erlang(k, rate=k) has mean k / k = 1
        inter_arrival = rng.gamma(shape=erlang_k, scale=1 / erlang_k)
        inter_arrival_times.append(inter_arrival)
        current_time += inter_arrival

        # Remove customers whose service has finished
        busy_until = [
            departure_time
            for departure_time in busy_until
            if departure_time > current_time
        ]

        # Check whether a server is available
        if len(busy_until) >= m:
            blocked_customers += 1
        else:
            service_time = mean_service_time
            departure_time = current_time + service_time
            busy_until.append(departure_time)

        # Save system state after this arrival
        times.append(current_time)
        servers_in_use.append(len(busy_until))

    blocking_fraction = blocked_customers / n_customers

    if np.mean(inter_arrival_times) > 1.5 or np.mean(inter_arrival_times) < 0.5:
        print("Warning: Mean inter-arrival time is not close to 1, check the Erlang generation.")

    return (
        blocking_fraction,
        np.array(times),
        np.array(servers_in_use),
        blocked_customers
    )

def simulate_blocking_system_erlang_patero_105(
    m=10,
    mean_service_time=8,
    n_customers=10_000,
    erlang_k=2,
    seed=None
):
    """
    Simulate a blocking system with:
        - m parallel servers
        - no waiting room
        - Erlang inter-arrival times with mean 1
        - exponential service times

    Returns:
        blocking_fraction : float
            Fraction of customers that were blocked.

        times : np.ndarray
            Arrival times of the customers.

        servers_in_use : np.ndarray
            Number of busy servers after each arrival.

        blocked_customers : int
            Total number of blocked customers.
    """

    rng = np.random.default_rng(seed)

    current_time = 0.0
    blocked_customers = 0

    # Stores the departure time of each currently busy server
    busy_until = []

    # Values used for the stairs plot
    times = []
    servers_in_use = []
    inter_arrival_times = []

    for _ in range(n_customers):

        # Generate next arrival time
        # Erlang(k, rate=k) has mean k / k = 1
        inter_arrival = rng.gamma(shape=erlang_k, scale=1 / erlang_k)
        inter_arrival_times.append(inter_arrival)
        current_time += inter_arrival

        # Remove customers whose service has finished
        busy_until = [
            departure_time
            for departure_time in busy_until
            if departure_time > current_time
        ]

        # Check whether a server is available
        if len(busy_until) >= m:
            blocked_customers += 1
        else:
            # Patero service time with  k = 1.05
            pareto_scale = mean_service_time * (1.05 - 1) / 1.05
            U = rng.uniform()
            service_time = pareto_scale * U ** (-1 / 1.05)
            departure_time = current_time + service_time
            busy_until.append(departure_time)

        # Save system state after this arrival
        times.append(current_time)
        servers_in_use.append(len(busy_until))

    blocking_fraction = blocked_customers / n_customers

    if np.mean(inter_arrival_times) > 1.5 or np.mean(inter_arrival_times) < 0.5:
        print("Warning: Mean inter-arrival time is not close to 1, check the Erlang generation.")

    return (
        blocking_fraction,
        np.array(times),
        np.array(servers_in_use),
        blocked_customers
    )



def simulate_blocking_system_erlang_patero_205(
    m=10,
    mean_service_time=8,
    n_customers=10_000,
    erlang_k=2,
    seed=None
):
    """
    Simulate a blocking system with:
        - m parallel servers
        - no waiting room
        - Erlang inter-arrival times with mean 1
        - exponential service times

    Returns:
        blocking_fraction : float
            Fraction of customers that were blocked.

        times : np.ndarray
            Arrival times of the customers.

        servers_in_use : np.ndarray
            Number of busy servers after each arrival.

        blocked_customers : int
            Total number of blocked customers.
    """

    rng = np.random.default_rng(seed)

    current_time = 0.0
    blocked_customers = 0

    # Stores the departure time of each currently busy server
    busy_until = []

    # Values used for the stairs plot
    times = []
    servers_in_use = []
    inter_arrival_times = []

    for _ in range(n_customers):

        # Generate next arrival time
        # Erlang(k, rate=k) has mean k / k = 1
        inter_arrival = rng.gamma(shape=erlang_k, scale=1 / erlang_k)
        inter_arrival_times.append(inter_arrival)
        current_time += inter_arrival

        # Remove customers whose service has finished
        busy_until = [
            departure_time
            for departure_time in busy_until
            if departure_time > current_time
        ]

        # Check whether a server is available
        if len(busy_until) >= m:
            blocked_customers += 1
        else:
            # Patero service time with  k = 2.05
            pareto_scale = mean_service_time * (2.05 - 1) / 2.05
            U = rng.uniform()
            service_time = pareto_scale * U ** (-1 / 2.05)
            departure_time = current_time + service_time
            busy_until.append(departure_time)

        # Save system state after this arrival
        times.append(current_time)
        servers_in_use.append(len(busy_until))

    blocking_fraction = blocked_customers / n_customers

    if np.mean(inter_arrival_times) > 1.5 or np.mean(inter_arrival_times) < 0.5:
        print("Warning: Mean inter-arrival time is not close to 1, check the Erlang generation.")

    return (
        blocking_fraction,
        np.array(times),
        np.array(servers_in_use),
        blocked_customers
    )

def simulate_blocking_system_erlang_gaussian(
    m=10,
    mean_service_time=8,
    n_customers=10_000,
    erlang_k=2,
    seed=None
):
    """
    Simulate a blocking system with:
        - m parallel servers
        - no waiting room
        - Erlang inter-arrival times with mean 1
        - exponential service times

    Returns:
        blocking_fraction : float
            Fraction of customers that were blocked.

        times : np.ndarray
            Arrival times of the customers.

        servers_in_use : np.ndarray
            Number of busy servers after each arrival.

        blocked_customers : int
            Total number of blocked customers.
    """

    rng = np.random.default_rng(seed)

    current_time = 0.0
    blocked_customers = 0

    # Stores the departure time of each currently busy server
    busy_until = []

    # Values used for the stairs plot
    times = []
    servers_in_use = []
    inter_arrival_times = []

    for _ in range(n_customers):

        # Generate next arrival time
        # Erlang(k, rate=k) has mean k / k = 1
        inter_arrival = rng.gamma(shape=erlang_k, scale=1 / erlang_k)
        inter_arrival_times.append(inter_arrival)
        current_time += inter_arrival

        # Remove customers whose service has finished
        busy_until = [
            departure_time
            for departure_time in busy_until
            if departure_time > current_time
        ]

        # Check whether a server is available
        if len(busy_until) >= m:
            blocked_customers += 1
        else:
            # gaussian service time with mean = mean_service_time and std = mean_service_time / 2
            service_time = rng.normal(loc=mean_service_time, scale=mean_service_time / 2)
            departure_time = current_time + service_time
            busy_until.append(departure_time)

        # Save system state after this arrival
        times.append(current_time)
        servers_in_use.append(len(busy_until))

    blocking_fraction = blocked_customers / n_customers

    if np.mean(inter_arrival_times) > 1.5 or np.mean(inter_arrival_times) < 0.5:
        print("Warning: Mean inter-arrival time is not close to 1, check the Erlang generation.")

    return (
        blocking_fraction,
        np.array(times),
        np.array(servers_in_use),
        blocked_customers
    )

def main():
    #################### -------------------- Exercise 4.1 -------------------- ####################
    m = 10
    mean_service_time = 8
    n_customers = 10_000
    n_replications = 10
    erlang_k = 1
    seed = 42
    arrivalrate = 1

    blocking_fractions = []

    for i in range(n_replications):
        blocking_fraction, times, servers_in_use, blocked_customers = simulate_blocking_system_erlang(
            m=m,
            mean_service_time=mean_service_time,
            n_customers=n_customers,
            erlang_k=erlang_k,
            seed=seed + i
        )

        blocking_fractions.append(blocking_fraction)

    blocking_fractions = np.array(blocking_fractions)

    mean_blocking_fraction = np.mean(blocking_fractions)

    print("Exercise 4.1")
    print("------------")
    print(f"Blocking fractions: {blocking_fractions}")
    print(f"Mean blocking fraction: {mean_blocking_fraction:.4f}")

    plot_servers_in_use(
        times,
        servers_in_use,
        m=m,
        n_plot=300,
        save_path="Assets/4a_servers_in_use.png"
    )

    # Exact Erlang B solution
    A = mean_service_time * arrivalrate
    exact_blocking_probability = erlang_b(m, A)
    print(f"Exact blocking probability: {exact_blocking_probability:.4f}")

    ################### -------------------- Exercise 4.2 a) -------------------- ####################
    erlang_k_values = list(np.arange(0.5, 10.5, 0.5))  # More k values for smoother plot

    print()
    print("Exercise 4.2 a)")
    print("---------------") 


    alpha = 0.05

    theta_bars = []
    lower_bounds = []
    upper_bounds = []

    for k in erlang_k_values:
        theta_hat_values = []

        for i in range(n_replications):
            blocking_fraction, _, _, _ = simulate_blocking_system_erlang(
                m=m,
                mean_service_time=mean_service_time,
                n_customers=n_customers,
                erlang_k=k,
                seed=seed + i
            )

            theta_hat_values.append(blocking_fraction)

        theta_hat_values = np.array(theta_hat_values)

        theta_bar, S_theta, lower, upper = normal_confidence_interval(
            theta_hat_values,
            alpha=alpha
        )

        theta_bars.append(theta_bar)
        lower_bounds.append(lower)
        upper_bounds.append(upper)

        print(f"Erlang k={k}")
        print(f"theta_hat_values = {theta_hat_values}")
        print(f"theta_bar = {theta_bar:.4f}")
        print(f"S_theta = {S_theta:.4f}")
        print(f"95% confidence interval: [{lower:.4f}, {upper:.4f}]")
        print()

    plot_confidence_intervals(
        k_values=erlang_k_values,
        theta_bars=theta_bars,
        lower_bounds=lower_bounds,
        upper_bounds=upper_bounds,
        save_path="Assets/4a_confidence_intervals.png"
    )
    ################### -------------------- Exercise 4.2 b) -------------------- ####################
    print("Exercise 4.2 b)")
    print("---------------")
    
    blocking_fractions = []
    for i in range(n_replications):
        blocking_fraction, times, servers_in_use, blocked_customers = simulate_blocking_system_hyperexponential(
            m=m,
            mean_service_time=mean_service_time,
            n_customers=n_customers,
            p1=0.8,
            lambda1=0.8333,
            p2=0.2,
            lambda2=5.0,
            seed=seed + i
        )

        blocking_fractions.append(blocking_fraction)

    blocking_fractions = np.array(blocking_fractions)

    mean_blocking_fraction = np.mean(blocking_fractions)

    plot_servers_in_use(
        times,
        servers_in_use,
        m=m,
        n_plot=300,
        save_path="Assets/4a_servers_in_use.png"
    )

    # Confidence interval for hyperexponential case
    theta_bar, S_theta, lower, upper = normal_confidence_interval(
        blocking_fractions,
        alpha=alpha
    )
    print(f"Mean blocking fraction: {mean_blocking_fraction:.4f}")
    print(f"95% confidence interval for hyperexponential case: [{lower:.4f}, {upper:.4f}]")

    ################### -------------------- Exercise 4.3) -------------------- ####################
    print("\n\nExercise 4.3")
    for func in [
            simulate_blocking_system_erlang_constant, 
            simulate_blocking_system_erlang_patero_105,
            simulate_blocking_system_erlang_patero_205,
            simulate_blocking_system_erlang_gaussian
        ]:
        blocking_fractions, mean_blocking_fraction, times, servers_in_use, blocked_customers = run_simulation(
            func=func,
            n_replications=n_replications,
            m=m,
            mean_service_time=mean_service_time,
            n_customers=n_customers,
            erlang_k=erlang_k,
            seed=seed
        )

        print(f"Results for {func.__name__}:")
        print(f"Blocking fractions: {blocking_fractions}")
        print(f"Mean blocking fraction: {mean_blocking_fraction:.4f}")
        print(f"Total blocked customers: {blocked_customers}")
        print()

        # Confidence interval for the blocking fraction
        theta_bar, S_theta, lower, upper = normal_confidence_interval(
            blocking_fractions,
            alpha=alpha
        )
        print(f"95% confidence interval for {func.__name__}: [{lower:.4f}, {upper:.4f}]")
        print()

if __name__ == "__main__":
    main()
    
