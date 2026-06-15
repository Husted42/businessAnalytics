import numpy as np
import matplotlib.pyplot as plt
import pandas as pd


################################ ---------- Exercise 7 - Part 1 ---------- ################################
# Simulated annealing for TSP using Euclidean distance

def euclidean_dist(a, b):
    """
    Compute Euclidean distance between two points.
    """
    return np.sqrt(np.sum((a - b) ** 2))


def route_length(route, points):
    """
    Compute total length of a closed TSP route.

    route: array of city indices
    points: array of shape (n, 2)
    """
    total_distance = 0.0
    n = len(route)

    for i in range(n):
        current_city = route[i]
        next_city = route[(i + 1) % n]  # return to start
        total_distance += euclidean_dist(points[current_city], points[next_city])

    return total_distance


def propose_swap(route):
    """
    Generate a new candidate route by swapping two randomly selected cities.
    """
    new_route = route.copy()

    i, j = np.random.choice(len(route), size=2, replace=False)

    new_route[i], new_route[j] = new_route[j], new_route[i]

    return new_route


def temperature(k, scheme="sqrt"):
    """
    Cooling schemes suggested in the slides.

    scheme="sqrt": T_k = 1 / sqrt(1 + k)
    scheme="log":  T_k = 1 / log(2 + k)
    """
    if scheme == "sqrt":
        return 1 / np.sqrt(1 + k)

    elif scheme == "log":
        return 1 / np.log(2 + k)

    else:
        raise ValueError("scheme must be either 'sqrt' or 'log'")


def simulated_annealing_tsp(
    points,
    n_iterations=50_000,
    cooling_scheme="sqrt",
    seed=42
):
    np.random.seed(seed)

    n = len(points)

    # Initial route: random permutation of cities
    current_route = np.random.permutation(n)
    current_distance = route_length(current_route, points)

    best_route = current_route.copy()
    best_distance = current_distance

    history = []

    for k in range(n_iterations):
        T = temperature(k, scheme=cooling_scheme)

        candidate_route = propose_swap(current_route)
        candidate_distance = route_length(candidate_route, points)

        difference = candidate_distance - current_distance

        # Accept if better, otherwise accept with probability exp(-difference / T)
        if difference <= 0:
            accept = True
        else:
            acceptance_probability = np.exp(-difference / T)
            accept = np.random.uniform() < acceptance_probability

        if accept:
            current_route = candidate_route
            current_distance = candidate_distance

            if current_distance < best_distance:
                best_route = current_route.copy()
                best_distance = current_distance

        history.append(best_distance)

    return best_route, best_distance, history


def plot_route(points, route, title="Simulated annealing TSP route", savepath="Assets/71_route"):
    """
    Plot the resulting closed route in the plane.
    """
    ordered_points = points[route]

    # Add first point again to close the route
    closed_route = np.vstack([ordered_points, ordered_points[0]])

    plt.figure(figsize=(7, 7))
    plt.plot(closed_route[:, 0], closed_route[:, 1], marker="o")

    for city_idx in route:
        x, y = points[city_idx]
        plt.text(x, y, str(city_idx), fontsize=10)

    plt.title(title)
    plt.xlabel("x")
    plt.ylabel("y")
    plt.axis("equal")
    plt.grid(True)
    plt.savefig(savepath)
    plt.close()


def plot_history(history, title="History", savepath="Assets/71_history"):
    """
    Plot best route length over iterations.
    """
    plt.figure(figsize=(8, 5))
    plt.plot(history)
    plt.title(title)
    plt.xlabel("Iteration")
    plt.ylabel("Best route length")
    plt.grid(True)
    plt.savefig(savepath)
    plt.close()


def generate_circle_points(n, radius=1, seed=42):
    """
    Generate n points placed on a circle.

    This is useful as a sanity check because the optimal route should follow
    the circle around the outside.
    """
    np.random.seed(seed)

    angles = np.linspace(0, 2 * np.pi, n, endpoint=False)

    points = np.column_stack([
        radius * np.cos(angles),
        radius * np.sin(angles)
    ])

    return points


def generate_random_points(n, seed=42):
    """
    Generate n random points in the unit square.
    """
    np.random.seed(seed)
    return np.random.uniform(0, 1, size=(n, 2))


################################ ---------- Exercise 7 - Part 2 ---------- ################################
# Simulated annealing for TSP using a general cost matrix

def load_cost_matrix(path="./cost.csv"):
    """
    Load a cost matrix from CSV.

    The file from Learn may have shape (19, 20), where the first column
    contains row labels. This function removes that first column if needed.
    """
    df = pd.read_csv(path, header=None)

    cost_matrix = df.to_numpy(dtype=float)

    if cost_matrix.shape[0] != cost_matrix.shape[1]:
        raise ValueError(f"Cost matrix must be square, but got shape {cost_matrix.shape}")

    return cost_matrix


def route_cost(route, cost_matrix):
    """
    Compute total cost of a closed TSP route using a general cost matrix.

    route: array of city indices
    cost_matrix[i, j]: cost of travelling from city i to city j
    """
    total_cost = 0.0
    n = len(route)

    for i in range(n):
        current_city = route[i]
        next_city = route[(i + 1) % n]  # return to first city
        total_cost += cost_matrix[current_city, next_city]

    return total_cost


def propose_reverse_segment(route):
    """
    Generate a new candidate route by reversing a randomly chosen segment.
    This is often a good proposal mechanism for TSP.
    """
    new_route = route.copy()

    i, j = np.random.choice(len(route), size=2, replace=False)

    if i > j:
        i, j = j, i

    new_route[i:j + 1] = new_route[i:j + 1][::-1]

    return new_route


def simulated_annealing_tsp_cost_matrix(
    cost_matrix,
    n_iterations=100_000,
    cooling_scheme="sqrt",
    proposal="swap",
    seed=42
):
    np.random.seed(seed)

    n = cost_matrix.shape[0]

    # Initial route
    current_route = np.array(range(n))
    current_cost = route_cost(current_route, cost_matrix)

    best_route = current_route.copy()
    best_cost = current_cost

    history = []

    for k in range(n_iterations):
        T = temperature(k, scheme=cooling_scheme)

        if proposal == "swap":
            candidate_route = propose_swap(current_route)

        elif proposal == "reverse":
            candidate_route = propose_reverse_segment(current_route)

        else:
            raise ValueError("proposal must be either 'swap' or 'reverse'")

        candidate_cost = route_cost(candidate_route, cost_matrix)

        difference = candidate_cost - current_cost

        # Accept all improvements
        if difference <= 0:
            accept = True

        else:
            # Accept worse solution with probability exp(-difference / T)
            acceptance_probability = np.exp(-difference / T)
            accept = np.random.uniform() < acceptance_probability

        if accept:
            current_route = candidate_route
            current_cost = candidate_cost

            if current_cost < best_cost:
                best_route = current_route.copy()
                best_cost = current_cost

        history.append(best_cost)

    return best_route, best_cost, history


def run_experiments(cost_matrix, n_iterations):
    """
    Compare different cooling schemes and proposal mechanisms.

    Each experiment is repeated 100 times, and the average best cost is returned.
    """
    experiments = [
        ("sqrt", "swap"),
        ("log", "swap"),
        ("sqrt", "reverse"),
        ("log", "reverse")
    ]

    results = []
    n_repetitions = 1

    for cooling_scheme, proposal in experiments:
        best_costs = []
        best_routes = []
        histories = []

        for i in range(n_repetitions):
            best_route, best_cost, history = simulated_annealing_tsp_cost_matrix(
                cost_matrix=cost_matrix,
                n_iterations=n_iterations,
                cooling_scheme=cooling_scheme,
                proposal=proposal,
                seed=42
            )

            best_costs.append(best_cost)
            best_routes.append(best_route)
            histories.append(history)

        avg_best_cost = np.mean(best_costs)

        # Route corresponding to the best run among the 100 repetitions
        best_run_index = np.argmin(best_costs)

        results.append({
            "Cooling scheme": cooling_scheme,
            "Proposal": proposal,
            "Best cost": avg_best_cost,
            "Best route": best_routes[best_run_index],
            "History": histories[best_run_index]
        })

    return results


###########################################################################################################
################################ ----------------- Main ------------------ ################################
###########################################################################################################

def main():
    ################################ ---------- Exercise 7 - Part 1 ---------- ################################

    n = 20

    # Use this for circle points:
    points = generate_circle_points(n)

    # Use this for random points:
    np.random.seed(42)
    # points = np.random.rand(n, 2)

    best_route, best_distance, history = simulated_annealing_tsp(
        points=points,
        n_iterations=10_000,
        cooling_scheme="sqrt",
        seed=42
    )

    print("Exercise 7 Part 1: Simulated annealing for TSP")
    print("------------------------------------------------")
    print("Best route:")
    print(best_route)
    print(f"Best route length: {best_distance:.4f}")

    plot_route(
        points,
        best_route,
        title=f"TSP route using simulated annealing, distance = {best_distance:.4f}",
        savepath="Assets/71_route"
    )

    plot_history(
        history,
        title="Best route length over iterations",
        savepath="Assets/71_history"
    )

    ################################ ---------- Exercise 7 - Part 2 ---------- ################################

    cost_matrix = load_cost_matrix("./cost.csv")

    print()
    print("Exercise 7 Part 2: Simulated annealing with general travel costs")
    print("----------------------------------------------------------------")
    print(f"Cost matrix shape: {cost_matrix.shape}")

    results = run_experiments(cost_matrix, n_iterations=10_000)

    for result in results:
        print()
        print(f"Cooling scheme: {result['Cooling scheme']}")
        print(f"Proposal: {result['Proposal']}")
        print(f"Average best cost over 100 runs: {result['Best cost']:.4f}")
        # print(f"Best route cost from best run: {route_cost(result['Best route'], cost_matrix):.4f}")
        # print("Best route:")
        # print(result["Best route"] + 1)  # +1 so cities are printed as 1, 2, ..., n

    # Plot the best run
    best_result = min(results, key=lambda x: x["Best cost"])

    plot_history(
        best_result["History"],
        title=(
            f"Best cost over iterations "
            f"({best_result['Cooling scheme']}, {best_result['Proposal']})"
        ),
        savepath="Assets/72_history"
    )

    plot_route(
        points,
        route=best_result["Best route"],
        title=f"TSP route using simulated annealing, average cost = {best_result['Best cost']:.4f}",
        savepath="Assets/72_route"
    )

    print()
    print("Best overall result")
    print("-------------------")
    print(f"Cooling scheme: {best_result['Cooling scheme']}")
    print(f"Proposal: {best_result['Proposal']}")
    print(f"Average best cost over 100 runs: {best_result['Best cost']:.4f}")
    print("Best route:")
    print(best_result["Best route"] + 1)


if __name__ == "__main__":
    main()