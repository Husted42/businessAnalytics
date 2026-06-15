import numpy as np

################################ ---------- Exercise 8 - Part 1 ---------- ################################
def run_bootstraping(data, a, b, k, seed):

    x_bar = np.mean(data)
    n = len(data)
    rng = np.random.default_rng(seed)

    bootstrap_differences = []

    for _ in range(k):
        bootstrap_sample = rng.choice(data, size=n, replace=True)
        bootstrap_mean = np.mean(bootstrap_sample)
        bootstrap_differences.append(bootstrap_mean - x_bar)

    bootstrap_differences = np.array(bootstrap_differences)

    p_hat = np.mean((a < bootstrap_differences) & (bootstrap_differences < b))


    results = {
        "Sample mean": x_bar,
        "Estimated p": p_hat,
    }
    return results

def main():
    data = np.array([56, 101, 78, 67, 93, 87, 64, 72, 80, 69])
    a = -5
    b = 5
    k = 10
    seed = 42
    results = run_bootstraping(data, a, b, k, seed)
    print(results)

    print("Sample mean:", results["Sample mean"])
    print("Estimated p:", results["Estimated p"])

if __name__ == "__main__":
    main()