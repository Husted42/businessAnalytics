import pandas as pd
import matplotlib.pyplot as plt
from pathlib import Path
import numpy as np

# Read data
base_dir = Path(__file__).resolve().parent
csv_path = base_dir / "DST_BIL54.csv"
if not csv_path.exists():
    csv_path = base_dir / "Assignment1" / "DST_BIL54.csv"
df = pd.read_csv(csv_path)

# Parse monthly timestamp (YYYY-MM -> first day of month)
df["time"] = pd.to_datetime(df["time"] + "-01", format="%Y-%m-%d", utc=True)

# Fractional year: 2018-Jan = 2018, 2018-Feb = 2018 + 1/12, ...
df["x"] = df["time"].dt.year + (df["time"].dt.month - 1) / 12

# Make output variable a floating point in millions
df["total"] = pd.to_numeric(df["total"], errors="coerce") / 1e6

# Train/test split
teststart = pd.Timestamp("2024-01-01", tz="UTC")
dtrain = df[df["time"] < teststart].copy()
_dtest = df[df["time"] >= teststart].copy()

# x, y from training set
dtrain["x0"] = dtrain["x"] - 2018.0
x = dtrain["x0"].to_numpy()
y = dtrain["total"].to_numpy()
N = len(x)

# ----- RLS (no forgetting) -----
def rls_no_forgetting(x, y, R0, theta0, T):
    RS = []
    R = R0.astype(float).copy()
    RS.append(R)
    thetas = []
    theta = theta0.astype(float).copy()
    thetas.append(theta)
    steps = min(T, len(x))
    for t in range(steps):
        X = np.array([[1.0], [x[t]]])
        R = RS[-1] + (X @ X.T)
        err = y[t] - (X.T @ thetas[-1]).item()
        theta = theta + np.linalg.solve(R, X * err)
        RS.append(R.copy())
        thetas.append(theta.copy())
    return thetas, RS

R0 = np.array([[0.1, 0.0],
               [0.0, 0.1]])
theta0 = np.zeros((2, 1))

thetas_rls_3, RS_3 = rls_no_forgetting(x, y, R0, theta0, 3)
print(f"Theta_3={thetas_rls_3}.")

thetas_rls_N, RS_N = rls_no_forgetting(x, y, R0, theta0, N)
print(f"Theta_N={thetas_rls_N[-1]}.")

# ----- OLS -----
Xmat = np.column_stack([np.ones(N), x])        # (N,2)
theta_ols = np.linalg.lstsq(Xmat, y, rcond=None)[0].reshape(2, 1)

print("theta_RLS(N) =", thetas_rls_N[-1].ravel())
print("theta_OLS    =", theta_ols.ravel())
print("difference   =", (thetas_rls_N[-1] - theta_ols).ravel())

for delta in [10.0, 1.0, 0.1, 0.01, 0.001]:
    R0_test = delta * np.eye(2)
    theta_rls, _ = rls_no_forgetting(x, y, R0_test, theta0, N)
    diff = np.linalg.norm(theta_rls[-1] - theta_ols)
    print(f"delta={delta:<6}  ||theta_rls-ols||={diff:.3e}")