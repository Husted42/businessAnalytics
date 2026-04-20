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


#################### -------------------- Code -------------------- #################### 
def rls_step(theta_prior, R_prior, x, y):
    R = R_prior + x.T @ x # Formula 11.13b
    theta = theta_prior + np.linalg.inv(R) @ x.T * (y - x @ theta_prior) # Formula 11.13a

    return theta, R

#################### -------------------- Main -------------------- ####################
def main():

    x = dtrain["x0"].to_numpy()
    y = dtrain["total"].to_numpy()
    R0 = np.array([[0.1, 0.0],
               [0.0, 0.1]])
    theta0 = np.zeros((2, 1)).astype(float)
    N = len(x)

    for i in range(N):
        xi = x[i]
        yi = y[i]
        print(f"i {i} xi: {xi}, yi: {yi}")
        X_t = np.array([[1.0, xi]])
        y_t = np.array([[yi]])

        theta, R = rls_step(theta0, R0, X_t, y_t)
        theta0 = theta
        R0 = R

        # Wee ne

if __name__ == "__main__":
    main()