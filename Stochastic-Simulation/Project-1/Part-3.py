import numpy as np
import matplotlib.pyplot as plt
from scipy.stats import t, chi2, kstest
from scipy.linalg import expm
import sys
from contextlib import redirect_stdout


class Tee:
    def __init__(self, *files):
        self.files = files

    def write(self, text):
        for file in self.files:
            file.write(text)

    def flush(self):
        for file in self.files:
            file.flush()

#################### -------------------- Task 12 -------------------- ####################

def main():
    np.random.seed(42)
    #################### -------------------- Task 12 -------------------- ####################


if __name__ == "__main__":
    with open("Log/P2_results.txt", "w") as file:
        with redirect_stdout(Tee(sys.stdout, file)):
            main()