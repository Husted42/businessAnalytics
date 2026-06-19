import numpy as np
import matplotlib.pyplot as plt
from scipy.stats import t, chi2, kstest
from scipy.linalg import expm
import sys
from contextlib import redirect_stdout
from Part_2 import simulate_woman_ctmc, Q

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
def simulate_woman_observations(Q, interval=48):
    state = 0
    time = 0
    next_observation = 0
    woman_states = []

    while state != 4:
        rate = -Q[state, state]
        sojourn_time = np.random.exponential(scale=1 / rate)
        jump_time = time + sojourn_time

        # Record all doctor visits before the next state transition
        while next_observation < jump_time:
            woman_states.append(state)
            next_observation += interval

        time = jump_time

        # Jump probabilities: P(i -> j) = q_ij / -q_ii
        probabilities = np.zeros(len(Q))

        for next_state in range(len(Q)):
            if next_state != state:
                probabilities[next_state] = (
                    Q[state, next_state]
                    / -Q[state, state]
                )

        state = np.random.choice(len(Q), p=probabilities)

    # Record death at the first doctor visit after death
    woman_states.append(4)

    return woman_states

#################### -------------------- Task 13 -------------------- ####################
def simulate_interval(Q, start_state, end_state, interval=48):
    """
    Simulate the Markov process between two observations.
    Reject the simulation unless X(interval) = end_state.
    """
    while True:
        state = start_state
        time = 0

        N_ij = np.zeros_like(Q) # Numbe of transitions going from state i to state j
        S_i = np.zeros(len(Q)) # Total amount of time spent in each transition

        # Simulate the process between the two observations (48 t apart)
        # This is essentially the exact same as in Part_2 simulate_woman_ctmc()
        while time < interval:
            if state == 4:
                S_i[state] += interval - time
                break

            rate = -Q[state, state]
            sojourn_time = np.random.exponential(1 / rate)

            # If all the time passes and we haven't reached a new state we need to break the loop
            # Otherwise it would run indefinitely 
            if time + sojourn_time >= interval:
                S_i[state] += interval - time
                break

            # We then update the time
            # Note that the time can be longer than the interval, in that case we break (See above)
            S_i[state] += sojourn_time
            time += sojourn_time

            # q_ii / -q(i,j)
            # if j=i then we get -1, which is not a valid state. 
            # Therefore we et the probability at the current state to 0
            probabilities = Q[state] / rate
            probabilities[state] = 0 
            if sum(probabilities) - 1 > 0.01: raise ValueError ("Dones't sum to 1")

            next_state = np.random.choice(len(Q), p=probabilities)

            N_ij[state, next_state] += 1
            state = next_state

        if state == end_state:
            return N_ij, S_i


def estimate_Q(observation_states_matrix, Q_initial, interval=48):
    Q_current = Q_initial.copy()
    i = 0
    while True:
        i = i + 1
        N_ij = np.zeros_like(Q)
        S_i = np.zeros(len(Q))

        # Step 1: simulate complete trajectories
        for woman_states in observation_states_matrix:
            for j in range(len(woman_states) - 1):
                start_state = woman_states[j]
                end_state = woman_states[j + 1]

                if start_state == 4:
                    break

                N_interval, S_interval = simulate_interval(
                    Q_current,
                    start_state,
                    end_state,
                    interval
                )

                # Step 2: summarize the trajectories
                N_ij += N_interval
                S_i += S_interval

        # Step 3: find Q^(k+1) using equation (2)
        Q_new = np.zeros_like(Q_current)

        for i in range(len(Q) - 1):
            for j in range(len(Q)):
                if i != j:
                    Q_new[i, j] = N_ij[i, j] / S_i[i]

            Q_new[i, i] = -np.sum(Q_new[i])

        difference = np.max(np.abs(Q_current - Q_new))
        print(f"Difference: {difference:.6f}")

        if difference < 1e-3:
            return Q_new, i

        Q_current = Q_new

def main():
    np.random.seed(42)
    n = 1000
    
    #################### -------------------- Task 12 -------------------- ####################
    print("-------------- Task 12 - Simulate data --------------")
    '''
        The women go to the doctoer every 4'th year, 
        so we assume that this is the only data we have
    '''
    observation_states = []
    for i in range(n):
        woman_states = simulate_woman_observations(Q)
        observation_states.append(woman_states)
    
    total_stages = 0
    for i in range(len(observation_states)):
        total_stages += len(observation_states[i])
    print(total_stages)
    
    # I assume this is easier if it's a matrix:
    m = max(len(woman_states) for woman_states in observation_states)
    observation_states_matrix = np.full(
        shape=(n, m),
        fill_value=4,
        dtype=int
    )

    for i, woman_states in enumerate(observation_states):
        observation_states_matrix[i, :len(woman_states)] = woman_states

    print("Created one fine matrix")
    print(observation_states_matrix)
    print("Shape:", observation_states_matrix.shape)


    #################### -------------------- Task 13 -------------------- ####################
    print("\n\n-------------- Task 13 - The algorithm --------------")

    Q_initial = np.array([
        [-0.008, 0.002, 0.002, 0.002, 0.002],
        [0.000, -0.006, 0.002, 0.002, 0.002],
        [0.000, 0.000, -0.004, 0.002, 0.002],
        [0.000, 0.000, 0.000, -0.002, 0.002],
        [0.000, 0.000, 0.000, 0.000, 0.000]
    ])

    Q_estimated, i = estimate_Q(
        observation_states_matrix,
        Q_initial
    )

    print("\nOriginal Q:")
    print(Q)

    print("\nEstimated Q:")
    print(Q_estimated)
    print(i)




if __name__ == "__main__":
    with open("Log/P3_results.txt", "w") as file:
        with redirect_stdout(Tee(sys.stdout, file)):
            main()