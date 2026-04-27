import csv

########### ----------- Exercise 01 ----------- ###########
def main():
    filename = "scores.txt"
    rows = []

    # Load data
    # O(filesize)   
    with open(filename, "r", newline="") as file:
        reader = csv.reader(file, delimiter="\t")
        # O(Rows)
        for row in reader:
            student_id = row[0]
            scores = list(map(int, row[1:]))

            total = sum(scores)

            rows.append((student_id, total))

    # O(rows Log(rows))
    rows.sort(key=lambda x: x[1], reverse=True)

    # O(1)
    extreme_rows = rows[:10] + rows[-10:]

    # Save results
    # O(1)
    with open("scores_extreme.txt", "w", newline="") as file:
        writer = csv.writer(file, delimiter="\t")

        # Optional header
        writer.writerow(["id", "Total"])

        # O(1)
        for row in extreme_rows:
            writer.writerow(row)

    # O(1)
    for row in extreme_rows:
        print(row)


''' ANSWER:
    Sorting is asymtopically biggest so it's, given rows=n we have
    O(n*log(n))
'''

########### ----------- Exercise 02 ----------- ###########

def fastaread(filename : str) -> tuple[list[str], list[str]]:
    seq = ""
    # O(lines in file) I just assume that how it reads
    with open(filename, "r") as file:
        
        headers, sequences = [], []
        # O(lines in file) s
        for line in file:
            line = line.strip()

            # O(1)
            if line.startswith(">"):
                headers.append(line[1:])  
                sequences.append("")  
            else:
                sequences[-1] += line  
    
    if len(headers) != len(sequences):
        raise ValueError("Number of headers and sequences do not match.")
    if len(headers) == 0:
        raise ValueError("No headers found in the file.")

    return headers, sequences

''' ANSWER:
    Loops twice over lines. N = lines
    O(n^2)
'''

########### ----------- Exercise 03 ----------- ###########
'''
    Normalize a list of scores to a range of 0 to 1 using min-max normalization.
    If all scores are the same, return a list of 0.0 to avoid division by zero.

    returns: A list of normalized scores.
'''
def normalize_scores(scores : list[float]) -> list[float]:
    # O(2*n) = O(n) to find min and max
    min_score = min(scores)
    max_score = max(scores)
    if max_score == min_score:
        return [0.0] * len(scores) 
    # O(n) to normalize scores
    return [(score - min_score) / (max_score - min_score) for score in scores]

''' ANSWER:
    O(n) to find min and max, O(n) to normalize scores,

    O(3*n) = O(n)
'''

########### ----------- Exercise 04 ----------- ###########
########### ----------- Exercise 05 ----------- ###########
########### ----------- Exercise 06 ----------- ###########

########### ----------- Exercise 07 ----------- ###########
''' QT-clustering algorithm

    Sorry for over commenting, I needed it to keep track of what is going on.
'''
data = [
    (1, 2),
    (2, 3),
    (3, 4),
    (10, 10),
    (11, 11),
    (12, 12)
]
threshold = 2

candidate_clusters = []

# Pythagorean theorem
def distance(point1, point2):
    return ((point1[0] - point2[0]) ** 2 + 
            (point1[1] - point2[1]) ** 2) ** 0.5

# Check how the diameter of a cluster would change if we added a new point. By complete-linkage
# O(k), where k = len(cluster)
def diameter_if_added(cluster, new_point, current_diameter):
    max_distance_to_cluster = 0

    # Compare the new point to every point already in the cluster
    # O(k)
    for point in cluster:
        d = distance(point, new_point)  # O(1)

        if d > max_distance_to_cluster:
            max_distance_to_cluster = d

    # This grows the cluster diameter if the new point is farther away
    return max(current_diameter, max_distance_to_cluster)


# O(n) loop over all points as starting points
for start_point in data:

    # Candidate cluster starts with just the starting point
    candidate_cluster = [start_point]

    # O(n), because we scan all points and exclude the starting point
    remaining_points = [p for p in data if p != start_point]

    current_diameter = 0  # Only 1 point, so diameter is 0

    # Keep trying to add the point that increases the cluster diameter the least
    # Runs at most O(n) times
    while remaining_points:
        best_point = None
        best_new_diameter = float("inf")

        # Find the point that gives the smallest new complete-linkage diameter
        # O(m * k), where m = len(remaining_points), k = len(candidate_cluster)
        for point in remaining_points:
            new_diameter = diameter_if_added(
                candidate_cluster,
                point,
                current_diameter
            )

            # If two points are equal, this keeps the first one found,
            # because we only update on strictly smaller diameter
            if new_diameter < best_new_diameter:
                best_point = point
                best_new_diameter = new_diameter

        # Stop if adding the best point would exceed the quality threshold.
        # The point that exceeds the threshold is NOT added.
        if best_new_diameter > threshold:
            break

        # Add the best valid point to the candidate cluster
        candidate_cluster.append(best_point)

        # Remove and update
        # remaining_points.remove(best_point) is O(m)
        remaining_points.remove(best_point)
        current_diameter = best_new_diameter

    # Store the candidate cluster for this starting point
    # append is O(1)
    candidate_clusters.append(candidate_cluster)

print("Candidate clusters:")
for cluster in candidate_clusters:
    print(cluster)

''' ANSWER:
    O(n) loop over starting points
        O(n) to find remaining points
        O(n) loop to add points to cluster
            O(m * k) to find best point to add, where m = remaining points, k = cluster size
                O(k) to calculate diameter if added

    In worst case, m and k can both be O(n), so we have:
    O(n * (n + n * n * n)) = O(n^4)
'''