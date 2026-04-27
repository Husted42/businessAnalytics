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


main()

'''
    Sorting is asymtopically biggest so it's, given rows=n we have
    n log(n)
'''

########### ----------- Exercise 02 ----------- ###########

def fastaread(filename : str) -> tuple[list[str], list[str]]:
    seq = ""
    with open(filename, "r") as file:
        
        headers, sequences = [], []
        for line in file:
            line = line.strip()

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

headers, sequences = fastaread("dna7.fsa")

print("Headers:")
print(headers)

print("\nSequences:")
print(sequences)

########### ----------- Exercise 03 ----------- ###########
'''
    Normalize a list of scores to a range of 0 to 1 using min-max normalization.
    If all scores are the same, return a list of 0.0 to avoid division by zero.

    returns: A list of normalized scores.
'''
def normalize_scores(scores : list[float]) -> list[float]:
    min_score = min(scores)
    max_score = max(scores)
    if max_score == min_score:
        return [0.0] * len(scores) 
    return [(score - min_score) / (max_score - min_score) for score in scores]

test_scores = [10, 20, 30, 40, 50]
normalized = normalize_scores(test_scores)
print("Original scores:", test_scores)
print("Normalized scores:", normalized)
