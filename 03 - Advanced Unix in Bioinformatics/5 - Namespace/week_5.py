import requests


def download_file(url, output_path):
    response = requests.get(url)
    response.raise_for_status()
    with open(output_path, "wb") as f:
        f.write(response.content)

download_file("https://teaching.healthtech.dtu.dk/material/22118/dna7.fsa", "dna7.fsa")

########### ----------- Exercise 01 ----------- ###########
'''
    Read file line by line and store headers and sequences in separate lists.
    Headers start with '>' and sequences are the lines that follow until the next header.
    
    Returns: two lists: headers and sequences.
'''
def fastaread(filename : str) -> tuple[list[str], list[str]]:
    seq = ""
    with open(filename, "r") as file:
        
        # Each line is eiter a header (starting with '>') or part of a sequence
        headers, sequences = [], []
        for line in file:
            line = line.strip()

            if line.startswith(">"):
                headers.append(line[1:])  # Remove '>'
                sequences.append("")  # Start a new sequence
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

########### ----------- Exercise 02 ----------- ###########
'''
    Write headers and sequences to a FASTA file. Each sequence is written in lines of 60 characters.

    returns: None
'''
def fastawrite(headers : list[str], sequences : list[str], output_path : str) -> None:
    if len(headers) != len(sequences):
        raise ValueError("Number of headers and sequences must be the same.")
    
    with open(output_path, "w") as file:
        for header, sequence in zip(headers, sequences):
            
            # Write header followed by the sequence in 60 character batches
            file.write(f">{header}\n")
            for i in range(0, len(sequence), 60):
                file.write(sequence[i:i+60] + "\n")
    
    return None

fastawrite(headers, sequences, "dna7_copy.fsa")

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

########### ----------- Exercise 04 ----------- ###########
'''
    Normalize a list of scores to a range of 0 to 1 using min-max normalization.
    If all scores are the same, return a list of 0.0 to avoid division by zero.

    returns: A list of normalized scores.
'''
def normalize_scores(scores : list[float], min_val : float, max_val : float) -> list[float]:
    if max_val == min_val:
        return [0.0] * len(scores) 
    return [(score - min_val) / (max_val - min_val) for score in scores]

test_scores = [10, 20, 30, 40, 50]
normalized = normalize_scores(test_scores, min(test_scores), max(test_scores))
print("Original scores:", test_scores)
print("Normalized scores:", normalized)

########### ----------- Exercise 05 ----------- ###########

'''
    Read a file of numbers into an n x m matrix (list of lists).

    returns: A matrix (list of lists) of floats.
'''
def read_matrix(filename : str) -> list[list[float]]:
    matrix = []
    with open(filename, "r") as file:
        for line in file:
            row = [float(x) for x in line.strip().split()]
            matrix.append(row)
    return matrix


'''
    Count the number of positive and negative values in each column of a matrix.
    
    returns: A list of tuples
'''
def count_positive_negative(matrix : list[list[float]]) -> list[tuple[int, int]]:
    counts = []
    for col in zip(*matrix):
        pos_count = sum(1 for x in col if x > 0)
        neg_count = sum(1 for x in col if x < 0)
        counts.append((pos_count, neg_count))
    return counts



matrix = read_matrix("ex1.dat")
counts = count_positive_negative(matrix)
print("Column counts (positive, negative):")
for idx, (pos, neg) in enumerate(counts):
    print(f"Column {idx}: {pos} positive, {neg} negative")

########### ----------- Exercise 06 ----------- ###########
'''
    Finds the first occurrence of a delimiter in a line and returns it.

    returns: The first delimiter
'''
def determine_delimiter(line: str) -> str:
    delimiters = ["\t", ",", ":", ";", "|"]
    
    for delim in delimiters:
        if delim in line:
            return delim
    
    raise ValueError("No known delimiter found in the line.")

files = ["ex1.dat", "dna7.fsa"]
for filename in files:
    for line in open(filename):
        delimiter = determine_delimiter(line)
        print(f"Determined delimiter: '{delimiter}' for file {filename}")
        break  # Only need to check the first line

########### ----------- Exercise 07 ----------- ###########
download_file("https://teaching.healthtech.dtu.dk/material/22118/employee-data.csv", "employee-data.csv")

'''
    Identify the index of a column in a delimited headline.
'''
def identifyColumn(delimiter: str, headline: str, column_name: str) -> int:
    columns = headline.strip().split(delimiter)
    
    for index, name in enumerate(columns):
        if name.strip() == column_name:
            return index
    
    return None

# Test
files = ["employee-data.csv"]
for filename in files:
    with open(filename, "r") as file:
        first_line = file.readline()
        delimiter = determine_delimiter(first_line)
        column_index = identifyColumn(delimiter, first_line, "Ethnicity")
        print(f"Column index for 'Ethnicity' in file {filename}: {column_index}")

########### ----------- Exercise 8 ----------- ###########
import sys

def parseCommand():
    numbers = []
    names = []
    filename = None

    args = sys.argv[1:]  # skip program name
    i = 0

    while i < len(args):
        arg = args[i]

        if arg == "-c":
            if names:
                raise ValueError("Options -c and -n are mutually exclusive.")
            if i + 1 >= len(args):
                raise ValueError("Missing value for -c option.")

            raw_numbers = args[i + 1].split(",")

            try:
                numbers = [int(n) for n in raw_numbers]
            except ValueError:
                raise ValueError("Invalid positive integer list for -c.")

            if any(n <= 0 for n in numbers):
                raise ValueError("All numbers must be positive integers.")

            i += 2

        elif arg == "-n":
            if numbers:
                raise ValueError("Options -c and -n are mutually exclusive.")
            if i + 1 >= len(args):
                raise ValueError("Missing value for -n option.")

            names = args[i + 1].split(",")
            i += 2

        else:
            if filename is not None:
                raise ValueError("Multiple filenames provided.")
            filename = arg
            i += 1

    if filename is None:
        raise ValueError("Filename is required.")

    return numbers, names, filename

if __name__ == "__main__":
    try:
        numbers, names, filename = parseCommand()
        print("Numbers:", numbers)
        print("Names:", names)
        print("Filename:", filename)
    except ValueError as e:
        print("Error:", e)


########### ----------- Exercise 9 ----------- ###########

# cut.py: Select columns from a delimited file, similar to Unix cut
import sys
import os
import csv

# usage(): Print usage instructions and exit
def usage():
    print("Usage:")
    print("  python cut.py <filename> [-c col1,col2,... | -n name1,name2,...]")
    print("Options:")
    print("  -c: Select columns by index (1-based), strips headline")
    print("  -n: Select columns by name, keeps headline")
    print("If neither -c nor -n is used, all columns are displayed, delimiter changed to tab.")
    sys.exit(1)

# detect_delimiter(): Try to guess the delimiter used in the file
def detect_delimiter(filename):
    with open(filename, 'r', encoding='utf-8') as f:
        sample = f.read(1024)  # Read a sample to detect delimiter
    for delim in [',', ';', ':', '\t']:
        if delim in sample:
            return delim
    return ','  # Default to comma if none found

# read_csv(): Read the file using the detected delimiter
def read_csv(filename, delimiter):
    with open(filename, 'r', encoding='utf-8') as f:
        reader = csv.reader(f, delimiter=delimiter)
        return list(reader)

# main(): Parse arguments and perform column selection
def main():
    # Check for minimum argument count
    if len(sys.argv) < 2:
        usage()
    filename = sys.argv[1]
    # Check if file exists
    if not os.path.isfile(filename):
        print(f"Error: File '{filename}' not found.")
        sys.exit(1)
    args = sys.argv[2:]
    delim = detect_delimiter(filename)
    data = read_csv(filename, delim)
    # Check for empty file
    if not data:
        print("Error: File is empty.")
        sys.exit(1)

    # Handle -c option: select columns by index
    if '-c' in args:
        idx = args.index('-c')
        if idx+1 >= len(args):
            usage()
        cols = args[idx+1].split(',')
        try:
            cols = [int(c)-1 for c in cols]  # Convert to 0-based indices
        except ValueError:
            print("Error: Column indices must be integers.")
            sys.exit(1)
        # Print selected columns for each row except headline
        for row in data[1:]:
            print('\t'.join([row[c] for c in cols]))

    # Handle -n option: select columns by name
    elif '-n' in args:
        idx = args.index('-n')
        if idx+1 >= len(args):
            usage()
        names = args[idx+1].split(',')
        header = data[0]
        # Check for missing headline
        if not header or any(not h for h in header):
            print("Error: No headline found for named columns.")
            sys.exit(1)
        try:
            indices = [header.index(n) for n in names]  # Get indices for names
        except ValueError as e:
            print(f"Error: Column name not found: {e}")
            sys.exit(1)
        # Print headline and selected columns
        print('\t'.join([header[i] for i in indices]))
        for row in data[1:]:
            print('\t'.join([row[i] for i in indices]))

    # If no option, print all columns with tab delimiter
    else:
        for row in data:
            print('\t'.join(row))

# Entry point for script
if __name__ == "__main__":
    main()
