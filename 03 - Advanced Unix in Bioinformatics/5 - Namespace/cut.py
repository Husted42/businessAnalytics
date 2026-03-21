
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
