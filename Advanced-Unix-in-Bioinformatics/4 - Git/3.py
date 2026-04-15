import pandas as pd
import argparse
from statistics import median, mode

'''
    Trim the top and bottom 5% of the values and return the average of the remaining values.
'''
def trimmed_mean(values, trim_fraction):
    values = sorted(values)
    n = len(values)
    trim_count = int(n * trim_fraction)
    trimmed_values = values[trim_count : n - trim_count]
    return sum(trimmed_values) / len(trimmed_values) if trimmed_values else 0

def read_columns(filename : str, cols : list[str], calculate_avg : bool = False, calculate_median : bool = False, biggest : bool = False, length : bool = False, trimmed_mean_fraction : float = 0.0, frequent : bool = False):
    df = pd.read_csv(filename, sep="\t", names=cols)
    
    if cols is None:
        cols = df.select_dtypes(include=['number']).columns.tolist()

    number_list = []
    for col in cols:
        l1 = df[col].tolist()
        number_list.extend(l1)
    
    if calculate_avg:
        avg = sum(number_list) / len(number_list) if number_list else 0
        print(f"Average: {avg}")
    
    if calculate_median:
        med = median(number_list) if number_list else 0
        print(f"Median: {med}")
    
    if biggest:
        big = max(number_list) if number_list else 0
        print(f"Biggest: {big}")
    
    if length:
        print(f"Length: {len(number_list)}")
    
    if frequent:
        freq = mode(number_list) if number_list else 0
        print(f"Most Frequent: {freq}")

def main():
    parser = argparse.ArgumentParser(description="Read specific columns from a TSV file")
    parser.add_argument("filename", help="Input TSV file")
    parser.add_argument("-c", "--columns", nargs="+", default=None,
                        help="Column names to read")
    parser.add_argument("-a", "--average", action="store_true",
                        help="Calculate and return the average of all values")
    parser.add_argument("-m", "--median", action="store_true",
                        help="Calculate and return the median of all values")
    parser.add_argument("-b", "--biggest", action="store_true",
                        help="Find and return the biggest value")
    parser.add_argument("-n", "--length", action="store_true",
                        help="Print the length of all values")
    parser.add_argument("-t", "--trimmed-mean", type=float, default=0.0,
                        help="Calculate and return the trimmed mean of all values (specify trim fraction, e.g., 0.05 for 5%)")
    parser.add_argument("-f", "--frequent", action="store_true",
                        help="Find and return the most frequent value")
    
    args = parser.parse_args()
    read_columns(args.filename, args.columns, args.average, args.median, args.biggest, args.length, args.trimmed_mean_fraction, args.frequent)

if __name__ == "__main__":
    main()
