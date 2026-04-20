import sys

'''
    Identify the index of a column in a delimited headline.
'''
def identifyColumn(delimiter: str, headline: str, column_name: str) -> int:
    columns = headline.strip().split(delimiter)
    
    for index, name in enumerate(columns):
        if name.strip() == column_name:
            return index
    
    return None

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