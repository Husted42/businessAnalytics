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