########### ----------- Exercise 01 ----------- ###########

def factorial(n):
    if not isinstance(n, int) or n < 0:
        raise ValueError("factorial requires a non-negative integer")
    if n < 0:
        raise ValueError("factorial requires a non-negative integer")
    result = 1
    for i in range(2, n + 1):
        result *= i
    return result
