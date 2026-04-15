########### ----------- Exercise 02 ----------- ###########
import sys
import os
import unittest

# Ignore that this is a windows path ;)
sys.path.append('C:\\Users\\huste\\Documents\\Github\\businessAnalytics\\03 - Advanced Unix in Bioinformatics\\9 - Unit test\\Handin\\src') 

from factorial import factorial

class TestFactorial(unittest.TestCase):

    # Valid inputs
    def test_factorial_12(self):
        self.assertEqual(factorial(12), 479001600)

    def test_factorial_2(self):
        self.assertEqual(factorial(2), 2)

    def test_factorial_1(self):
        self.assertEqual(factorial(1), 1)

    def test_factorial_0(self):
        self.assertEqual(factorial(0), 1)

    # Invalid inputs (all should raise ValueError now)
    def test_factorial_negative(self):
        with self.assertRaises(ValueError):
            factorial(-1)

    def test_factorial_float_intlike(self):
        with self.assertRaises(ValueError):
            factorial(3.0)

    def test_factorial_float(self):
        with self.assertRaises(ValueError):
            factorial(3.4)

    def test_factorial_string_int(self):
        with self.assertRaises(ValueError):
            factorial("3")

    def test_factorial_string_float(self):
        with self.assertRaises(ValueError):
            factorial("3.1.")

    def test_factorial_string_alpha(self):
        with self.assertRaises(ValueError):
            factorial("ABC")


if __name__ == "__main__":
    unittest.main()