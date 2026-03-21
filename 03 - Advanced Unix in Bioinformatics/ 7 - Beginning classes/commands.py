'''
    I feel like it's waste of your time to go through all excercises,
    So I have concated them toghetger
'''

########### ----------- Exercise 1-2 ----------- ###########
class MyMath:
    def __init__(self):
        self._numbers = []

    def supply(self, numbers):
        self._numbers = list(numbers)

    def maximum(self):
        if not self._numbers:
            raise ValueError("no numbers supplied")
        return max(self._numbers)

    def minimum(self):
        if not self._numbers:
            raise ValueError("no numbers supplied")
        return min(self._numbers)

    def average(self):
        if not self._numbers:
            raise ValueError("no numbers supplied")
        return sum(self._numbers) / len(self._numbers)

    @staticmethod
    def cube(x):
        return x ** 3

    @staticmethod
    def factorial(n):
        if not isinstance(n, int) or n < 0:
            raise ValueError("factorial requires a non-negative integer")
        result = 1
        for i in range(2, n + 1):
            result *= i
        return result

    @staticmethod
    def collatz_steps(n):
        if not isinstance(n, int) or n <= 0:
            raise ValueError("collatz_steps requires a positive integer")
        steps = 0
        while n != 1:
            if n % 2 == 0:
                n //= 2
            else:
                n = 3 * n + 1
            steps += 1
        return steps

# quick check
mm = MyMath()
mylist = [4, 1, 8, -2, 7]
mm.supply(mylist)class MyMath:

assert mm.maximum() == 8
assert mm.minimum() == -2
assert mm.average() == sum(mylist) / len(mylist)

assert MyMath.collatz_steps(77031) == 350
assert MyMath.factorial(5) == 120
print("Max is", mm.maximum())

########### ----------- Exercise 3-8 ----------- ###########
class Fasta:
    DNA = set("ATCG")
    RNA = set("AUCG")
    PROTEIN = set("ACDEFGHIKLMNPQRSTVWY")

    def __init__(self, filename=None):
        self.headers = []
        self.seqs = []
        if filename:
            self.load(filename)

    def load(self, filename):
        self.headers = []
        self.seqs = []
        current_header = None
        current_seq = []
        with open(filename, "r") as f:
            for line in f:
                line = line.strip()
                if not line:
                    continue
                if line.startswith(">"):
                    if current_header is not None:
                        self.headers.append(current_header)
                        self.seqs.append("".join(current_seq))
                    current_header = line[1:].strip()
                    current_seq = []
                else:
                    if current_header is None:
                        raise ValueError("FASTA format error: sequence line before header")
                    current_seq.append(line)
            if current_header is not None:
                self.headers.append(current_header)
                self.seqs.append("".join(current_seq))

    def save(self, filename):
        with open(filename, "w") as f:
            for header, seq in zip(self.headers, self.seqs):
                f.write(">" + header + "\n")
                for i in range(0, len(seq), 80):
                    f.write(seq[i:i+80] + "\n")

    def content(self, start=None, end=None):
        if start is None and end is None:
            return self.headers.copy(), self.seqs.copy()
        if end is None:
            return self.headers[start], self.seqs[start]
        return self.headers[start:end].copy(), self.seqs[start:end].copy()

    def delete(self, start=None, end=None):
        if start is None and end is None:
            self.headers = []
            self.seqs = []
        elif end is None:
            del self.headers[start]
            del self.seqs[start]
        else:
            del self.headers[start:end]
            del self.seqs[start:end]

    def insert(self, header, sequence, position=None):
        if isinstance(header, str):
            headers = [header]
            seqs = [sequence]
        else:
            headers = list(header)
            seqs = list(sequence)

        if len(headers) != len(seqs):
            raise ValueError("header and sequence must have the same length")

        if position is None:
            position = len(self.headers)

        if position < 0:
            position += len(self.headers)

        position = max(0, min(position, len(self.headers)))

        for i, (h, s) in enumerate(zip(headers, seqs)):
            self.headers.insert(position + i, h)
            self.seqs.insert(position + i, s)

    def verify(self, alphabet, start=None, end=None):
        if isinstance(alphabet, str):
            if hasattr(self.__class__, alphabet):
                allowed = getattr(self.__class__, alphabet)
            else:
                allowed = set(alphabet)
        else:
            allowed = set(alphabet)

        if start is None and end is None:
            seqs_to_check = self.seqs
        elif end is None:
            seqs_to_check = [self.seqs[start]]
        else:
            seqs_to_check = self.seqs[start:end]

        for seq in seqs_to_check:
            if not set(seq).issubset(allowed):
                return False
        return True

    def discard(self, alphabet, start=None, end=None):
        if isinstance(alphabet, str):
            if hasattr(self.__class__, alphabet):
                allowed = getattr(self.__class__, alphabet)
            else:
                allowed = set(alphabet)
        else:
            allowed = set(alphabet)

        if start is None and end is None:
            indices = range(len(self.seqs))
        elif end is None:
            indices = [range(len(self.seqs))[start]]
        else:
            indices = range(len(self.seqs))[start:end]

        to_delete = []
        for i in indices:
            if not set(self.seqs[i]).issubset(allowed):
                to_delete.append(i)

        for i in reversed(to_delete):
            del self.headers[i]
            del self.seqs[i]

import os

def run_fasta_tests():
    print("Running Fasta tests...")

    # -----------------------------
    # Test 1: empty initialization
    # -----------------------------
    f = Fasta()
    assert f.headers == []
    assert f.seqs == []

    # ---------------------------------------
    # Test 2: insert single item at the end
    # ---------------------------------------
    f.insert("h1", "ATCG")
    assert f.headers == ["h1"]
    assert f.seqs == ["ATCG"]

    # ---------------------------------------
    # Test 3: insert another single at end
    # ---------------------------------------
    f.insert("h2", "GGTT")
    assert f.headers == ["h1", "h2"]
    assert f.seqs == ["ATCG", "GGTT"]

    # ---------------------------------------
    # Test 4: insert single at position 0
    # ---------------------------------------
    f.insert("h0", "AAAA", 0)
    assert f.headers == ["h0", "h1", "h2"]
    assert f.seqs == ["AAAA", "ATCG", "GGTT"]

    # ---------------------------------------
    # Test 5: insert multiple in middle
    # ---------------------------------------
    f.insert(["h3", "h4"], ["CCCC", "TTTT"], 2)
    assert f.headers == ["h0", "h1", "h3", "h4", "h2"]
    assert f.seqs == ["AAAA", "ATCG", "CCCC", "TTTT", "GGTT"]

    # ---------------------------------------
    # Test 6: insert with negative position
    # ---------------------------------------
    f.insert("hx", "NNNN", -1)
    assert f.headers == ["h0", "h1", "h3", "h4", "hx", "h2"]
    assert f.seqs == ["AAAA", "ATCG", "CCCC", "TTTT", "NNNN", "GGTT"]

    # ---------------------------------------
    # Test 7: insert mismatched lengths
    # ---------------------------------------
    f2 = Fasta()
    try:
        f2.insert(["a", "b"], ["ATCG"])
        assert False, "Expected ValueError for mismatched header/sequence lengths"
    except ValueError:
        pass

    # ---------------------------------------
    # Test 8: content() full
    # ---------------------------------------
    headers, seqs = f.content()
    assert headers == ["h0", "h1", "h3", "h4", "hx", "h2"]
    assert seqs == ["AAAA", "ATCG", "CCCC", "TTTT", "NNNN", "GGTT"]

    # ---------------------------------------
    # Test 9: content(index)
    # ---------------------------------------
    h, s = f.content(1)
    assert h == "h1"
    assert s == "ATCG"

    # ---------------------------------------
    # Test 10: content(slice)
    # ---------------------------------------
    headers, seqs = f.content(1, 4)
    assert headers == ["h1", "h3", "h4"]
    assert seqs == ["ATCG", "CCCC", "TTTT"]

    # ---------------------------------------
    # Test 11: content with negative index
    # ---------------------------------------
    h, s = f.content(-1)
    assert h == "h2"
    assert s == "GGTT"

    # ---------------------------------------
    # Test 12: content returns copies
    # ---------------------------------------
    headers, seqs = f.content()
    headers.append("BAD")
    seqs.append("BADSEQ")
    assert f.headers == ["h0", "h1", "h3", "h4", "hx", "h2"]
    assert f.seqs == ["AAAA", "ATCG", "CCCC", "TTTT", "NNNN", "GGTT"]

    # ---------------------------------------
    # Test 13: delete single element
    # ---------------------------------------
    f3 = Fasta()
    f3.headers = ["a", "b", "c"]
    f3.seqs = ["AAA", "CCC", "GGG"]
    f3.delete(1)
    assert f3.headers == ["a", "c"]
    assert f3.seqs == ["AAA", "GGG"]

    # ---------------------------------------
    # Test 14: delete slice
    # ---------------------------------------
    f3 = Fasta()
    f3.headers = ["a", "b", "c", "d"]
    f3.seqs = ["AAA", "CCC", "GGG", "TTT"]
    f3.delete(1, 3)
    assert f3.headers == ["a", "d"]
    assert f3.seqs == ["AAA", "TTT"]

    # ---------------------------------------
    # Test 15: delete all
    # ---------------------------------------
    f3.delete()
    assert f3.headers == []
    assert f3.seqs == []

    # ---------------------------------------
    # Test 16: delete with negative index
    # ---------------------------------------
    f3 = Fasta()
    f3.headers = ["a", "b", "c"]
    f3.seqs = ["AAA", "CCC", "GGG"]
    f3.delete(-1)
    assert f3.headers == ["a", "b"]
    assert f3.seqs == ["AAA", "CCC"]

    # ---------------------------------------
    # Test 17: verify all with DNA
    # ---------------------------------------
    f4 = Fasta()
    f4.headers = ["s1", "s2", "s3"]
    f4.seqs = ["ATCG", "GGTT", "AAAA"]
    assert f4.verify(Fasta.DNA) is True

    # ---------------------------------------
    # Test 18: verify fails on invalid DNA
    # ---------------------------------------
    f4.seqs = ["ATCG", "GGXT", "AAAA"]
    assert f4.verify(Fasta.DNA) is False

    # ---------------------------------------
    # Test 19: verify slice
    # ---------------------------------------
    assert f4.verify(Fasta.DNA, 0, 1) is True
    assert f4.verify(Fasta.DNA, 1, 2) is False

    # ---------------------------------------
    # Test 20: verify single position
    # ---------------------------------------
    assert f4.verify(Fasta.DNA, 0) is True
    assert f4.verify(Fasta.DNA, 1) is False

    # ---------------------------------------
    # Test 21: verify with alphabet name string
    # ---------------------------------------
    f4.seqs = ["AUCG", "AUUG"]
    assert f4.verify("RNA") is True
    assert f4.verify(Fasta.RNA) is True

    # ---------------------------------------
    # Test 22: verify with custom alphabet
    # ---------------------------------------
    f4.seqs = ["ABBA", "AABB"]
    assert f4.verify("AB") is True
    f4.seqs = ["ABBA", "AABCX"]
    assert f4.verify("AB") is False

    # ---------------------------------------
    # Test 23: discard all invalid DNA entries
    # ---------------------------------------
    f5 = Fasta()
    f5.headers = ["h1", "h2", "h3", "h4"]
    f5.seqs = ["ATCG", "ATXG", "GGTT", "NNNN"]
    f5.discard(Fasta.DNA)
    assert f5.headers == ["h1", "h3"]
    assert f5.seqs == ["ATCG", "GGTT"]

    # ---------------------------------------
    # Test 24: discard only inside slice
    # ---------------------------------------
    f5 = Fasta()
    f5.headers = ["h1", "h2", "h3", "h4"]
    f5.seqs = ["ATCG", "ATXG", "GGTT", "NNNN"]
    f5.discard(Fasta.DNA, 1, 3)
    assert f5.headers == ["h1", "h3", "h4"]
    assert f5.seqs == ["ATCG", "GGTT", "NNNN"]

    # ---------------------------------------
    # Test 25: discard single index
    # ---------------------------------------
    f5 = Fasta()
    f5.headers = ["h1", "h2", "h3"]
    f5.seqs = ["ATCG", "ATXG", "GGTT"]
    f5.discard(Fasta.DNA, 1)
    assert f5.headers == ["h1", "h3"]
    assert f5.seqs == ["ATCG", "GGTT"]

    # ---------------------------------------
    # Test 26: save and load
    # ---------------------------------------
    f6 = Fasta()
    f6.headers = ["seq1", "seq2"]
    f6.seqs = ["ATCGATCG", "GGTTAAAA"]

    testfile = "test_fasta_output.fsa"
    f6.save(testfile)

    f7 = Fasta()
    f7.load(testfile)

    assert f7.headers == ["seq1", "seq2"]
    assert f7.seqs == ["ATCGATCG", "GGTTAAAA"]

    os.remove(testfile)

    # ---------------------------------------
    # Test 27: __init__ with filename
    # ---------------------------------------
    testfile2 = "test_fasta_init.fsa"
    with open(testfile2, "w") as ftmp:
        ftmp.write(">a\n")
        ftmp.write("ATCG\n")
        ftmp.write(">b\n")
        ftmp.write("GGTT\n")

    f8 = Fasta(testfile2)
    assert f8.headers == ["a", "b"]
    assert f8.seqs == ["ATCG", "GGTT"]

    os.remove(testfile2)

    # ---------------------------------------
    # Test 28: load should fail if seq before header
    # ---------------------------------------
    badfile = "bad_fasta.fsa"
    with open(badfile, "w") as ftmp:
        ftmp.write("ATCG\n")
        ftmp.write(">a\n")
        ftmp.write("GGTT\n")

    f9 = Fasta()
    try:
        f9.load(badfile)
        os.remove(badfile)
        assert False, "Expected ValueError for invalid FASTA format"
    except ValueError:
        os.remove(badfile)

    print("All Fasta tests passed successfully!")


run_fasta_tests()