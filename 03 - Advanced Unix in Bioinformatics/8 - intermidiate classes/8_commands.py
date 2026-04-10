#################################### ------ Excericse 1-4 ------ ####################################
'''
    Contines code from last week, and added the new features.
    Sicnce there is a lot of code I have combined the exercises into 2 classes.
'''

class Fasta:
    DNA = "ATCG"
    RNA = "AUCG"
    PROTEIN = "ACDEFGHIKLMNPQRSTVWY"

    ### --- Initialization --- ###
    # Initialize empty Fasta object or load from file
    def __init__(self, filename=None):
        self.headers = []
        self.seqs = []

        # Iteration state
        self._iter_index = 0
        self._current_index = None
        self._iterating = False

        if filename is not None:
            self.load(filename)

    ### --- Exercise 1: Length & Iteration --- ###
    # Return number of sequences
    def __len__(self):
        return len(self.seqs)

    # Initialize iterator
    def __iter__(self):
        self._iter_index = 0
        self._current_index = None
        self._iterating = True
        return self

    # Return next (header, sequence) pair
    def __next__(self):
        if self._iter_index >= len(self.seqs):
            self._current_index = None
            self._iterating = False
            raise StopIteration

        self._current_index = self._iter_index
        result = (self.headers[self._current_index], self.seqs[self._current_index])
        self._iter_index += 1
        return result

    ### --- Helper Functions --- ###
    # Ensure inputs are lists and match in length
    def _normalize_to_lists(self, headers, seqs):
        if isinstance(headers, str):
            headers = [headers]
        if isinstance(seqs, str):
            seqs = [seqs]

        if len(headers) != len(seqs):
            raise ValueError("Headers and sequences must have the same length")

        return headers, seqs

    # Convert alphabet name to actual alphabet string
    def _normalize_alphabet(self, alphabet):
        if alphabet == "DNA":
            return self.DNA
        if alphabet == "RNA":
            return self.RNA
        if alphabet == "PROTEIN":
            return self.PROTEIN
        return alphabet

    ### --- Basic Insert --- ###
    # Insert sequences at position or end
    def insert(self, headers, seqs, pos=None):
        headers, seqs = self._normalize_to_lists(headers, seqs)

        if pos is None:
            pos = len(self.headers)

        for i, (h, s) in enumerate(zip(headers, seqs)):
            self.headers.insert(pos + i, h)
            self.seqs.insert(pos + i, s)

    ### --- Exercise 2: insertthis --- ###
    # Insert elements at current iteration position
    def insertthis(self, headers, seqs):
        if not self._iterating or self._current_index is None:
            raise RuntimeError("insertthis() can only be used during iteration at a current item")

        headers, seqs = self._normalize_to_lists(headers, seqs)
        n = len(headers)

        self.insert(headers, seqs, self._current_index)

        # Skip inserted elements and already visited element
        self._iter_index += n
        self._current_index += n

    ### --- Content Access --- ###
    # Return full content, single item, or slice
    def content(self, start=None, end=None):
        if start is None and end is None:
            return self.headers[:], self.seqs[:]

        if end is None:
            return self.headers[start], self.seqs[start]

        return self.headers[start:end], self.seqs[start:end]

    ### --- Delete --- ###
    # Delete full, single, or slice of entries
    def delete(self, start=None, end=None):
        if start is None and end is None:
            self.headers = []
            self.seqs = []
            return

        if end is None:
            del self.headers[start]
            del self.seqs[start]
            return

        del self.headers[start:end]
        del self.seqs[start:end]

    ### --- Exercise 2: deletethis --- ###
    # Delete current element during iteration
    def deletethis(self):
        if not self._iterating or self._current_index is None:
            raise RuntimeError("deletethis() can only be used during iteration at a current item")

        idx = self._current_index
        self.delete(idx)

        # Adjust iterator after deletion
        self._iter_index -= 1
        self._current_index = None

    ### --- Verify --- ###
    # Check if sequences match alphabet
    def verify(self, alphabet, start=None, end=None):
        alphabet = self._normalize_alphabet(alphabet)

        if start is None and end is None:
            seqs = self.seqs
        elif end is None:
            seqs = [self.seqs[start]]
        else:
            seqs = self.seqs[start:end]

        allowed = set(alphabet)
        return all(all(char in allowed for char in seq) for seq in seqs)

    ### --- Exercise 2: verifythis --- ###
    # Verify current sequence during iteration
    def verifythis(self, alphabet):
        if not self._iterating or self._current_index is None:
            raise RuntimeError("verifythis() can only be used during iteration at a current item")

        return self.verify(alphabet, self._current_index)

    ### --- Discard --- ###
    # Remove invalid sequences based on alphabet
    def discard(self, alphabet, start=None, end=None):
        alphabet = self._normalize_alphabet(alphabet)
        allowed = set(alphabet)

        if start is None and end is None:
            indices = range(len(self.seqs))
        elif end is None:
            indices = [start]
        else:
            indices = range(start, end)

        keep_headers = []
        keep_seqs = []

        indices_set = set(indices)

        for i, (h, s) in enumerate(zip(self.headers, self.seqs)):
            valid = all(c in allowed for c in s)
            if i in indices_set:
                if valid:
                    keep_headers.append(h)
                    keep_seqs.append(s)
            else:
                keep_headers.append(h)
                keep_seqs.append(s)

        self.headers = keep_headers
        self.seqs = keep_seqs

    ### --- Exercise 2: discardthis --- ###
    # Remove current sequence if invalid
    def discardthis(self, alphabet):
        if not self._iterating or self._current_index is None:
            raise RuntimeError("discardthis() can only be used during iteration at a current item")

        if not self.verifythis(alphabet):
            self.deletethis()

    ### --- File IO --- ###
    # Save Fasta to file
    def save(self, filename):
        with open(filename, "w") as f:
            for h, s in zip(self.headers, self.seqs):
                f.write(f">{h}\n")
                f.write(f"{s}\n")

    # Load Fasta from file
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
                    current_header = line[1:]
                    current_seq = []
                else:
                    if current_header is None:
                        raise ValueError("Invalid FASTA format: sequence before header")
                    current_seq.append(line)

        if current_header is not None:
            self.headers.append(current_header)
            self.seqs.append("".join(current_seq))

    ### --- Exercise 3: __iadd__ --- ###
    # In-place addition (+=)
    def __iadd__(self, other):
        if not isinstance(other, Fasta):
            return NotImplemented

        self.headers.extend(other.headers)
        self.seqs.extend(other.seqs)
        return self

    ### --- Exercise 4: __add__ --- ###
    # Create new Fasta by combining two
    def __add__(self, other):
        if not isinstance(other, Fasta):
            return NotImplemented

        new_fasta = Fasta()
        new_fasta.headers = self.headers[:] + other.headers[:]
        new_fasta.seqs = self.seqs[:] + other.seqs[:]
        return new_fasta
    
def run_new_fasta_feature_tests():
    # ---------------------------------------
    # Test 1: __len__ on empty Fasta
    # ---------------------------------------
    f = Fasta()
    assert len(f) == 0

    # ---------------------------------------
    # Test 2: __len__ after inserts
    # ---------------------------------------
    f.insert(["h1", "h2", "h3"], ["ATCG", "GGTT", "AAAA"])
    assert len(f) == 3

    # ---------------------------------------
    # Test 3: __iter__ basic iteration order
    # ---------------------------------------
    collected = []
    for header, sequence in f:
        collected.append((header, sequence))

    assert collected == [("h1", "ATCG"), ("h2", "GGTT"), ("h3", "AAAA")]

    # ---------------------------------------
    # Test 4: len usable in if statement
    # ---------------------------------------
    seen = []
    if len(f) > 0:
        for header, sequence in f:
            seen.append(header)

    assert seen == ["h1", "h2", "h3"]

    # ---------------------------------------
    # Test 5: verifythis() on current item
    # ---------------------------------------
    f2 = Fasta()
    f2.insert(["a", "b", "c"], ["ATCG", "ATXG", "GGTT"])

    results = []
    for header, sequence in f2:
        results.append(f2.verifythis("DNA"))

    assert results == [True, False, True]

    # ---------------------------------------
    # Test 6: deletethis() removes invalid entries during iteration
    # ---------------------------------------
    f3 = Fasta()
    f3.insert(["a", "b", "c", "d"], ["ATCG", "ATXG", "GGTT", "AXAA"])

    visited = []
    for header, sequence in f3:
        visited.append(header)
        if not f3.verifythis("DNA"):
            f3.deletethis()

    # We should still have visited all original entries once
    assert visited == ["a", "b", "c", "d"]

    # Invalid DNA entries should be removed
    assert f3.headers == ["a", "c"]
    assert f3.seqs == ["ATCG", "GGTT"]

    # ---------------------------------------
    # Test 7: discardthis() removes current invalid entry
    # ---------------------------------------
    f4 = Fasta()
    f4.insert(["h1", "h2", "h3"], ["ATCG", "ATXG", "GGTT"])

    for header, sequence in f4:
        f4.discardthis("DNA")

    assert f4.headers == ["h1", "h3"]
    assert f4.seqs == ["ATCG", "GGTT"]

    # ---------------------------------------
    # Test 8: insertthis() inserts at current position
    # ---------------------------------------
    f5 = Fasta()
    f5.insert(["a", "b"], ["ATCG", "GGTT"])

    visited = []
    for header, sequence in f5:
        visited.append(header)
        if header == "a":
            f5.insertthis("x", "AAAA")

    # By chosen behavior:
    # inserted element is NOT visited in this same iteration
    assert visited == ["a", "b"]

    # But it should be inserted into the object
    assert f5.headers == ["x", "a", "b"]
    assert f5.seqs == ["AAAA", "ATCG", "GGTT"]

    # ---------------------------------------
    # Test 9: insertthis() with multiple inserted entries
    # ---------------------------------------
    f6 = Fasta()
    f6.insert(["a", "b"], ["ATCG", "GGTT"])

    visited = []
    for header, sequence in f6:
        visited.append(header)
        if header == "a":
            f6.insertthis(["x1", "x2"], ["AAAA", "CCCC"])

    assert visited == ["a", "b"]
    assert f6.headers == ["x1", "x2", "a", "b"]
    assert f6.seqs == ["AAAA", "CCCC", "ATCG", "GGTT"]

    # ---------------------------------------
    # Test 10: inserted elements appear in later fresh iteration
    # ---------------------------------------
    collected = []
    for header, sequence in f6:
        collected.append(header)

    assert collected == ["x1", "x2", "a", "b"]

    # ---------------------------------------
    # Test 11: deletethis() should only work during iteration
    # ---------------------------------------
    f7 = Fasta()
    f7.insert("a", "ATCG")

    try:
        f7.deletethis()
        assert False, "Expected exception when calling deletethis() outside iteration"
    except Exception:
        pass

    # ---------------------------------------
    # Test 12: insertthis() should only work during iteration
    # ---------------------------------------
    try:
        f7.insertthis("x", "AAAA")
        assert False, "Expected exception when calling insertthis() outside iteration"
    except Exception:
        pass

    # ---------------------------------------
    # Test 13: verifythis() should only work during iteration
    # ---------------------------------------
    try:
        f7.verifythis("DNA")
        assert False, "Expected exception when calling verifythis() outside iteration"
    except Exception:
        pass

    # ---------------------------------------
    # Test 14: discardthis() should only work during iteration
    # ---------------------------------------
    try:
        f7.discardthis("DNA")
        assert False, "Expected exception when calling discardthis() outside iteration"
    except Exception:
        pass

    # ---------------------------------------
    # Test 15: __iadd__ appends contents to same object
    # ---------------------------------------
    f8 = Fasta()
    f8.insert(["h1", "h2"], ["ATCG", "GGTT"])

    f9 = Fasta()
    f9.insert(["p1", "p2"], ["MKKL", "VVAA"])

    f8 += f9

    assert f8.headers == ["h1", "h2", "p1", "p2"]
    assert f8.seqs == ["ATCG", "GGTT", "MKKL", "VVAA"]

    # Right-hand side should remain unchanged
    assert f9.headers == ["p1", "p2"]
    assert f9.seqs == ["MKKL", "VVAA"]

    # ---------------------------------------
    # Test 16: __add__ creates new Fasta object
    # ---------------------------------------
    f10 = Fasta()
    f10.insert(["a"], ["ATCG"])

    f11 = Fasta()
    f11.insert(["b"], ["GGTT"])

    f12 = f10 + f11

    assert isinstance(f12, Fasta)
    assert f12.headers == ["a", "b"]
    assert f12.seqs == ["ATCG", "GGTT"]

    # Originals must remain unchanged
    assert f10.headers == ["a"]
    assert f10.seqs == ["ATCG"]
    assert f11.headers == ["b"]
    assert f11.seqs == ["GGTT"]

    # ---------------------------------------
    # Test 17: __add__ result independent of originals
    # ---------------------------------------
    f12.headers.append("c")
    f12.seqs.append("AAAA")

    assert f10.headers == ["a"]
    assert f10.seqs == ["ATCG"]
    assert f11.headers == ["b"]
    assert f11.seqs == ["GGTT"]

    # ---------------------------------------
    # Test 18: __iadd__ followed by discard to remove protein entries
    # ---------------------------------------
    f13 = Fasta()
    f13.insert(["dna1", "dna2"], ["ATCG", "GGTT"])

    f14 = Fasta()
    f14.insert(["prot1", "prot2"], ["MKKL", "VVAA"])

    f13 += f14
    assert f13.headers == ["dna1", "dna2", "prot1", "prot2"]

    # Keep only DNA-valid sequences
    f13.discard("DNA")
    assert f13.headers == ["dna1", "dna2"]
    assert f13.seqs == ["ATCG", "GGTT"]

    print("All tests for the 4 new Fasta features passed successfully!")


run_new_fasta_feature_tests()



#################################### ------ Excericse 5-6 ------ ####################################
'''
    Contines code from last week, and added the new features.
'''
class FastaSet(Fasta):

    ### --- Helper: identifiers --- ###
    # Return list of identifiers from headers
    def identifiers(self):
        return [header.split()[0] for header in self.headers]

    ### --- Helper: identifier lookup --- ###
    # Return indices matching given identifiers
    def _indices_from_ids(self, ids):
        if ids is None:
            return list(range(len(self.headers)))

        if isinstance(ids, str):
            ids = [ids]

        ids_set = set(ids)
        all_ids = self.identifiers()
        return [i for i, identifier in enumerate(all_ids) if identifier in ids_set]

    ### --- Override content --- ###
    # Return all content or content matching identifiers
    def content(self, ids=None):
        if ids is None:
            return self.headers[:], self.seqs[:]

        indices = self._indices_from_ids(ids)
        headers = [self.headers[i] for i in indices]
        seqs = [self.seqs[i] for i in indices]
        return headers, seqs

    ### --- Override delete --- ###
    # Delete entries matching identifiers, or all if none given
    def delete(self, ids=None):
        if ids is None:
            self.headers = []
            self.seqs = []
            return

        indices_to_delete = set(self._indices_from_ids(ids))
        self.headers = [h for i, h in enumerate(self.headers) if i not in indices_to_delete]
        self.seqs = [s for i, s in enumerate(self.seqs) if i not in indices_to_delete]

    ### --- Override verify --- ###
    # Verify all or only entries matching identifiers
    def verify(self, alphabet, ids=None):
        alphabet = self._normalize_alphabet(alphabet)
        allowed = set(alphabet)

        indices = self._indices_from_ids(ids)
        seqs = [self.seqs[i] for i in indices]

        return all(all(char in allowed for char in seq) for seq in seqs)

    ### --- Override discard --- ###
    # Discard invalid entries among selected identifiers
    def discard(self, alphabet, ids=None):
        alphabet = self._normalize_alphabet(alphabet)
        allowed = set(alphabet)

        indices_to_check = set(self._indices_from_ids(ids))

        new_headers = []
        new_seqs = []

        for i, (h, s) in enumerate(zip(self.headers, self.seqs)):
            valid = all(char in allowed for char in s)

            if i in indices_to_check:
                if valid:
                    new_headers.append(h)
                    new_seqs.append(s)
            else:
                new_headers.append(h)
                new_seqs.append(s)

        self.headers = new_headers
        self.seqs = new_seqs

    ### --- Helper for set operations --- ###
    # Return mapping from identifier to (header, sequence)
    def _id_map(self):
        return {
            header.split()[0]: (header, seq)
            for header, seq in zip(self.headers, self.seqs)
        }

    ### --- Set operation: union --- ###
    # Return new FastaSet with identifiers from both
    def __or__(self, other):
        if not isinstance(other, FastaSet):
            return NotImplemented

        result = FastaSet()

        self_map = self._id_map()
        other_map = other._id_map()

        seen = set()

        for header, seq in zip(self.headers, self.seqs):
            identifier = header.split()[0]
            if identifier not in seen:
                result.headers.append(header)
                result.seqs.append(seq)
                seen.add(identifier)

        for header, seq in zip(other.headers, other.seqs):
            identifier = header.split()[0]
            if identifier not in seen:
                result.headers.append(header)
                result.seqs.append(seq)
                seen.add(identifier)

        return result

    ### --- Set operation: intersection --- ###
    # Return new FastaSet with identifiers found in both
    def __and__(self, other):
        if not isinstance(other, FastaSet):
            return NotImplemented

        result = FastaSet()

        other_ids = set(other.identifiers())

        for header, seq in zip(self.headers, self.seqs):
            identifier = header.split()[0]
            if identifier in other_ids:
                result.headers.append(header)
                result.seqs.append(seq)

        return result

    ### --- Set operation: difference --- ###
    # Return new FastaSet with identifiers only in self
    def __sub__(self, other):
        if not isinstance(other, FastaSet):
            return NotImplemented

        result = FastaSet()

        other_ids = set(other.identifiers())

        for header, seq in zip(self.headers, self.seqs):
            identifier = header.split()[0]
            if identifier not in other_ids:
                result.headers.append(header)
                result.seqs.append(seq)

        return result

    ### --- Set operation: symmetric difference --- ###
    # Return new FastaSet with identifiers in one but not both
    def __xor__(self, other):
        if not isinstance(other, FastaSet):
            return NotImplemented

        result = FastaSet()

        self_ids = set(self.identifiers())
        other_ids = set(other.identifiers())

        for header, seq in zip(self.headers, self.seqs):
            identifier = header.split()[0]
            if identifier not in other_ids:
                result.headers.append(header)
                result.seqs.append(seq)

        for header, seq in zip(other.headers, other.seqs):
            identifier = header.split()[0]
            if identifier not in self_ids:
                result.headers.append(header)
                result.seqs.append(seq)

        return result
    

def run_fasta_set_tests():
    print("Running FastaSet tests...")

    # ---------------------------------------
    # Test 1: identifiers()
    # ---------------------------------------
    fs = FastaSet()
    fs.insert(["id1 desc", "id2 something", "id3"], ["ATCG", "GGTT", "AAAA"])
    assert fs.identifiers() == ["id1", "id2", "id3"]

    # ---------------------------------------
    # Test 2: content() with identifiers
    # ---------------------------------------
    headers, seqs = fs.content(["id1", "id3"])
    assert headers == ["id1 desc", "id3"]
    assert seqs == ["ATCG", "AAAA"]

    # ---------------------------------------
    # Test 3: content() with single identifier
    # ---------------------------------------
    headers, seqs = fs.content("id2")
    assert headers == ["id2 something"]
    assert seqs == ["GGTT"]

    # ---------------------------------------
    # Test 4: content() full
    # ---------------------------------------
    headers, seqs = fs.content()
    assert headers == ["id1 desc", "id2 something", "id3"]
    assert seqs == ["ATCG", "GGTT", "AAAA"]

    # ---------------------------------------
    # Test 5: delete() with identifiers
    # ---------------------------------------
    fs2 = FastaSet()
    fs2.insert(["id1", "id2", "id3"], ["ATCG", "GGTT", "AAAA"])
    fs2.delete(["id2"])
    assert fs2.headers == ["id1", "id3"]
    assert fs2.seqs == ["ATCG", "AAAA"]

    # ---------------------------------------
    # Test 6: delete() all
    # ---------------------------------------
    fs2.delete()
    assert fs2.headers == []
    assert fs2.seqs == []

    # ---------------------------------------
    # Test 7: verify() with identifiers
    # ---------------------------------------
    fs3 = FastaSet()
    fs3.insert(["id1", "id2", "id3"], ["ATCG", "ATXG", "GGTT"])

    assert fs3.verify("DNA", ["id1", "id3"]) is True
    assert fs3.verify("DNA", ["id2"]) is False

    # ---------------------------------------
    # Test 8: discard() with identifiers
    # ---------------------------------------
    fs4 = FastaSet()
    fs4.insert(["id1", "id2", "id3"], ["ATCG", "ATXG", "GGTT"])
    fs4.discard("DNA", ["id1", "id2"])

    # id2 removed, id3 untouched
    assert fs4.headers == ["id1", "id3"]
    assert fs4.seqs == ["ATCG", "GGTT"]

    # ---------------------------------------
    # Test 9: discard() full
    # ---------------------------------------
    fs5 = FastaSet()
    fs5.insert(["id1", "id2", "id3"], ["ATCG", "ATXG", "GGTT"])
    fs5.discard("DNA")

    assert fs5.headers == ["id1", "id3"]
    assert fs5.seqs == ["ATCG", "GGTT"]

    # ---------------------------------------
    # Test 10: union (|)
    # ---------------------------------------
    a = FastaSet()
    a.insert(["id1", "id2"], ["ATCG", "GGTT"])

    b = FastaSet()
    b.insert(["id2", "id3"], ["CCCC", "AAAA"])

    u = a | b
    assert u.identifiers() == ["id1", "id2", "id3"]

    # id2 should come from 'a'
    assert u.seqs == ["ATCG", "GGTT", "AAAA"]

    # ---------------------------------------
    # Test 11: intersection (&)
    # ---------------------------------------
    i = a & b
    assert i.identifiers() == ["id2"]
    assert i.seqs == ["GGTT"]

    # ---------------------------------------
    # Test 12: difference (-)
    # ---------------------------------------
    d = a - b
    assert d.identifiers() == ["id1"]
    assert d.seqs == ["ATCG"]

    # ---------------------------------------
    # Test 13: symmetric difference (^)
    # ---------------------------------------
    x = a ^ b
    assert set(x.identifiers()) == {"id1", "id3"}
    assert len(x) == 2

    # ---------------------------------------
    # Test 14: original objects unchanged
    # ---------------------------------------
    assert a.identifiers() == ["id1", "id2"]
    assert b.identifiers() == ["id2", "id3"]

    # ---------------------------------------
    # Test 15: content after set operation
    # ---------------------------------------
    headers, seqs = u.content(["id3"])
    assert headers == ["id3"]
    assert seqs == ["AAAA"]

    print("All FastaSet tests passed successfully!")


run_fasta_set_tests()