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