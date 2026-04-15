import sys
import os
import unittest
import tempfile

sys.path.append('C:\\Users\\huste\\Documents\\Github\\businessAnalytics\\03 - Advanced Unix in Bioinformatics\\9 - Unit test\\Handin\\src')

from fasta import Fasta


class TestFastaSaveDelete(unittest.TestCase):

    ### ----------- DELETE TESTS ----------- ###

    def test_delete_single_entry(self):
        f = Fasta()
        f.insert(["seq1", "seq2", "seq3"], ["ATCG", "GGTT", "CCAA"])

        f.delete(1)

        self.assertEqual(f.headers, ["seq1", "seq3"])
        self.assertEqual(f.seqs, ["ATCG", "CCAA"])

    def test_delete_slice(self):
        f = Fasta()
        f.insert(["seq1", "seq2", "seq3", "seq4"], ["ATCG", "GGTT", "CCAA", "TTAA"])

        f.delete(1, 3)

        self.assertEqual(f.headers, ["seq1", "seq4"])
        self.assertEqual(f.seqs, ["ATCG", "TTAA"])

    def test_delete_all(self):
        f = Fasta()
        f.insert(["seq1", "seq2"], ["ATCG", "GGTT"])

        f.delete()

        self.assertEqual(f.headers, [])
        self.assertEqual(f.seqs, [])
        self.assertEqual(len(f), 0)

    def test_delete_only_entry(self):
        f = Fasta()
        f.insert("seq1", "ATCG")

        f.delete(0)

        self.assertEqual(f.headers, [])
        self.assertEqual(f.seqs, [])

    def test_delete_invalid_index_raises_error(self):
        f = Fasta()
        f.insert("seq1", "ATCG")

        with self.assertRaises(IndexError):
            f.delete(5)

    ### ----------- SAVE TESTS ----------- ###

    def test_save_single_sequence(self):
        f = Fasta()
        f.insert("seq1", "ATCG")

        with tempfile.NamedTemporaryFile(mode="r+", delete=False) as tmp:
            tmp_name = tmp.name

        try:
            f.save(tmp_name)

            with open(tmp_name, "r") as file:
                content = file.read()

            expected = ">seq1\nATCG\n"
            self.assertEqual(content, expected)

        finally:
            os.remove(tmp_name)

    def test_save_multiple_sequences(self):
        f = Fasta()
        f.insert(["seq1", "seq2", "seq3"], ["ATCG", "GGTT", "CCAA"])

        with tempfile.NamedTemporaryFile(mode="r+", delete=False) as tmp:
            tmp_name = tmp.name

        try:
            f.save(tmp_name)

            with open(tmp_name, "r") as file:
                content = file.read()

            expected = (
                ">seq1\n"
                "ATCG\n"
                ">seq2\n"
                "GGTT\n"
                ">seq3\n"
                "CCAA\n"
            )
            self.assertEqual(content, expected)

        finally:
            os.remove(tmp_name)

    def test_save_empty_fasta(self):
        f = Fasta()

        with tempfile.NamedTemporaryFile(mode="r+", delete=False) as tmp:
            tmp_name = tmp.name

        try:
            f.save(tmp_name)

            with open(tmp_name, "r") as file:
                content = file.read()

            self.assertEqual(content, "")

        finally:
            os.remove(tmp_name)

    def test_save_and_reload(self):
        f1 = Fasta()
        f1.insert(["seq1", "seq2"], ["ATCG", "GGTT"])

        with tempfile.NamedTemporaryFile(mode="r+", delete=False) as tmp:
            tmp_name = tmp.name

        try:
            f1.save(tmp_name)

            f2 = Fasta()
            f2.load(tmp_name)

            self.assertEqual(f2.headers, ["seq1", "seq2"])
            self.assertEqual(f2.seqs, ["ATCG", "GGTT"])

        finally:
            os.remove(tmp_name)


if __name__ == "__main__":
    unittest.main()