########### ----------- Exercise 01 ----------- ###########

'''
    I already use a repo so I will just continoue in this
'''

import requests


def download_file(url, output_path):
    response = requests.get(url)
    response.raise_for_status()
    with open(output_path, "wb") as f:
        f.write(response.content)

download_file("https://teaching.healthtech.dtu.dk/material/22118/scores.txt", "scores.txt")
download_file("https://teaching.healthtech.dtu.dk/material/22118/negative_list.txt", "negative_list.txt")
download_file("https://teaching.healthtech.dtu.dk/material/22118/translation.txt", "translation.txt")


########### ----------- Exercise 02 ----------- ###########
import pandas as pd

def main():
    filename = "scores.txt"

    # Load data
    df = pd.read_csv(filename, sep="\t", names=['id', '1', '2', '3', '4', '5', '6'])

    # Sum cols and order
    df["Total"] = df.iloc[:, 1:].sum(axis=1)
    df = df[['id', 'Total']]
    df = df.sort_values(by="Total", ascending=False)

    # Save the top 10 in a new df
    df_top = df.head(10)
    df_tail = df.tail(10)
    df_extreme = pd.concat([df_top, df_tail])
    df_extreme.to_csv("scores_extreme.txt", sep="\t", index=False)

    print(df_extreme)

main()


'''
id	Total
LF808489.CDS.1	5.27661
KI133588.CDS.2	5.24153
JB80314549.CDS.3	5.178749999999999
J21782.CDS.3	5.1575500000000005
UY37981324.CDS.1	5.11245
CR97613151.CDS.1	5.09985
TF58157495	5.07904
N33521.CDS.1	5.037459999999999
E15922.CDS.2	5.02731
HD25632497.CDS.1	5.004689999999999
JP12357726.CDS.1	1.1492099999999998
IE553374.CDS.1	1.1350500000000001
FS53962190.CDS.1	1.12183
ZL767849.CDS.1	1.07654
QS900083.CDS.1	1.05693
ND80117744.CDS.1	1.04862
KP22241601.CDS.2	0.9468399999999999
YM29518703.CDS.1	0.9122
BQ99602892.CDS.1	0.7989900000000001
BE618688.CDS.1	0.78614
'''


########### ----------- Exercise 03 ----------- ###########
import pandas as pd

def main():
    filename_scores = "scores.txt"
    filename_negative = "negative_list.txt"
    filename_translation = "translation.txt"

    # Load data
    df_scores = pd.read_csv(
        filename_scores,
        sep="\t",
        names=["accession", "1", "2", "3", "4", "5", "6"],
        dtype={"accession": "string"},
    )

    df_negative = pd.read_csv(
        filename_negative,
        sep="\t",
        names=["swissprot"],
        dtype={"swissprot": "string"},
    )

    df_translation = pd.read_csv(
        filename_translation,
        sep="\t",
        names=["accession", "swissprot"],
        dtype={"accession": "string", "swissprot": "string"},
    )

    df_negative_map = df_negative.merge(df_translation, on="swissprot", how="left") # Get the accessions for the negative list
    negative_accessions = df_negative_map["accession"].dropna().unique() # Drop if no match
    df_filtered = df_scores[~df_scores["accession"].isin(negative_accessions)].copy() # Filter out the negative accessions

    # Sum the cols and order
    score_cols = ["1", "2", "3", "4", "5", "6"]
    df_filtered[score_cols] = df_filtered[score_cols].apply(
        pd.to_numeric, errors="coerce"
    ).fillna(0)
    df_filtered["Total"] = df_filtered[score_cols].sum(axis=1)
    df_ranked = df_filtered[["accession", "Total"]].sort_values("Total", ascending=False)

    # Save outputs
    df_filtered.to_csv("scores_filtered.txt", sep="\t", index=False)      # includes Total
    df_extreme = pd.concat([df_ranked.head(10), df_ranked.tail(10)])
    df_extreme.to_csv("scores_extreme.txt", sep="\t", index=False)

    print(df_extreme.to_string(index=False))

if __name__ == "__main__":
    main()

########### ----------- Exercise 04 ----------- ###########
import pandas as pd

def main():
    filename = "scores.txt"

    df = pd.read_csv(filename, sep="\t", header=None)
    df = df.rename(columns={0: "id"})

    df["Total"] = df.iloc[:, 1:].sum(axis=1)
    df = df[['id', 'Total']]
    df = df.sort_values(by="Total", ascending=False)

    df_top = df.head(10)
    df_tail = df.tail(10)
    df_extreme = pd.concat([df_top, df_tail])
    df_extreme.to_csv("scores_extreme.txt", sep="\t", index=False)

    print(df_extreme)

main()
########### ----------- Exercise 05 ----------- ###########
import pandas as pd

def main():
    filename = "scores.txt"

    # Load data
    df = pd.read_csv(filename, sep="\t", header=None)

    # Compute average and order
    df["Average"] = df.iloc[:, 1:].mean(axis=1)
    df = df[[0, "Average"]]
    df.columns = ["id", "Average"]
    df = df.sort_values(by="Average", ascending=False)

    # Save the top 10 in a new df
    df_top = df.head(10)
    df_tail = df.tail(10)
    df_extreme = pd.concat([df_top, df_tail])
    df_extreme.to_csv("scores_extreme.txt", sep="\t", index=False)

    print(df_extreme)

main()
########### ----------- Exercise 06 ----------- ###########
import pandas as pd

def main():
    filename = "scores.txt"

    # Load data
    df = pd.read_csv(filename, sep="\t", names=['id', '1', '2', '3', '4', '5', '6'])

    # Apply weights and sum cols
    weights = [1.5, 1, 1, 1, 1, 0.5]
    df["Total"] = (df.iloc[:, 1:] * weights).sum(axis=1)

    # Order
    df = df[['id', 'Total']]
    df = df.sort_values(by="Total", ascending=False)

    # Save the top 10 in a new df
    df_top = df.head(10)
    df_tail = df.tail(10)
    df_extreme = pd.concat([df_top, df_tail])
    df_extreme.to_csv("scores_extreme.txt", sep="\t", index=False)

    print(df_extreme)

main()
########### ----------- Exercise 07 ----------- ###########
import pandas as pd

def main():
    filename = "scores.txt"

    # Load data
    df = pd.read_csv(filename, sep="\t", names=['id', '1', '2', '3', '4', '5', '6'])

    # Weighted sum cols (linear sliding scale)
    values = df.iloc[:, 1:]
    N = values.shape[1]  # number of numbers on the line

    # W = B - (B - E) * (P - 1) / (N - 1)
    B, E = 1.5, 0.5
    if N == 1:
        weights = pd.Series([B], index=values.columns)
    else:
        P = pd.Series(range(1, N + 1), index=values.columns)  # positions 1..N
        weights = B - (B - E) * (P - 1) / (N - 1)

    df["Total"] = values.mul(weights, axis=1).sum(axis=1)
    df = df[['id', 'Total']]
    df = df.sort_values(by="Total", ascending=False)

    # Save the top 10 and bottom 10 in a new df
    df_top = df.head(10)
    df_tail = df.tail(10)
    df_extreme = pd.concat([df_top, df_tail])
    df_extreme.to_csv("scores_extreme.txt", sep="\t", index=False)

    print(df_extreme)

main()
########### ----------- Exercise 08 ----------- ###########
import pandas as pd

def main():
    filename = "scores.txt"

    # Load data
    df = pd.read_csv(filename, sep="\t", names=['id', '1', '2', '3', '4', '5', '6'])

    # Sum cols and order
    df["Total"] = df.iloc[:, 1:].sum(axis=1)
    df = df[['id', 'Total']]
    df = df.sort_values(by="Total", ascending=False)

    # Save the top 10 in a new df
    df_top = df.head(10)
    df_top.to_csv("scores_top10.txt", sep="\t", index=False)

    print(df_top)

main()

########### ----------- Exercise 09 ----------- ###########
import pandas as pd

def main():
    filename = "scores.txt"
    k = 10
    cols = ['id', '1', '2', '3', '4', '5', '6']
    best = pd.DataFrame(columns=['id', 'Total'])

    for chunk in pd.read_csv(filename, sep="\t", names=cols, chunksize=200_000):
        # Sum cols and order
        chunk["Total"] = chunk.iloc[:, 1:].sum(axis=1)
        chunk = chunk[['id', 'Total']]

        best = pd.concat([best, chunk], ignore_index=True)
        best = best.nlargest(k, "Total")  # keep only top k so memory stays tiny

    # Save the top 10
    best = best.sort_values("Total", ascending=False)
    best.to_csv("scores_top10.txt", sep="\t", index=False)

    print(best)

main()