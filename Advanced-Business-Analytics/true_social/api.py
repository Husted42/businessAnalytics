import json
import pandas as pd

with open("realDonaldTrump_truths.json", "r", encoding="utf-8") as f:
    data = json.load(f)

rows = []
for post in data:
    rows.append({
        "id": post.get("id"),
        "created_at": post.get("created_at"),
        "url": post.get("url"),
        "content": post.get("content"),
        "replies_count": post.get("replies_count"),
        "reblogs_count": post.get("reblogs_count"),
        "favourites_count": post.get("favourites_count"),
    })

pd.DataFrame(rows).to_csv("realDonaldTrump_truths.csv", index=False)
print("Saved realDonaldTrump_truths.csv")