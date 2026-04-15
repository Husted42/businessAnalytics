import asyncio
import csv
from playwright.async_api import async_playwright

PROFILE_URL = "https://truthsocial.com/@realDonaldTrump"

async def main():
    results = []

    async with async_playwright() as p:
        browser = await p.chromium.launch(headless=False)
        page = await browser.new_page()
        await page.goto(PROFILE_URL, wait_until="domcontentloaded", timeout=120000)
        await page.wait_for_timeout(5000)

        seen_ids = set()
        stagnant = 0
        last_seen_count = 0

        for _ in range(200):
            posts = page.locator('[data-testid="status"]')
            count = await posts.count()
            print("visible posts:", count)

            for i in range(count):
                post = posts.nth(i)
                try:
                    post_id = await post.get_attribute("id")
                    if not post_id or post_id in seen_ids:
                        continue
                    seen_ids.add(post_id)

                    text_locator = post.locator('[data-testid="markup"]')
                    text = (await text_locator.inner_text()).strip() if await text_locator.count() else ""

                    link_locator = post.locator('a[href*="/@realDonaldTrump/posts/"]').first
                    href = await link_locator.get_attribute("href") if await link_locator.count() else ""

                    time_locator = post.locator("time").first
                    time_text = await time_locator.inner_text() if await time_locator.count() else ""
                    time_title = await time_locator.get_attribute("title") if await time_locator.count() else ""

                    aria_label = await post.get_attribute("aria-label")

                    results.append({
                        "post_id": post_id,
                        "text": text,
                        "href": f"https://truthsocial.com{href}" if href and href.startswith("/") else href,
                        "time_text": time_text,
                        "time_title": time_title,
                        "aria_label": aria_label,
                    })

                    print("saved:", post_id, text[:80])

                except Exception as e:
                    print("skip:", i, e)

            await page.evaluate("window.scrollTo(0, document.body.scrollHeight)")
            await page.wait_for_timeout(2500)

            if len(seen_ids) == last_seen_count:
                stagnant += 1
            else:
                stagnant = 0
            last_seen_count = len(seen_ids)

            if stagnant >= 6:
                break

        await browser.close()

    with open("donald_trump_truths_scraped.csv", "w", newline="", encoding="utf-8") as f:
        writer = csv.DictWriter(
            f,
            fieldnames=["post_id", "text", "href", "time_text", "time_title", "aria_label"]
        )
        writer.writeheader()
        writer.writerows(results)

    print(f"Saved {len(results)} posts")

asyncio.run(main())