#!/usr/bin/env python3
import csv
import re
import sys
from pathlib import Path

try:
    import requests
    from bs4 import BeautifulSoup
except ImportError:
    print("missing dependencies. install with: pip install requests beautifulsoup4")
    sys.exit(1)


def fetch_html(url: str) -> str:
    headers = {
        "User-Agent": "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/120.0.0.0 Safari/537.36"
    }
    response = requests.get(url, headers=headers, timeout=30)
    response.raise_for_status()
    return response.text


def parse_champions(html: str) -> list[dict]:
    """parse champion data from html."""
    soup = BeautifulSoup(html, "html.parser")
    champions = []

    # find all table rows
    rows = soup.find_all("tr", role="row")

    for row in rows:
        # find champion name from TableItemRow
        item_row = row.find("div", class_="TableItemRow")
        if not item_row:
            continue

        # get champion name from img alt or link text
        link = item_row.find("a", class_="StatLink")
        if not link:
            continue

        # extract name from link text (after the img)
        champion_name = link.get_text(strip=True)
        if not champion_name:
            # fallback to img alt
            img = item_row.find("img", class_="TableItemImg")
            if img:
                champion_name = img.get("alt", "").strip()

        if not champion_name:
            continue

        # get tier
        tier_div = row.find("div", class_=re.compile(r"StatTierBadge"))
        tier = tier_div.get_text(strip=True) if tier_div else ""

        # get avg placement
        placement_div = row.find("div", class_=re.compile(r"TablePlacement"))
        avg_placement = placement_div.get_text(strip=True) if placement_div else ""

        # get win rate (first TableNum that's not TableBestUnitRow)
        all_cells = row.find_all("td", role="cell")
        win_rate = ""
        frequency = ""
        frequency_percent = ""

        for cell in all_cells:
            # win rate is in TableNum TableNumRow (not TableBestUnitRow)
            win_div = cell.find("div", class_="TableNum TableNumRow")
            if win_div and not win_rate:
                win_rate = win_div.get_text(strip=True)

            # frequency is in TableNumRight TableNumRow
            freq_div = cell.find("div", class_="TableNumRight TableNumRow")
            if freq_div:
                freq_text = freq_div.get_text(strip=True)
                # separate count and percentage
                freq_span = freq_div.find("span", class_="FrequencyPercent")
                if freq_span:
                    frequency_percent = freq_span.get_text(strip=True)
                    frequency = freq_text.replace(frequency_percent, "").strip()
                else:
                    frequency = freq_text

        # get popular items
        best_unit_div = row.find("div", class_="TableNum TableBestUnitRow")
        items = []
        if best_unit_div:
            item_imgs = best_unit_div.find_all("img", class_="AugmentStatTopUnit")
            items = [img.get("alt", "") for img in item_imgs if img.get("alt")]

        champions.append({
            "champion": champion_name,
            "tier": tier,
            "avg_placement": avg_placement,
            "win_rate": win_rate,
            "frequency": frequency,
            "frequency_percent": frequency_percent,
            "popular_items": ", ".join(items)
        })

    return champions


def save_to_csv(champions: list[dict], output_path: str) -> None:
    """save champion data to csv file."""
    fieldnames = [
        "champion", "tier", "avg_placement", "win_rate",
        "frequency", "frequency_percent", "popular_items"
    ]

    with open(output_path, "w", newline="", encoding="utf-8") as f:
        writer = csv.DictWriter(f, fieldnames=fieldnames)
        writer.writeheader()
        writer.writerows(champions)


def main():
    """main entry point."""
    # can use local file for testing or fetch from web
    if len(sys.argv) > 1 and Path(sys.argv[1]).exists():
        # use local html file
        html = Path(sys.argv[1]).read_text(encoding="utf-8")
        print(f"parsing local file: {sys.argv[1]}")
    else:
        # fetch from metatft
        url = "https://www.metatft.com/units"
        print(f"fetching data from {url}...")
        html = fetch_html(url)

    champions = parse_champions(html)
    print(f"found {len(champions)} champions")

    output_file = "metatft_champions.csv"
    save_to_csv(champions, output_file)
    print(f"saved to {output_file}")

    # print sample
    if champions:
        print("\nsample data:")
        for champ in champions[:3]:
            print(f"  {champ['champion']}: {champ['tier']} tier, {champ['avg_placement']} avg placement")


if __name__ == "__main__":
    main()
