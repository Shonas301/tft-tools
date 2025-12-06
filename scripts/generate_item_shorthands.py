#!/usr/bin/env python3
import csv
import sys
from collections import defaultdict

def sanitize(name):
    """Remove special characters and spaces"""
    return ''.join(c for c in name if c.isalnum())

def generate_shorthand(name):
    """Generate a shorthand for an item name"""
    clean_name = name.replace("'", "").replace("-", " ")

    if ' ' in clean_name:
        words = clean_name.split()
        if len(words) >= 3:
            return ''.join(sanitize(w)[0] for w in words[:3] if sanitize(w)).upper()
        elif len(words) == 2:
            # For 2-word items, take 2 letters from first word, 1 from second
            # Or first letter of each if first word is short
            first = sanitize(words[0])
            second = sanitize(words[1])
            if len(first) <= 2:
                return (first[:2] + second[0]).upper()
            else:
                return (first[0] + second[0]).upper()
        else:
            return sanitize(words[0])[:3].upper()

    # Single word: take first 3 characters
    return sanitize(clean_name)[:3].upper()

def make_unique(shorthands_list):
    """Ensure all shorthands are unique by appending numbers if needed"""
    seen = defaultdict(int)
    result = []

    for name, shorthand in shorthands_list:
        if seen[shorthand] > 0:
            unique_shorthand = f"{shorthand}{seen[shorthand]}"
        else:
            unique_shorthand = shorthand
        seen[shorthand] += 1
        result.append((name, unique_shorthand))

    return result

def main():
    if len(sys.argv) != 3:
        print("Usage: generate_item_shorthands.py <input.csv> <output.csv>")
        sys.exit(1)

    input_file = sys.argv[1]
    output_file = sys.argv[2]

    with open(input_file, 'r') as f:
        reader = csv.DictReader(f)
        rows = list(reader)
        fieldnames = reader.fieldnames

    shorthands_list = [(row['name'], generate_shorthand(row['name'])) for row in rows]
    unique_shorthands = make_unique(shorthands_list)

    new_fieldnames = list(fieldnames) + ['shorthand']

    for row, (_, shorthand) in zip(rows, unique_shorthands):
        row['shorthand'] = shorthand

    with open(output_file, 'w', newline='') as f:
        writer = csv.DictWriter(f, fieldnames=new_fieldnames)
        writer.writeheader()
        writer.writerows(rows)

    print(f"Generated shorthands for {len(rows)} items")
    print(f"Output written to {output_file}")

    print("\nSample shorthands:")
    for i, (name, shorthand) in enumerate(unique_shorthands[:20]):
        print(f"  {name:30s} -> {shorthand}")

if __name__ == '__main__':
    main()
