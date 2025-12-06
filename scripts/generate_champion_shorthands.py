#!/usr/bin/env python3
import csv
import sys
from collections import defaultdict

def sanitize(name):
    """Remove special characters and spaces"""
    return ''.join(c for c in name if c.isalnum())

def generate_shorthand(name):
    """Generate a shorthand for a champion name"""
    clean = sanitize(name)

    if ' ' in name:
        words = name.split()
        if '&' in name:
            parts = [sanitize(w) for w in words if w != '&']
            return ''.join(p[:2] for p in parts).upper()
        return ''.join(sanitize(w)[0] for w in words if sanitize(w)).upper()

    return clean[:3].upper()

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
        print("Usage: generate_champion_shorthands.py <input.csv> <output.csv>")
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

    print(f"Generated shorthands for {len(rows)} champions")
    print(f"Output written to {output_file}")

if __name__ == '__main__':
    main()
