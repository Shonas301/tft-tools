#!/usr/bin/env python3
import csv
import sys
from collections import defaultdict

def sanitize(name):
    """Remove special characters and spaces"""
    return ''.join(c for c in name if c.isalnum())

def generate_shorthand(name):
    """Generate a shorthand for an augment name"""
    # Remove special characters
    clean_name = name.replace("'", "").replace("-", " ").replace("+", "")

    if ' ' in clean_name:
        words = clean_name.split()
        significant_words = [w for w in words if w.upper() not in ['I', 'II', 'III', 'IV'] and len(w) > 1]

        if not significant_words:
            significant_words = words

        # Take first letter of each significant word, up to 3 letters
        if len(significant_words) >= 3:
            return ''.join(sanitize(w)[0] for w in significant_words[:3]).upper()
        elif len(significant_words) == 2:
            return ''.join(sanitize(w)[0] for w in significant_words).upper()
        else:
            return sanitize(significant_words[0])[:3].upper()

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
        print("Usage: generate_augment_shorthands.py <input.csv> <output.csv>")
        sys.exit(1)

    input_file = sys.argv[1]
    output_file = sys.argv[2]

    # Read the CSV
    with open(input_file, 'r') as f:
        reader = csv.DictReader(f)
        rows = list(reader)
        fieldnames = reader.fieldnames

    # Generate shorthands
    shorthands_list = [(row['name'], generate_shorthand(row['name'])) for row in rows]
    unique_shorthands = make_unique(shorthands_list)

    # Add shorthand column
    new_fieldnames = list(fieldnames) + ['shorthand']

    # Add shorthands to rows
    for row, (_, shorthand) in zip(rows, unique_shorthands):
        row['shorthand'] = shorthand

    # Write output
    with open(output_file, 'w', newline='') as f:
        writer = csv.DictWriter(f, fieldnames=new_fieldnames)
        writer.writeheader()
        writer.writerows(rows)

    print(f"Generated shorthands for {len(rows)} augments")
    print(f"Output written to {output_file}")

    # Print some examples
    print("\nSample shorthands:")
    for i, (name, shorthand) in enumerate(unique_shorthands[:20]):
        print(f"  {name:35s} -> {shorthand}")

if __name__ == '__main__':
    main()
