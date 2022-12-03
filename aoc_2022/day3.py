from pathlib import Path
import sys

def priority(c):
    if c.isupper():
        return ord(c) - ord("A") + 27
    else:
        return ord(c) - ord("a") + 1

def chunks(iterable, n):
    iterable = iter(iterable)
    yield from zip(*([iterable] * n))


def part1(f):
    total = 0
    for line in map(str.strip, f):
        split = len(line) // 2
        compartments = line[:split], line[split:]
        item, = set.intersection(*map(set, compartments))
        total += priority(item)
    return total


def part2(f):
    total = 0
    groups = chunks(map(str.strip, f), 3)
    for group in groups:
        item, = set.intersection(*map(set, group))
        total += priority(item)
    return total


def main(argv=sys.argv):
    _, input_name = argv

    with Path(input_name).open() as f:
        print(part1(f))
        f.seek(0)
        print(part2(f))

if __name__ == "__main__":
    main()
