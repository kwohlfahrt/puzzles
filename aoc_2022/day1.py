import sys
from pathlib import Path


def parse(file):
    caloriess = 0
    for line in map(str.strip, file):
        if not line:
            yield caloriess
            caloriess = 0
        else:
            caloriess += int(line)
    yield caloriess


def part1(file):
    return max(parse(file))


def part2(file):
    return sum(sorted(parse(file), key=lambda x: -x)[:3])


def main(argv=sys.argv):
    _, input_name = argv

    with Path(input_name).open() as f:
        print(part1(f))
        f.seek(0)
        print(part2(f))

if __name__ == "__main__":
    main()
