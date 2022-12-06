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
