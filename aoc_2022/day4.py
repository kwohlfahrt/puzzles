class Range:
    def __init__(self, start, end):
        self.start = start
        self.end = end

    @classmethod
    def parse(cls, s):
        return cls(*map(int, s.split('-')))

    def __eq__(self, other):
        return self.start == other.start and self.end == other.end

    def __ge__(self, other):
        return self.start <= other.start and self.end >= other.end

    def __len__(self):
        return max(self.end - self.start + 1, 0)

    def intersection(self, other):
        return type(self)(max(self.start, other.start), min(self.end, other.end))


def part1(f):
    total = 0
    for line in map(str.strip, f):
        a, b = map(Range.parse, line.split(","))
        if a >= b or b >= a:
            total += 1
    return total


def part2(f):
    total = 0
    for line in map(str.strip, f):
        a, b = map(Range.parse, line.split(","))
        if len(a.intersection(b)) > 0:
            total += 1
    return total
