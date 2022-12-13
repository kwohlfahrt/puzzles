import json
from itertools import zip_longest, chain

from util import chunks

class Packet:
    def __init__(self, data):
        self.data = data

    @classmethod
    def parse(cls, line):
        return cls(json.loads(line))

    def __str__(self):
        return json.dumps(self.data)

    def __lt__(self, other):
        cls = type(self)
        for i, j in zip_longest(self, other):
            match i, j:
                case int(i), int(j):
                    if i != j:
                        return i < j
                case int(i), list(j):
                    return cls([i]) < cls(j)
                case list(i), int(j):
                    return cls(i) < cls([j])
                case None, j:
                    return True
                case i, None:
                    return False
                case list(i), list(j):
                    if i != j:
                        return cls(i) < cls(j)

    def __eq__(self, other):
        return all(i == j for i, j in zip_longest(self, other))

    def __gt__(self, other):
        return not self < other and not self == other

    def __iter__(self):
        yield from self.data

    def __repr__(self):
        return f"Packet({str(self)})"


def part1(f):
    total = 0
    for i, (p1, p2, _) in enumerate(chunks(f, 3), start=1):
        p1 = Packet.parse(p1)
        p2 = Packet.parse(p2)
        if p1 < p2:
            total += i
    return total


def part2(f):
    dividers = Packet([[2]]), Packet([[6]])
    packets = chain(
        dividers,
        map(Packet.parse, filter(lambda line: line != "\n", f))
    )
    packets = sorted(packets)
    return (packets.index(dividers[0]) + 1) * (packets.index(dividers[1]) + 1)
