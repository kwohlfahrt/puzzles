from itertools import takewhile
import re


class Stacks:
    def __init__(self, stacks):
        self.stacks = stacks

    @classmethod
    def parse(cls, file):
        lines = map(lambda s: s.rstrip("\n"), file)
        lines = takewhile(lambda x: x, lines)
        stacks = {num: list(''.join(stack).strip()[::-1]) for *stack, num in zip(*lines) if num != ' '}
        return cls(stacks)

    def apply(self, move):
        for _ in range(move.n):
            crate = self.stacks[move.src].pop()
            self.stacks[move.dst].append(crate)

    def apply2(self, move):
        crates = self.stacks[move.src][-move.n:]
        del self.stacks[move.src][-move.n:]
        self.stacks[move.dst].extend(crates)

    def tops(self):
        return (v[-1] for v in self.stacks.values())


class Move:
    MOVE_RE = re.compile(r"^move ([0-9]+) from ([0-9]+) to ([0-9]+)$")

    def __init__(self, n, src, dst):
        self.n = n
        self.src = src
        self.dst = dst

    @classmethod
    def parse(cls, line):
        n, src, dst = cls.MOVE_RE.match(line).groups()
        return cls(int(n), src, dst)


def part1(f):
    stacks = Stacks.parse(f)
    for move in map(Move.parse, map(str.strip, f)):
        stacks.apply(move)
    return ''.join(stacks.tops())


def part2(f):
    stacks = Stacks.parse(f)
    for move in map(Move.parse, map(str.strip, f)):
        stacks.apply2(move)
    return ''.join(stacks.tops())
