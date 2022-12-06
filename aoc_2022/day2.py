from dataclasses import dataclass
from enum import Enum, auto

class Move(Enum):
    rock = auto()
    paper = auto()
    scissors = auto()

    @property
    def value(self):
        cls = type(self)
        match self:
            case cls.rock:
                return 1
            case cls.paper:
                return 2
            case cls.scissors:
                return 3

    @classmethod
    def from_str(cls, s):
        match s:
            case "A" | "X":
                return cls.rock
            case "B" | "Y":
                return cls.paper
            case "C" | "Z":
                return cls.scissors

    def for_result(self, result):
        cls = type(self)

        for opt in cls:
            match result:
                case "X":
                    if opt < self:
                        return opt
                case "Y":
                    if opt == self:
                        return opt
                case "Z":
                    if opt > self:
                        return opt

    def __gt__(self, other):
        cls = type(self)

        if self == other:
            return False

        match (self, other):
            case (cls.rock, cls.scissors):
                return True
            case (cls.scissors, cls.paper):
                return True
            case (cls.paper, cls.rock):
                return True
            case _:
                return False

    def __lt__(self, other):
        return not (self == other or self > other)


@dataclass
class Round:
    opponent: Move
    own: Move

    @classmethod
    def parse(cls, line: str):
        a, b = map(Move.from_str, line.strip().split())
        return cls(a, b)

    @property
    def score(self):
        if self.opponent == self.own:
            return self.own.value + 3
        elif self.own > self.opponent:
            return self.own.value + 6
        else:
            return self.own.value + 0


class Round2(Round):
    @classmethod
    def parse(cls, line):
        a, b = line.strip().split()
        a = Move.from_str(a)
        b = a.for_result(b)
        return cls(a, b)


def part1(file):
    return sum(r.score for r in map(Round.parse, file))


def part2(file):
    return sum(r.score for r in map(Round2.parse, file))
