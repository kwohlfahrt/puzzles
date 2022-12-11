import re
import operator as op
from math import lcm

monkey_regex = """Monkey (?P<id>\d):
  Starting items: (?P<items>[0-9, ]+)
  Operation: (?P<op>.+)
  Test: divisible by (?P<test>\d+)
    If true: throw to monkey (?P<true>\d+)
    If false: throw to monkey (?P<false>\d+)"""


class Operation:
    regex = re.compile("new = ([^ ]+) (\*|\+) ([^ ]+)")
    operations = {"+": op.add, "*": op.mul}

    def __init__(self, operation, terms):
        self.terms = terms
        self.operation = self.operations[operation]

    @classmethod
    def parse(cls, line):
        [term1, operation, term2] = cls.regex.match(line).groups()
        terms = [(lambda x: x) if term == "old" else (lambda x: int(term)) for term in [term1, term2]]
        return cls(operation, terms)

    def __call__(self, value):
        return self.operation(self.terms[0](value), self.terms[1](value))


class Monkey:
    regex = re.compile(monkey_regex, re.MULTILINE)

    def __init__(self, items, operation, test, destinations):
        self.items = items
        self.operation = operation
        self.test = test
        self.destinations = destinations
        self.count = 0

    @classmethod
    def parse(cls, lines):
        matches = cls.regex.finditer(lines)
        return {
            m.group("id"): cls(
                list(map(int, m.group("items").split(", "))),
                Operation.parse(m.group("op")),
                int(m.group("test")),
                {True: m.group("true"), False: m.group("false")}
            )
        for m in matches}

    def turn(self, monkeys):
        for item in self.items:
            self.count += 1
            item = self.operation(item) // 3
            monkeys[self.destinations[item % self.test == 0]].items.append(item)
        self.items = []


class Monkey2(Monkey):
    def turn(self, monkeys):
        for item in self.items:
            self.count += 1
            item = self.operation(item) % lcm(*(m.test for m in monkeys.values()))
            monkeys[self.destinations[item % self.test == 0]].items.append(item)
        self.items = []


def part1(f):
    monkeys = Monkey.parse(f.read())
    for _ in range(20):
        for monkey in monkeys.values():
            monkey.turn(monkeys)

    monkeys = sorted(monkeys.values(), key=lambda m: -m.count)[:2]
    return monkeys[0].count * monkeys[1].count


def part2(f):
    monkeys = Monkey2.parse(f.read())
    for _ in range(10000):
        for monkey in monkeys.values():
            monkey.turn(monkeys)

    monkeys = sorted(monkeys.values(), key=lambda m: -m.count)[:2]
    return monkeys[0].count * monkeys[1].count
