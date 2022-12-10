class Vector:
    def __init__(self, *pos):
        self.pos = pos

    def __len__(self):
        return len(self.pos)

    def __getitem__(self, idx):
        return self.pos[idx]

    def __sub__(self, other):
        return type(self)(*(a - b for a, b in zip(self, other)))

    def __add__(self, other):
        return type(self)(*(a + b for a, b in zip(self, other)))

    def clamp(self, low, high):
        return type(self)(*(max(min(x, high), low) for x in self))

    def __hash__(self):
        return hash(self.pos)

    def __eq__(self, other):
        return self.pos == other.pos

    def norm(self):
        return max(map(abs, self))

    def __repr__(self):
        return f"Vector{repr(self.pos)}"


class Rope:
    def __init__(self, length=1):
        self.head = Vector(0, 0)
        self.tails = [Vector(0, 0) for _ in range(length)]

    def move(self, direction, amount):
        for _ in range(amount):
            match direction:
                case "R":
                    self.head += Vector(1, 0)
                case "L":
                    self.head -= Vector(1, 0)
                case "U":
                    self.head += Vector(0, 1)
                case "D":
                    self.head -= Vector(0, 1)
            last = self.head
            for i, tail in enumerate(self.tails):
                delta = last - tail
                if delta.norm() > 1:
                    self.tails[i] += delta.clamp(-1, 1)
                last = self.tails[i]
            yield last

    def interpret(self, line):
        direction, amount = line.strip().split()
        yield self.tails[-1]
        yield from self.move(direction, int(amount))


def part1(f):
    rope = Rope()
    visited = set()
    for line in f:
        visited.update(rope.interpret(line))
    return len(visited)


def part2(f):
    rope = Rope(length=9)
    visited = set()
    for line in f:
        visited.update(rope.interpret(line))
    return len(visited)
