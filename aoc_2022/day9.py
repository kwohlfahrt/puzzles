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


class Rope:
    def __init__(self, length=1):
        self.head = Vector(0, 0)
        self.tail = Vector(0, 0)

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
            delta = self.head - self.tail
            if delta.norm() > 1:
                self.tail += delta.clamp(-1, 1)
                yield self.tail

    def interpret(self, line):
        direction, amount = line.strip().split()
        yield self.tail
        yield from self.move(direction, int(amount))


def part1(f):
    rope = Rope()
    visited = set()
    for line in f:
        visited.update(rope.interpret(line))
    return len(visited)


def part2(f):
    ...
