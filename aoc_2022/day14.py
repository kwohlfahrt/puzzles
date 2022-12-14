from functools import cached_property
from itertools import repeat
from util import windows


class Grid:
    def __init__(self, rocks, floor):
        self.rocks = rocks
        self.floor = floor

    @cached_property
    def bottom(self):
        return max(r[1] + 2 for r in self.rocks)

    @classmethod
    def parse(cls, lines, floor=False):
        rocks = set()
        for line in lines:
            coords = (tuple(map(int, c.split(","))) for c in line.split(" -> "))
            for (x1, y1), (x2, y2) in windows(coords, 2):
                x1, x2 = min(x1, x2), max(x1, x2)
                y1, y2 = min(y1, y2), max(y1, y2)
                if x1 == x2:
                    line_coords = zip(repeat(x1), range(y1, y2 + 1))
                elif y1 == y2:
                    line_coords = zip(range(x1, x2 + 1), repeat(y1))
                else:
                    raise ValueError(f"Segment {x1}, {y1} -> {x2}, {y2} is not horizontal or vertical")
                rocks.update(line_coords)
        return cls(rocks, floor)

    def add_sand(self):
        x, y = (500, 0)
        while True:
            if self.floor and y == self.bottom - 1:
                self.rocks.add((x, y))
                return False
            elif y >= self.bottom:
                return True
            elif (x, y + 1) not in self.rocks:
                pass
            elif (x - 1, y + 1) not in self.rocks:
                x = x - 1
            elif (x + 1, y + 1) not in self.rocks:
                x = x + 1
            elif (x, y) == (500, 0):
                return True
            else:
                self.rocks.add((x, y))
                return False
            y += 1

    def __str__(self):
        xs, ys = zip(*self.rocks)
        min_x = min(xs)
        grid = [["." for _ in range(min_x, max(xs) + 1)] for _ in range(0, max(ys) + 1)]
        for x, y in self.rocks:
            grid[y][x - min_x] = "#"
        return "\n".join(map("".join, grid))


def part1(f):
    cave = Grid.parse(f)
    total = 0
    while not cave.add_sand():
        total += 1
    return total

def part2(f):
    cave = Grid.parse(f, floor=True)
    total = 1
    while not cave.add_sand():
        total += 1
    return total
