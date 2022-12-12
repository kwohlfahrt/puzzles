from collections import deque
from itertools import repeat, product


class Grid:
    def __init__(self, heights):
        self.heights = heights

    @classmethod
    def parse(cls, lines):
        heights = []
        for y, line in enumerate(lines):
            if (x := line.find("S")) >= 0:
                position = x, y
            if (x := line.find("E")) >= 0:
                target = x, y

            line = line.replace("S", "a").replace("E", "z")
            heights.append([ord(c) - ord("a") for c in line])

        return cls(heights), position, target

    def __str__(self):
        grid = [[chr(ord("a") + h) for h in row] for row in self.heights]
        return "\n".join(map("".join, grid))

    def neighbors(self, position):
        x, y = position
        if x > 0:
            yield x - 1, y
        if x < len(self.heights[0]) - 1:
            yield x + 1, y
        if y > 0:
            yield x, y - 1
        if y < len(self.heights) - 1:
            yield x, y + 1

    def __getitem__(self, idx):
        x, y = idx
        return self.heights[y][x]

    def __iter__(self):
        return product(range(len(self.heights[0])), range(len(self.heights)))


def search(grid, position, targets, reverse=False):
    queue = deque([(position, 0)])
    seen = set()

    while queue:
        position, length = queue.pop()
        if position in targets:
            return length
        if position in seen:
            continue
        seen.add(position)
        neighbors = grid.neighbors(position)
        if not reverse:
            neighbors = filter(lambda n: grid[n] <= grid[position] + 1, neighbors)
        else:
            neighbors = filter(lambda n: grid[n] >= grid[position] - 1, neighbors)

        queue.extendleft(zip(neighbors, repeat(length + 1)))


def part1(f):
    grid, position, target = Grid.parse(map(str.strip, f))
    return search(grid, position, {target})


def part2(f):
    grid, _, target = Grid.parse(map(str.strip, f))
    lows = set(filter(lambda pos: grid[pos] == 0, grid))
    return search(grid, target, lows, reverse=True)
