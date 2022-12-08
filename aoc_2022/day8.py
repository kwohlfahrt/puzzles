from itertools import product, repeat


class Grid:
    def __init__(self, grid):
        self.grid = grid

    @classmethod
    def parse(cls, lines):
        return cls([[int(c) for c in line] for line in lines])

    def neighbours(self, i, j):
        yield zip(range(i-1, -1, -1), repeat(j))
        yield zip(range(i+1, self.size[0]), repeat(j))
        yield zip(repeat(i), range(j-1, -1, -1))
        yield zip(repeat(i), range(j+1, self.size[1]))

    def is_visible(self, i, j):
        height = self.grid[i][j]
        for direction in self.neighbours(i, j):
            for i, j in direction:
                if self.grid[i][j] >= height:
                    break
            else:
                return True
        return False

    def senic_score(self, i, j):
        height = self.grid[i][j]
        score = 1
        for direction in self.neighbours(i, j):
            direction_score = 0
            for i, j in direction:
                direction_score += 1
                if self.grid[i][j] >= height:
                    break
            score *= direction_score
        return score

    @property
    def size(self):
        return len(self.grid[0]), len(self.grid)


def part1(f):
    grid = Grid.parse(map(str.strip, f))
    count = 0
    for i, j in product(*map(range, grid.size)):
        if grid.is_visible(i, j):
            count += 1
    return count

def part2(f):
    grid = Grid.parse(map(str.strip, f))
    score = 0
    for i, j in product(*map(range, grid.size)):
        score = max(grid.senic_score(i, j), score)
    return score
