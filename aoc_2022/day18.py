from itertools import chain, filterfalse, product, repeat
from functools import partial
import operator as op


class Grid(set):
    @classmethod
    def parse(cls, lines):
        return cls((tuple(map(int, line.split(","))) for line in lines))

    @classmethod
    def faces(cls, bounds):
        voxels = set()
        ranges = [range(start, end + 1) for start, end in bounds]
        for i, (start, end) in enumerate(bounds):
            voxels.update(product(*replace(ranges, i, [start - 1])))
            voxels.update(product(*replace(ranges, i, [end + 1])))
        return cls(voxels)


    @property
    def neighbours(self):
        yield from filterfalse(partial(op.contains, self), chain.from_iterable(map(neighbours, self)))

    @property
    def bounds(self):
        return tuple(zip(map(min, zip(*self)), map(max, zip(*self))))


class Bounds:
    def __init__(self, bounds):
        self.bounds = bounds

    @classmethod
    def from_points(cls, points):
        return cls(tuple(zip(map(min, zip(*points)), map(max, zip(*points)))))

    def contains(self, point):
        return all(p >= start and p <= end for p, (start, end) in zip(point, self.bounds))


def replace(xs, i, x):
    return type(xs)((*xs[:i], x, *xs[i+1:]))


def neighbours(coord):
    for i, c in enumerate(coord):
        yield replace(coord, i, c + 1)
        yield replace(coord, i, c - 1)



def part1(f):
    g = Grid.parse(f)
    return sum(1 for _ in g.neighbours)


def part2(f):
    g = Grid.parse(f)
    bounds = Bounds.from_points(g)
    outside = new = Grid.faces(bounds.bounds)
    while new:
        new = Grid(set(filter(bounds.contains, new.neighbours)) - outside - g)
        outside.update(new)
    faces = filter(partial(op.contains, outside), g.neighbours)
    return sum(1 for _ in faces)
