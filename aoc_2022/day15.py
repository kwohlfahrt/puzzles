import re
from vector import Vector


class Sensor:
    re = re.compile("Sensor at x=(-?[0-9]+), y=(-?[0-9]+): closest beacon is at x=(-?[0-9]+), y=(-?[0-9]+)")

    def __init__(self, sensor, beacon):
        self.sensor = sensor
        self.beacon = beacon

    @classmethod
    def parse(cls, line):
        sensor_x, sensor_y, beacon_x, beacon_y = map(int, cls.re.match(line).groups())
        return cls(Vector(sensor_x, sensor_y), Vector(beacon_x, beacon_y))

    def __repr__(self):
        return f"Sensor({', '.join(map(str, self.sensor))}, beacon={self.beacon})"

    @property
    def dist(self):
        return int((self.sensor - self.beacon).norm(1))

    def occupies(self, y, limits=None):
        y_offset = y - self.sensor[1]
        x_dist = max(0, self.dist - abs(y_offset))
        x_range = self.sensor[0] - x_dist, self.sensor[0] + x_dist + (x_dist > 0)
        if limits is not None:
            x_range = max(limits[0], x_range[0]), min(limits[1], x_range[1])
        return Range(*x_range)

    @staticmethod
    def unoccupied(sensors, limit, y):
        occupied = sum(map(len, Range.union(*(s.occupies(y, (0, limit)) for s in sensors))))
        if occupied < limit:
            [x] = set(range(limit)).difference(*Range.union(*(s.occupies(y, (0, limit)) for s in sensors)))
            return x, y
        else:
            return None


class Range:
    def __init__(self, start, end):
        self.start = min(start, end)
        self.end = max(start, end)

    def __iter__(self):
        yield from range(self.start, self.end)

    def __len__(self):
        return self.end - self.start

    def __hash__(self):
        return hash((self.start, self.end))

    def __eq__(self, other):
        return self.start == other.start, self.end == other.end

    @classmethod
    def union(self, *ranges):
        ranges = iter(sorted(ranges, key=lambda r: r.start))
        current = next(ranges)
        for r in ranges:
            if r.start > current.end:
                yield current
                current = r
            else:
                current.end = max(current.end, r.end)
        yield current

    def __repr__(self):
        return f"Range({self.start}, {self.end})"


def part1(f, *args):
    sensors = list(map(Sensor.parse, f))
    y = 10 if "--test" in args else 2_000_000
    occupied = Range.union(*(s.occupies(y) for s in sensors))
    beacons = set({s.beacon[0] for s in sensors if s.beacon[1] == y})
    return sum(map(len, occupied)) - len(beacons)


def part2(f, *args):
    sensors = list(map(Sensor.parse, f))
    limit = 20 if "--test" in args else 4_000_000
    for y in range(0, limit):
        unoccupied = Sensor.unoccupied(sensors, limit, y)
        if unoccupied is not None:
            x, y = unoccupied
            return x * 4000000 + y
