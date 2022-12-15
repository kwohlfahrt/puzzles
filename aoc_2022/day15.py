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

    def occupies(self, y):
        dist = int((self.sensor - self.beacon).norm(1))
        y_offset = y - self.sensor[1]
        x_dist = max(0, dist - abs(y_offset))
        return range(self.sensor[0] - x_dist, self.sensor[0] + x_dist)


def part1(f):
    sensors = map(Sensor.parse, f)
    occupied = set.union(*map(set, (s.occupies(2_000_000) for s in sensors)))
    return len(occupied)


def part2(f):
    ...
