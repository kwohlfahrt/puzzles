from util import windows


def part1(f):
    for i, window in enumerate(windows(f.read(), 4), start=4):
        if len(set(window)) == len(window):
            return i


def part2(f):
    for i, window in enumerate(windows(f.read(), 14), start=14):
        if len(set(window)) == len(window):
            return i
