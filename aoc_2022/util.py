from itertools import tee


def chunks(iterable, n):
    iterable = iter(iterable)
    yield from zip(*([iterable] * n))


def windows(iterable, n):
    iterables = tee(iterable, n)
    for i, iterable in enumerate(iterables):
        for _ in range(i):
            next(iterable)
    yield from zip(*iterables)
