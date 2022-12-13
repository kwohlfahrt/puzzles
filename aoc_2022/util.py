def chunks(iterable, n):
    iterable = iter(iterable)
    yield from zip(*([iterable] * n))

