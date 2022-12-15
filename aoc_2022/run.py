#!/usr/bin/env python3
import sys
import importlib
from pathlib import Path


def main():
    _, module, path, *args = sys.argv
    mod = importlib.import_module(module)

    with Path(path).open() as f:
        print(mod.part1(f, *args))
        f.seek(0)
        print(mod.part2(f, *args))

if __name__ == "__main__":
    main()
