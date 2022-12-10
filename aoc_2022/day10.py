class CPU:
    def __init__(self):
        self.x = 1
        self.state = None

    def run(self, lines):
        lines = map(str.strip, lines)

        while True:
            yield self.x

            match self.state:
                case None:
                    pass
                case ["addx", value]:
                    self.x += value
                    self.state = None
                    continue

            try:
                instruction = next(lines)
            except StopIteration:
                return

            self.execute(instruction)

    def execute(self, instruction):
        match instruction.split():
            case ["noop"]:
                pass
            case ["addx", value]:
                self.state = ["addx", int(value)]


class CRT:
    def __init__(self):
        self.cpu = CPU()

    def run(self, lines):
        xs = self.cpu.run(lines)

        pixels = [[" " for _ in range(40)] for _ in range(6)]
        for j, row in enumerate(pixels):
            for i in range(len(row)):
                x = next(xs)
                if abs(i - x) <= 1:
                    pixels[j][i] = "#"
        return "\n".join("".join(row) for row in pixels)



def part1(f):
    cpu = CPU()
    signal = 0
    for cycle, x in enumerate(cpu.run(f), start=1):
        if (cycle - 20) % 40 == 0:
            signal += x * cycle
    return signal


def part2(f):
    crt = CRT()
    return crt.run(f)
