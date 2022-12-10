class CPU:
    def __init__(self):
        self.cycle = 1
        self.x = 1
        self.ready = True

    def run(self, lines):
        lines = map(str.strip, lines)
        while True:
            self.cycle += 1

            if (self.cycle - 20) % 40 == 0:
                yield self.signal_strength

            if not self.ready:
                self.ready = True
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
                self.x += int(value)
                self.ready = False

    @property
    def signal_strength(self):
        return self.x * self.cycle


def part1(f):
    cpu = CPU()
    return sum(cpu.run(f))


def part2(f):
    ...
