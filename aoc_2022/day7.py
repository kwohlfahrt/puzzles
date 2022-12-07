from functools import cached_property


class Node:
    def __init__(self, parent=None, size=None):
        self.parent = parent
        self.children = {}
        self._size = size

    @cached_property
    def size(self):
        if self._size is not None:
            return self._size
        else:
            return sum(c.size for c in self.children.values())

    def add_child(self, name, size=None):
        if name in self.children:
            return self.children[name]

        node = Node(parent=self, size=size)
        self.children[name] = node
        return node

    def walk(self):
        if self._size is not None:
            return

        yield self
        for node in self.children.values():
            yield from node.walk()


class Interpreter:
    def __init__(self):
        self.current_node = self.root_node = Node()

    def eval(self, line):
        match line.split():
            case ["$", "cd", ".."]:
                self.current_node = self.current_node.parent
            case ["$", "cd", d]:
                self.current_node = self.current_node.add_child(d)
            case ["$", "ls"]:
                pass
            case ["dir", name]:
                pass
            case [size, name]:
                self.current_node.add_child(name, size=int(size))


def part1(f):
    i = Interpreter()
    for line in map(str.strip, f):
        i.eval(line)
    return sum(n.size for n in i.root_node.walk() if n.size <= 100000)


def part2(f):
    i = Interpreter()
    for line in map(str.strip, f):
        i.eval(line)
    required_size = 30000000 - (70000000 - i.root_node.size)
    node = min(filter(lambda n: n.size >= required_size, i.root_node.walk()), key=lambda n: n.size)
    return node.size
