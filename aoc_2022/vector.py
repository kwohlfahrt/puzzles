class Vector:
    def __init__(self, *pos):
        self.pos = pos

    def __len__(self):
        return len(self.pos)

    def __getitem__(self, idx):
        return self.pos[idx]

    def __sub__(self, other):
        return type(self)(*(a - b for a, b in zip(self, other)))

    def __add__(self, other):
        return type(self)(*(a + b for a, b in zip(self, other)))

    def clamp(self, low, high):
        return type(self)(*(max(min(x, high), low) for x in self))

    def __hash__(self):
        return hash(self.pos)

    def __eq__(self, other):
        return self.pos == other.pos

    def norm(self, n=None):
        if n is None:
            # Infinity norm
            return max(map(abs, self))
        else:
            return sum(abs(i) ** n for i in self) ** (1 / n)

    def __repr__(self):
        return f"Vector{repr(self.pos)}"
