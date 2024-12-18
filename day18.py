"""Solution for day 18."""
from aoc import get_input, uncurry, in_bounds, get_neighbours
from functools import partial
from queue import SimpleQueue


Position = tuple[int, int]
ByteFalls = list[Position]


def format_input(inp: str) -> ByteFalls:
    """Formats the input and returns it."""
    # Welcome to python one-liners 101
    return [tuple(map(int, line.split(','))) for line in inp.splitlines()]


def solve(byte_falls: ByteFalls, fall_amount: int, width: int, height: int) -> int:
    """Solves part 1 and returns the answer.
    Because of the problem formulation width = 6 means the grid width will be 7, same for height.
    """
    # Simple bfs seach
    grid = {byte_falls[i] for i in range(fall_amount)}
    que = SimpleQueue()
    que.put((0, 0))
    visited = set()
    length = {(0, 0): 0}

    while que.qsize() != 0:
        tile = que.get()
        if tile == (width, height):
            return length[tile]
        for neigbour in filter(lambda a: a not in grid, filter(uncurry(partial(in_bounds, max_x=width + 1, max_y=height + 1)), get_neighbours(tile))):
            if neigbour not in visited:
                visited.add(neigbour)
                length[neigbour] = length[tile] + 1
                que.put(neigbour)


def solve2(byte_falls: ByteFalls) -> Position:
    """Solves part 2 and returns the answer."""
    # Brute force for the win, only takes like a minute to finish :P
    for i in range(len(byte_falls)):
        if solve(byte_falls, i, 70, 70) == None:
            return byte_falls[i - 1]


if __name__ == '__main__':
    inp = format_input(get_input('day18.txt'))
    res = solve(inp, 1024, 70, 70)
    print(res)
    res = solve2(inp)
    print(res)