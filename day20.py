from aoc import get_input, get_neighbours
from itertools import product
from typing import Literal


Position = tuple[int, int]
Tile = Literal['.', '#']
Grid = dict[Position, Tile]


def format_input(inp: str) -> tuple[Grid, Position, Position]:
    """Formats inp and returns a tuple with three parts:
    grid, start position and end position.
    """
    grid = {}
    for y, row in enumerate(inp.splitlines()):
        for x, tile in enumerate(row):
            if tile == 'S':
                start = x, y
                tile = '.'
            elif tile == 'E':
                end = x, y
                tile = '.'
            grid[(x, y)] = tile
    return grid, start, end


def dfs(grid: Grid, start: Position, end: Position) -> list[Position]:
    """Simple depth first seach from start to end, returns the first found path."""
    prevs = {start: None}
    def generate_path(tile):
        try:
            return generate_path(prevs[tile]) + [tile]
        except KeyError:
            return []

    stack = [start]
    visited = {start}
    while stack:
        next = stack.pop()
        if next == end:
            return generate_path(next)

        for neighbour in get_neighbours(next):
            if neighbour not in visited and grid.get(neighbour, '#') != '#':
                stack.append(neighbour)
                visited.add(neighbour)
                prevs[neighbour] = next


def get_many_neighbours(tile: Position, distance: int) -> set[tuple[Position, int]]:
    """Return all positions with manhatan distance lower than distance from tile
    as a set of tuples on form (Position, distance).
    Manhatan distance is abs(x1 - x2) + abs(y1 - y2).
    """
    def dist_below(neighbour: Position) -> bool:
        """Return True if neighbour is within distance from tile."""
        x, y = neighbour
        x2, y2 = tile
        return abs(x - x2) + abs(y - y2) <= distance

    def dist_between(neighbour: Position) -> int:
        """Return the distance between neighbour and tile."""
        x, y = neighbour
        x2, y2 = tile
        return abs(x - x2) + abs(y - y2)

    x, y = tile
    neighbours = filter(dist_below, product(range(x - distance, x + distance + 1), range(y - distance, y + distance + 1)))
    return set(map(lambda neigh: (neigh, dist_between(neigh)), neighbours))


def get_cheats(tile: Position, path: list[Position], distance: int, limit: int) -> int:
    """return amount of cheats possible from tile, with a distance shorter than distance
    and a short cut benefit of more than limit (exclusive).
    """
    cheats = 0
    curr_dist = path[tile]
    for neigbour, dist in get_many_neighbours(tile, distance):
        if path.get(neigbour, -1) - curr_dist - dist > limit:
            cheats += 1
    return cheats


def solve(grid: Grid, start: Position, end: Position, distance: int, limit: int):
    """Solves part 1 and 2.
    distance is the maximum distance of a cheat that is allowed (inclusive).
    limit is the smallest advantage a cheat must give for it to count (exclusive).
    """
    path = {tile: distance for distance, tile in enumerate(dfs(grid, start, end))}
    cheats = 0
    for tile in path:
        cheats += get_cheats(tile, path, distance, limit)
    return cheats


if __name__ == '__main__':
    inp = format_input(get_input('day20.txt'))
    res = solve(*inp, 2, 99)
    print(res)
    res = solve(*inp, 20, 99)
    print(res)