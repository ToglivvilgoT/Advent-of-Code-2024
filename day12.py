from aoc import get_input, in_bounds, get_neighbours


Position = tuple[int, int]
Region = set[Position]


def flood_fill(map: list[str], x: int, y: int) -> Region:
    """Extract the region connected to x and y using flood fill."""
    region = set()
    typ = map[y][x]
    stack = [(x, y)]
    visited = {(x, y)}
    max_x = len(map[0])
    max_y = len(map)
    while stack:
        x, y = stack.pop()
        if in_bounds(x, y, max_x, max_y) and map[y][x] == typ:
            region.add((x, y))
            for neighbour in get_neighbours((x, y)):
                if neighbour not in visited:
                    stack.append(neighbour)
                    visited.add(neighbour)
    return region


def format_input(inp: str) -> list[Region]:
    """Formats the input."""
    visited = set()
    regions = []
    lines = inp.splitlines()
    for y in range(len(lines)):
        for x in range(len(lines[y])):
            if (x, y) in visited:
                continue
            new_region = flood_fill(lines, x, y)
            regions.append(new_region)
            visited |= new_region
    return regions


def get_perimiter(region: Region) -> int:
    """Returns the perimiter of a region."""
    perimiter = 0
    for tile in region:
        fences = 4
        for neighbour in get_neighbours(tile):
            if neighbour in region:
                fences -= 1
        perimiter += fences
    return perimiter


def solve(regions: list[Region]) -> int:
    """Solves part 1 and returns the answer."""
    answer = 0
    for region in regions:
        area = len(region)
        perimiter = get_perimiter(region)
        answer += area * perimiter
    return answer


def get_neighbours2(pos: Position) -> list[tuple[Position, Position, Position]]:
    """Given a position, return a list of 4 tuples,
    each tuple has 3 positions coresponding to one of the four corners
    of the 3x3 square where pos is the middle tile.
    """
    RIGHT = (1, 0)
    UP_RIGHT = (1, -1)
    UP = (0, -1)
    UP_LEFT = (-1, -1)
    LEFT = (-1, 0)
    DOWN_LEFT = (-1, 1)
    DOWN = (0, 1)
    DOWN_RIGHT = (1, 1)

    x, y = pos
    neighbours = []
    for (dx, dy), (dx2, dy2), (dx3, dy3) in [
            (UP, LEFT, UP_LEFT),
            (RIGHT, UP, UP_RIGHT),
            (DOWN, RIGHT, DOWN_RIGHT),
            (LEFT, DOWN, DOWN_LEFT),
    ]:
        neighbours.append(((x + dx, y + dy), (x + dx2, y + dy2), (x + dx3, y + dy3)))
    return neighbours


def get_sides(region: Region) -> int:
    """Return amount of sides of region."""
    sides = 0
    for tile in region:
        for neighbour, neighbour2, neighbour3 in get_neighbours2(tile):
            # This is logic, it works so don't tuch it.
            if not (neighbour in region or neighbour2 in region and neighbour3 not in region):
                sides += 1
    return sides


def solve2(regions: list[Region]) -> int:
    """Solves part 2 and returns the answer."""
    answer = 0
    for region in regions:
        area = len(region)
        sides = get_sides(region)
        answer += area * sides
    return answer


if __name__ == '__main__':
    inp = format_input(get_input('day12.txt'))
    res = solve(inp)
    print(res)
    res = solve2(inp)
    print(res)