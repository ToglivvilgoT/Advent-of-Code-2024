from aoc import get_input, get_input_lines, uncurry
from functools import partial, reduce
from itertools import product


Grid = list[str]
Position = tuple[int, int]


def format_input(inp: str) -> tuple[Grid, list[Position]]:
    """Trasnfroms the input into a grid and a list of start positions."""
    grid = inp.splitlines()
    
    start_poses = []
    for y in range(len(grid)):
        for x in range(len(grid[y])):
            if grid[y][x] == '0':
                start_poses.append((x, y))

    return grid, start_poses


def get_neighbours(pos: Position) -> list[Position]:
    """Returns a list of all neighbours of pos  (diagonals not included)."""
    neighbours = []
    for dx, dy in (0, 1), (1, 0), (-1, 0), (0, -1):
        x, y = pos
        neighbours.append((x + dx, y + dy))        
    return neighbours


def in_bounds(pos: Position, max_x: int, max_y: int) -> bool:
    """Return True if pos is above or equal to (0, 0) and strictly below (max_x, max_y), False otherwise."""
    x, y = pos
    is_x = 0 <= x < max_x
    is_y = 0 <= y < max_y
    return is_x and is_y


def step_incline(next_pos: Position, grid: Grid, current_step: str) -> bool:
    """Returns True if the tile on next_pos is a number exactly 1 above current_step."""
    return (int(current_step) - int(grid[next_pos[1]][next_pos[0]])) == -1


def solveOne(start_pos: Position, grid: Grid, current_step: str = '0') -> set[Position]:
    """Given a hiking trail head (start_pos) and a grid, return a set of all reachable hiking trail ends (all '9's)."""
    if current_step == '9':
        return {start_pos}

    end_poses = set()
    for neighbour in filter(
                        partial(step_incline, grid=grid, current_step=current_step), 
                        filter(
                            partial(in_bounds, max_x=len(grid), max_y=len(grid[0])), 
                            get_neighbours(start_pos)
                        )
                    ):
        end_poses |= solveOne(neighbour, grid, str(int(current_step) + 1))
    return end_poses


def solve(grid: Grid, start_poses: list[Position]) -> int:
    """Solves part one and returns answer."""
    return sum(map(len, map(partial(solveOne, grid=grid), start_poses)))


def solveOne2(start_pos: list[Position], grid: Grid, current_step: str = '0') -> int:
    """Given a hiking trail head (start_pos) and a grid, return amount of paths leading to an end (end == a '9')."""
    if current_step == '9':
        return 1

    end_poses = 0
    for neighbour in filter(partial(step_incline, grid=grid, current_step=current_step), filter(partial(in_bounds, max_x=len(grid), max_y=len(grid[0])), get_neighbours(start_pos))):
        end_poses += solveOne2(neighbour, grid, str(int(current_step) + 1))
    return end_poses


def solve2(grid: Grid, start_poses: list[Position]) -> int:
    """Solves part 2 and returns the answer."""
    return sum(map(partial(solveOne2, grid=grid), start_poses))


if __name__ == '__main__':
    inp = format_input(get_input('day10.txt'))
    res = solve(*inp)
    print(res)
    res = solve2(*inp)
    print(res)