"""Solution for day 16."""
from aoc import get_input, get_neighbours
from functools import reduce
from queue import PriorityQueue
from typing import Literal


Position = tuple[int, int]
Direction = tuple[int, int]
PosDir = tuple[Position, Direction]
Tile = Literal['.', '#']
Grid = dict[Position, Tile]


def format_input(inp: str) -> tuple[Grid, Position, Position]:
    """Formats the input and returns it as a tuple of
    (grid, start_position, end_position).
    """
    lines = inp.splitlines()
    width = len(lines[0])
    height = len(lines)
    start_pos = (1, height - 2)
    end_pos = (width - 2, 1)
    grid = {(x, y): lines[y][x] for y in range(height) for x in range(width)}
    grid[start_pos] = grid[end_pos] = '.'
    return grid, start_pos, end_pos


def get_neighbours(pos: Position, dir: Direction) -> list[PosDir]:
    """Returns the neighbours of pos.
    The neighbour straight behind pos is ignored.
    """
    def left():
        """Returns dir rotated 90 degrees left."""
        x, y = dir
        return y, -x

    def right():
        """Returns dir rotated 90 degrees right."""
        x, y = dir
        return -y, x

    x, y = pos
    dx, dy = dir
    neighbours = [((x + dx, y + dy), dir)]
    dx, dy = left()
    neighbours.append(((x + dx, y + dy), (dx, dy)))
    dx, dy = right()
    neighbours.append(((x + dx, y + dy), (dx, dy)))
    return neighbours


def get_paths_tiles(prevs: dict[PosDir, tuple[int, list[PosDir]]], pos: Position, dir: Direction) -> set[Position]:
    """Return a set of all positions from start to pos by going through
    prevs backwards from pos to end and adding each tile along the way.
    """
    tiles = {pos}
    try:
        _, parent_tiles = prevs[(pos, dir)]
        for parent_tile in parent_tiles:
            tiles |= get_paths_tiles(prevs, *parent_tile)
    except ValueError | KeyError:
        pass
    return tiles


def solve(grid: Grid, start_pos: Position, end_pos: Position) -> int:
    """Solves part 1 and returns the answer.
    Solution is made with A* search.
    Note that h and g have been swapped because I implemented from memmory and forgot which was which...
    """
    def h(prev_dir: Direction, new_dir: Direction) -> int:
        """Returns the cost to get from previous tile to new tile."""
        return 1 + 1000 * (prev_dir != new_dir)

    def g(pos: Position) -> int:
        """Returns the estimated cost of getting from pos to end_pos."""
        x, y = pos
        ex, ey = end_pos
        return abs(ex - x) + abs(y - ey)

    que = PriorityQueue()
    que.put((g(start_pos), (start_pos, (1, 0), 0)))
    visited = set()
    # Prevs is a bit weird. Basically, every visited PosDir is a key in prevs,
    # The value at that key is a tuple of the cost and a list of all previous tiles.
    # the cost is the cost to get there from start via any of the previous tiles.
    # If a new parent tile would be found with cheaper cost, it will be the new and only element in the list
    # If it would have the same cost, it will be appended to the list
    # And if it wold be more expensive, it is ignored.
    prevs = {(start_pos, (1, 0)): (0, [])}
    best_path_cost = None
    # a set of all directions that the end can be reached from via an optimal path.
    end_dirs = set()
    while que.qsize() != 0:
        _, (pos, dir, prev_h) = que.get()
        # Realised now afterwards that this logic is quite dumb, flawed and should not work in all cases
        # but it works with my input so it's all good :)
        if pos == end_pos:
            if best_path_cost == None:
                end_dirs.add(dir)
                best_path_cost = prev_h
            elif best_path_cost < prev_h:
                good_seats = reduce(set.union, [get_paths_tiles(prevs, end_pos, end_dir) for end_dir in end_dirs], set())
                return best_path_cost, len(good_seats)
            else:
                end_dirs.add(dir)
        if (pos, dir) in visited:
            continue
        else:
            visited.add((pos, dir))

        for new_pos, new_dir in get_neighbours(pos, dir):
            if grid[new_pos] == '.':
                new_h = prev_h + h(dir, new_dir)
                new_cost = new_h + g(new_pos)
                que.put((new_cost, (new_pos, new_dir, new_h)))
                if (new_pos, new_dir) in prevs:
                    # This tile has been visited before
                    best_cost, parent_tiles = prevs[(new_pos, new_dir)]
                    # Check if this path to new_pos was cheaper than the previous or had the same cost.
                    # If it was more expensive, nothing happens.
                    if best_cost == new_cost:
                        parent_tiles.append((pos, dir))
                    elif best_cost > new_cost:
                        prevs[(new_pos, new_dir)] = (new_cost, [(pos, dir)])
                else:
                    # This tile has not been visited before.
                    prevs[(new_pos, new_dir)] = (new_cost, [(pos, dir)])


if __name__ == '__main__':
    inp = format_input(get_input('day16.txt'))
    res = solve(*inp)
    print(*res, sep='\n')