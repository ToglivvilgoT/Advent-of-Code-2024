from aoc import get_input, get_input_lines, uncurry, in_bounds, get_neighbours
from functools import partial, reduce
from itertools import product
from queue import PriorityQueue


def format_input(inp: str):
    lines = inp.splitlines()
    width = len(lines[0])
    height = len(lines)
    start_pos = (1, height - 2)
    end_pos = (width - 2, 1)
    world = {(x, y): lines[y][x] for y in range(height) for x in range(width)}
    world[start_pos] = world[end_pos] = '.'
    return world, start_pos, end_pos


def get_neighbours(pos, dir):
    def left():
        x, y = dir
        return y, -x

    def right():
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


def get_paths_tiles(prevs, pos, dir):
    tiles = {pos}
    try:
        _, parent_tiles = prevs[(pos, dir)]
        for parent_tile in parent_tiles:
            tiles |= get_paths_tiles(prevs, *parent_tile)
    except ValueError | KeyError:
        pass
    return tiles


def solve(world, start_pos, end_pos):
    def h(prev_dir, new_dir):
        return 1 + 1000 * (prev_dir != new_dir)

    def g(pos):
        x, y = pos
        ex, ey = end_pos
        return abs(ex - x) + abs(y - ey)

    que = PriorityQueue()
    que.put((g(start_pos), (start_pos, (1, 0), 0)))
    visited = set()
    prevs = {(start_pos, (1, 0)): (0, [])}
    best_path_cost = None
    end_dirs = set()
    while que.qsize() != 0:
        _, (pos, dir, prev_h) = que.get()
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
            if world[new_pos] == '.':
                new_h = prev_h + h(dir, new_dir)
                new_cost = new_h + g(new_pos)
                que.put((new_cost, (new_pos, new_dir, new_h)))
                if (new_pos, new_dir) in prevs:
                    best_cost, parent_tiles = prevs[(new_pos, new_dir)]
                    if best_cost == new_cost:
                        parent_tiles.append((pos, dir))
                    elif best_cost > new_cost:
                        prevs[(new_pos, new_dir)] = (new_cost, [(pos, dir)])
                else:
                    prevs[(new_pos, new_dir)] = (new_cost, [(pos, dir)])


if __name__ == '__main__':
    inp = format_input(get_input('day16.txt'))
    res = solve(*inp)
    print(*res, sep='\n')