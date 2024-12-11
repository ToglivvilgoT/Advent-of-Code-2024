import aoc
import sys
from copy import deepcopy


sys.setrecursionlimit(10000)


def format_input(input: str):
    lab_map = []
    start_pos = None

    lines = input.splitlines()
    for y in range(len(lines)):
        new_row = []
        for x in range(len(lines[y])):
            if lines[y][x] == '^':
                start_pos = x, y
                new_row.append('.')
            else:
                new_row.append(lines[y][x])
        lab_map.append(new_row)
    return lab_map, start_pos


def safe_check(matrix, x, y):
    inside = 0 <= y < len(matrix) and 0 <= x < len(matrix[y])
    if inside:
        return matrix[y][x] == '#'
    else:
        return None


def walk(lab_map, x, y, dx, dy, visited):
    next_x = x + dx
    next_y = y + dy
    match safe_check(lab_map, next_x, next_y):
        case True:
            return walk(lab_map, x, y, -dy, dx, visited)
        case False:
            visited.add((next_x, next_y))
            return walk(lab_map, next_x, next_y, dx, dy, visited)
        case None:
            return visited


def solve(lab_map, start_pos):
    return walk(lab_map, *start_pos, 0, -1, {start_pos})


def walk2(lab_map, x, y, dx, dy, visited, debug = False):
    if (((x, y), (dx, dy))) in visited:
        return True
    visited.add(((x, y), (dx, dy)))
    next_x = x + dx
    next_y = y + dy
    match safe_check(lab_map, next_x, next_y):
        case True:
            return walk2(lab_map, x, y, -dy, dx, visited, debug)
        case False:
            return walk2(lab_map, next_x, next_y, dx, dy, visited, debug)
        case None:
            return False


def solve2(lab_map: list[list[str]], start_pos, visited):
    res = 0
    for x, y in visited:
        new_map = deepcopy(lab_map)
        if (x, y) == (3, 6):
            debug = False
        else:
            debug = False
        new_map[y][x] = '#'
        res += walk2(new_map, *start_pos, 0, -1, set(), debug)
    return res


if __name__ == '__main__':
    input = format_input(aoc.get_input('day6.txt'))
    visited = solve(*input)
    print(len(visited))
    res = solve2(*input, visited)
    print(res)