import aoc
from functools import partial
from itertools import product


def handle_input(raw: str):
    antennas = {}
    lines = raw.splitlines()
    for y in range(len(lines)):
        for x in range(len(lines[y])):
            if lines[y][x] != '.':
                typ = lines[y][x]
                if typ in antennas:
                    antennas[typ].append((x, y))
                else:
                    antennas[typ] = [(x, y)]
    return antennas, (len(lines[0]), len(lines))


def uncurry(func):
    return lambda tup: func(*tup)


def get_anti_nodes(antennas: dict):
    anti_nodes = set()
    for group in antennas.values():
        for node1, node2 in product(group, repeat=2):
            if node1 == node2:
                continue
            dx = node1[0] - node2[0]
            dy = node1[1] - node2[1]
            anti_nodes.add((node1[0] + dx, node1[1] + dy))
            anti_nodes.add((node2[0] - dx, node2[1] - dy))
    return anti_nodes


def in_bounds(max_x, max_y, x, y):
    is_x = 0 <= x < max_x
    is_y = 0 <= y < max_y
    return is_x and is_y


def solve(antennas, size):
    return len(list(filter(uncurry(partial(in_bounds, *size)), get_anti_nodes(antennas))))


def get_line(x, y, dx, dy, size):
    nodes = set()
    for dx, dy in ((dx, dy), (-dx, -dy)):
        for step in range(100000):
            new_node = x + dx * step, y + dy * step
            if in_bounds(*size, *new_node):
                nodes.add(new_node)
            else:
                break
    return nodes


def get_anti_nodes2(antennas: dict, size):
    anti_nodes = set()
    for group in antennas.values():
        for node1, node2 in product(group, repeat=2):
            if node1 == node2:
                continue
            dx = node1[0] - node2[0]
            dy = node1[1] - node2[1]
            anti_nodes |= get_line(*node1, dx, dy, size)
    return anti_nodes


def solve2(antennas, size):
    return len(get_anti_nodes2(antennas, size))


if __name__ == '__main__':
    antennas, size = handle_input(aoc.get_input('day8.txt'))
    res = solve(antennas, size)
    print(res)
    res = solve2(antennas, size)
    print(res)