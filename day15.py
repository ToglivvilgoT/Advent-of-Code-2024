from aoc import get_input, get_input_lines, uncurry, in_bounds, get_neighbours
from functools import partial, reduce
from itertools import product
from queue import Queue


def format_input(inp: str):
    warehouse, moves = inp.split('\n\n')
    moves = moves.replace('\n', '')
    warehouse = list(map(list, warehouse.splitlines()))
    for y, row in enumerate(warehouse):
        for x, tile in enumerate(row):
            if tile == '@':
                warehouse[y][x] = '.'
                return warehouse, moves, (x, y)


def try_push(warehouse, x, y, dx, dy):
    new_x = x + dx
    new_y = y + dy
    match warehouse[new_y][new_x]:
        case '.':
            return new_x, new_y
        case '#':
            return False
        case 'O':
            return try_push(warehouse, new_x, new_y, dx, dy)


def box_coord_sum(warehouse, box='O'):
    answer = 0
    for y, row in enumerate(warehouse):
        for x, tile in enumerate(row):
            if tile == box:
                answer += 100 * y + x
    return answer


MOVES2DIR = {
        '<': (-1, 0),
        '>': (1, 0),
        '^': (0, -1),
        'v': (0, 1),
    }


def solve(warehouse: list[str], moves: str, robo_pos: tuple[int, int]):
    for move in moves:
        vel = MOVES2DIR[move]
        x, y = robo_pos
        dx, dy = vel
        new_x = x + dx
        new_y = y + dy
        match warehouse[new_y][new_x]:
            case '.':
                robo_pos = (new_x, new_y)
            case 'O':
                push = try_push(warehouse, new_x, new_y, dx, dy)
                if push:
                    warehouse[push[1]][push[0]] = 'O'
                    warehouse[new_y][new_x] = '.'
                    robo_pos = (new_x, new_y)
    return box_coord_sum(warehouse)


def widen(warehouse, robo_pos):
    thicc_warehouse = []
    for row in warehouse:
        thicc_row = []
        for tile in row:
            match tile:
                case '#':
                    thicc_row.append('#')
                    thicc_row.append('#')
                case '.':
                    thicc_row.append('.')
                    thicc_row.append('.')
                case 'O':
                    thicc_row.append('[')
                    thicc_row.append(']')
        thicc_warehouse.append(thicc_row)
    return thicc_warehouse, (robo_pos[0] * 2, robo_pos[1])


def try_push_horizontal(warehouse, x, y, dx):
    new_x = x + 2 * dx
    match warehouse[y][new_x]:
        case '.':
            warehouse[y][new_x] = warehouse[y][new_x - dx]
            warehouse[y][new_x - dx] = warehouse[y][x]
            warehouse[y][x] = '.'
            return True
        case '#':
            return False
        case _:
            if try_push_horizontal(warehouse, new_x, y, dx):
                warehouse[y][new_x] = warehouse[y][new_x - dx]
                warehouse[y][new_x - dx] = warehouse[y][x]
                warehouse[y][x] = '.'
                return True
            else:
                return False


def try_push_vertical(warehouse, x, y, dy):
    pushes = []
    que = Queue()
    que.put_nowait((x, y))
    pushes.append((x, y))
    if warehouse[y][x] == '[':
        que.put_nowait((x + 1, y))
        pushes.append((x + 1, y))
    else:
        que.put_nowait((x - 1, y))
        pushes.append((x - 1, y))

    while not que.qsize() == 0:
        x, y = que.get_nowait()
        y += dy
        match warehouse[y][x]:
            case '.':
                pass
            case '#':
                return False
            case box:
                if (x, y) in pushes:
                    continue
                    
                que.put_nowait((x, y))
                pushes.append((x, y))
                if box == '[':
                    que.put_nowait((x + 1, y))
                    pushes.append((x + 1, y))
                else:
                    que.put_nowait((x - 1, y))
                    pushes.append((x - 1, y))
    
    for x, y in reversed(pushes):
        warehouse[y + dy][x] = warehouse[y][x]
        warehouse[y][x] = '.'
    return True


def solve2(warehouse, moves, robo_pos):
    for move in moves:
        vel = MOVES2DIR[move]
        x, y = robo_pos
        dx, dy = vel
        new_x = x + dx
        new_y = y + dy
        match warehouse[new_y][new_x]:
            case '.':
                robo_pos = (new_x, new_y)
            case '[' | ']':
                if dx and try_push_horizontal(warehouse, new_x, new_y, dx):
                    robo_pos = new_x, new_y
                elif dy and try_push_vertical(warehouse, new_x, new_y, dy):
                    robo_pos = new_x, new_y
    return box_coord_sum(warehouse, box='[')


if __name__ == '__main__':
    warehouse, moves, robo_pos = format_input(get_input('day15.txt'))
    res = solve(warehouse, moves, robo_pos)
    print(res)

    warehouse, moves, robo_pos = format_input(get_input('day15.txt'))
    warehouse, robo_pos = widen(warehouse, robo_pos)
    res = solve2(warehouse, moves, robo_pos)
    print(res)