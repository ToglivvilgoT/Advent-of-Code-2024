from aoc import get_input, get_input_lines, uncurry, in_bounds, get_neighbours, sign
from functools import partial, reduce
from itertools import product
from collections import Counter


def format_input(inp: str):
    num_keypad = {}
    for x, val in enumerate('789'):
        num_keypad[val] = (x, 0)
    for x, val in enumerate('456'):
        num_keypad[val] = (x, 1)
    for x, val in enumerate('123'):
        num_keypad[val] = (x, 2)
    num_keypad['0'] = (1, 3)
    num_keypad['A'] = (2, 3)

    dir_keypad = {}
    dir_keypad['^'] = (1, 0)
    dir_keypad['A'] = (2, 0)
    dir_keypad['<'] = (0, 1)
    dir_keypad['v'] = (1, 1)
    dir_keypad['>'] = (2, 1)
    
    codes = inp.splitlines()

    return num_keypad, dir_keypad, codes


def next_moves(move, amount, dir_keypad, child_keypad):
    DIR_TO_KEY = {
        (-1, 0): '<',
        (1, 0): '>',
        (0, -1): '^',
        (0, 1): 'v',
        (0, 0): 'v',
    }

    curr_x, curr_y = move[0]
    next_x, next_y = move[1]
    dx = next_x - curr_x
    dy = next_y - curr_y

    next_poses = [dir_keypad['A']]
    if dx == 0 or dy == 0:
        next_poses += [dir_keypad[DIR_TO_KEY[(sign(dx), 0)]]] * abs(dx) + [dir_keypad[DIR_TO_KEY[(0, sign(dy))]]] * abs(dy)
    else:
        if dx < 0:
            if (next_x, curr_y) in child_keypad.values():
                next_poses += [dir_keypad[DIR_TO_KEY[(sign(dx), 0)]]] * abs(dx) + [dir_keypad[DIR_TO_KEY[(0, sign(dy))]]] * abs(dy)
            else:
                next_poses += [dir_keypad[DIR_TO_KEY[(0, sign(dy))]]] * abs(dy) + [dir_keypad[DIR_TO_KEY[(sign(dx), 0)]]] * abs(dx)
        else:
            if (curr_x, next_y) in child_keypad.values():
                next_poses += [dir_keypad[DIR_TO_KEY[(0, sign(dy))]]] * abs(dy) + [dir_keypad[DIR_TO_KEY[(sign(dx), 0)]]] * abs(dx)
            else:
                next_poses += [dir_keypad[DIR_TO_KEY[(sign(dx), 0)]]] * abs(dx) + [dir_keypad[DIR_TO_KEY[(0, sign(dy))]]] * abs(dy)
    next_poses.append(dir_keypad['A'])

    new_moves = Counter(zip(next_poses, next_poses[1:]))
    for key in new_moves:
        new_moves[key] *= amount
    
    return new_moves


def robot_presses(moves: Counter, dir_keypad, child_keypad):
    new_moves = Counter()
    for move, amount in moves.items():
        new_moves += next_moves(move, amount, dir_keypad, child_keypad)
    return new_moves


def move_len(moves):
    return moves.total()
    result = 0
    for move, amount in moves.items():
        (x1, y1), (x2, y2) = move
        dist = abs(x1 - x2) + abs(y1 - y2)
        result += (dist + 1) * amount
    return result


def move_to_sign(current_iter, dir_keypad):
    pos_to_sign = {val: key for key, val in dir_keypad.items()}
    def helper(move):
        _, pos = move
        return pos_to_sign[pos]
        
    signs = Counter()
    for move, amount in current_iter.items():
        signs[helper(move)] += amount
    return signs


def solveOne(code: str, num_keypad, dir_keypad, robots):
    poses = [num_keypad['A']] + list(map(lambda x: num_keypad[x], code))
    moves = list(zip(poses, poses[1:]))
    current_iter = Counter(moves)
    current_iter = robot_presses(current_iter, dir_keypad, num_keypad)
    for _ in range(robots):
        current_iter = robot_presses(current_iter, dir_keypad, dir_keypad)
    return move_len(current_iter) * int(code.replace('A', ''))


def solve(num_keypad, dir_keypad, codes, robots):
    return sum(map(partial(solveOne, num_keypad=num_keypad, dir_keypad=dir_keypad, robots=robots), codes))


if __name__ == '__main__':
    inp = format_input(get_input('day21.txt'))
    #res = solve(*inp, 2)
    #print(res)
    for i in range(0, 26):
        res = solve(*inp, i)
        print(i, res)