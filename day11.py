from aoc import get_input, get_input_lines, uncurry
from functools import partial, reduce
from itertools import product


def format_input(inp: str):
    inp = list(map(int, inp.split()))
    return {k: 1 for k in inp}


def str_to_int(string: str):
    string = string.lstrip('0')
    return int(string) if string else 0


def dict_add(dic, key, val):
    if key in dic:
        dic[key] += val
    else:
        dic[key] = val


def solve(stones: dict[int, int], iters: int = 25):
    next_gen = {}
    for stone, amount in stones.items():
        if stone == 0:
            dict_add(next_gen, 1, amount)
        else:
            str_stone = str(stone)
            if len(str_stone) % 2 == 0:
                half = len(str_stone) // 2
                dict_add(next_gen, int(str_to_int(str_stone[:half])), amount)
                dict_add(next_gen, int(str_to_int(str_stone[half:])), amount)
            else:
                dict_add(next_gen, stone * 2024, amount)

    iters -= 1
    if iters:
        return solve(next_gen, iters)
    else:
        return sum(next_gen.values())


def solve2(inp):
    pass


if __name__ == '__main__':
    inp = format_input(get_input('day11.txt'))
    res = solve(inp)
    print(res)
    res = solve(inp, 75)
    print(res)