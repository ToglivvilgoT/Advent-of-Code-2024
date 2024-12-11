import aoc
import sys
from copy import deepcopy


sys.setrecursionlimit(10000)


def format_input(input: str):
    equations = []
    for line in input.splitlines():
        equation = line.split(':')
        test_value = int(equation[0])
        test_nums = list(map(int, equation[1].split()))
        equations.append((test_value, test_nums))
    return equations


def solve_equ(test_value, test_nums, build_up = 0):
    if not test_nums:
        return test_value == build_up

    return solve_equ(test_value, test_nums[1:], build_up + test_nums[0]) or solve_equ(test_value, test_nums[1:], build_up * test_nums[0])


def uncurry(func):
    return lambda tup: func(*tup)


def solve(equations):
    return sum(map(lambda x: x[0], filter(uncurry(solve_equ), equations)))


def solve_equ2(test_value, test_nums, build_up = 0):
    if not test_nums:
        return test_value == build_up

    return solve_equ2(test_value, test_nums[1:], build_up + test_nums[0]) or \
           solve_equ2(test_value, test_nums[1:], build_up * test_nums[0]) or \
           solve_equ2(test_value, test_nums[1:], int(str(build_up) + str(test_nums[0])))


def solve2(equations):
    return sum(map(lambda x: x[0], filter(uncurry(solve_equ2), equations)))


if __name__ == '__main__':
    input = format_input(aoc.get_input('day7.txt'))
    res = solve(input)
    print(res)
    res = solve2(input)
    print(res)