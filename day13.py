from aoc import get_input, get_input_lines, uncurry, in_bounds, get_neighbours
from functools import partial, reduce
from itertools import product
from math import gcd
import numpy


def format_input(inp: str):
    machines = []
    for machine in inp.split('\n\n'):
        lines = machine.splitlines()
        first = lines[0].split('+')
        first.pop(0)
        first[0] = int(first[0].split(',')[0])
        first[1] = int(first[1])

        second = lines[1].split('+')
        second.pop(0)
        second[0] = int(second[0].split(',')[0])
        second[1] = int(second[1])

        result = lines[2].split('=')
        result.pop(0)
        result[0] = int(result[0].split(',')[0])
        result[1] = int(result[1])

        machines.append((first, second, result))
    return machines


def tup_add(tup1, tup2):
    return [tup1[0] + tup2[0], tup1[1] + tup2[1]]


def tup_mul(tup, fctr):
    return list(map(lambda x: x * fctr, tup))


def is_solution(a, b, machine):
    a_button, b_button, expected = machine
    return tup_add(tup_mul(a_button, a), tup_mul(b_button, b)) == expected


def solve(machines):
    result = 0
    for machine in machines:
        solutions = set()
        for a in range(101):
            for b in range(101):
                if is_solution(a, b, machine):
                    solutions.add(a * 3 + b)
        result += min(solutions, default=0)
    return result


def eu_solve(a, b):
    if a == b:
        return ((1, 1), (0, 1))
    elif a % b == 1:
        return ((1, a), (-(a // b), b))

    next_a, next_b = eu_solve(b, a % b)
    next_b = next_b[0], a
    next_a = next_a[0] + (-(a // b)) * next_b[0], next_a[1]
    return next_b, next_a


def eucledes(machine):
    (ax, ay), (bx, by), (x, y) = machine
    equations = numpy.array([
        [ax, bx],
        [ay, by],
    ])
    solution = numpy.array([x, y])
    result = numpy.linalg.solve(equations, solution).astype(int)
    
    test1 = equations[0][0] * result[0] + equations[0][1] * result[1]
    test2 = equations[1][0] * result[0] + equations[1][1] * result[1]
    if test1 == x and test2 == y:
        return result[0] * 3 + result[1]
    else:
        print(test1 - x, test2 - y)
        return 0


def solve2(machines):
    result = 0
    for machine in machines:
        result += eucledes(machine)
    return result


if __name__ == '__main__':
    inp = format_input(get_input('day13.txt'))
    #res = solve(inp)
    #print(res)
    res = solve2(list(map((lambda mach: [mach[0], mach[1], (mach[2][0] + 10000000000000, mach[2][1] + 10000000000000)]), inp)))
    print(res)