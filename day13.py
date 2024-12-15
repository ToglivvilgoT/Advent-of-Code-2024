"""Solution for day 13"""
from aoc import get_input
from itertools import product
import numpy


X = int
Y = int
Abutton = list[X | Y]
Bbutton = list[X | Y]
Prize = list[X | Y]
Machine = tuple[Abutton, Bbutton, Prize]


def format_input(inp: str) -> list[Machine]:
    """Formats the input into a list of Machines."""
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


def lst_add(lst1: list[int], lst2: list[int]) -> list[int]:
    """Adds the element togheter from two lists, lists must have length 2."""
    return [lst1[0] + lst2[0], lst1[1] + lst2[1]]


def lst_mul(lst: list[int], fctr: int) -> list[int]:
    """Multiplies each element of lst by fctr."""
    return list(map(lambda x: x * fctr, lst))


def is_solution(a: int, b: int, machine: Machine):
    """Returns True if a and b amount of Abutton and Bbutton presses gets the prize."""
    a_button, b_button, expected = machine
    return lst_add(lst_mul(a_button, a), lst_mul(b_button, b)) == expected


def solve(machines: list[Machine]) -> int:
    """Solves part 1 and returns the answer.
    It does this by just brute force trying all combinations up to 100 presses for each button.
    """
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
    """First part of solving a diofantic equation.
    Did not end up using this function since it did not provide an answer
    it just gives an infinit list of possible answers.
    """
    if a == b:
        return ((1, 1), (0, 1))
    elif a % b == 1:
        return ((1, a), (-(a // b), b))

    next_a, next_b = eu_solve(b, a % b)
    next_b = next_b[0], a
    next_a = next_a[0] + (-(a // b)) * next_b[0], next_a[1]
    return next_b, next_a


def solveOne(machine: Machine) -> int:
    """Solve one machine, return the answer for that machine or 0 if prize cant be grabed."""
    (ax, ay), (bx, by), (x, y) = machine
    equations = numpy.array([
        [ax, bx],
        [ay, by],
    ])
    solution = numpy.array([x, y])
    result = numpy.linalg.solve(equations, solution).astype(int)
    
    # I use the for loop to check the values around result
    # Since numpy.linalg.solve uses floats the answer may experience some rounding error
    # there for this for loop is needed.
    for dx, dy in product(range(-10, 11), repeat=2):
        res_x = result[0] + dx
        res_y = result[1] + dy
        test_x = ax * res_x + bx * res_y
        test_y = ay * res_x + by * res_y
        if test_x == x and test_y == y:
            return res_x * 3 + res_y
    return 0


def solve2(machines: list[Machine]) -> int:
    """Solves part 2, this also works for part one and is faster."""
    result = 0
    for machine in machines:
        result += solveOne(machine)
    return result


if __name__ == '__main__':
    inp = format_input(get_input('day13.txt'))
    res = solve(inp)
    print(res)
    # This line is just adds 10000000000000 to all input Machines, it is very readable thank you.
    res = solve2(list(map((lambda mach: [mach[0], mach[1], (mach[2][0] + 10000000000000, mach[2][1] + 10000000000000)]), inp)))
    print(res)