from aoc import get_input
from functools import partial, cache
from typing import Literal, Iterable, Any
from collections.abc import Callable


Towel = str
Pattern = str
ReturnType = Any
# This type alias is probably more confusing than helpful.
# I only used 'any' and 'sum' for this function.
SolveFunc = Callable[[Iterable[Literal[True] | ReturnType]], ReturnType]


def format_input(inp: str) -> tuple[list[Towel], list[Pattern]]:
    """Returns inp formated."""
    towels, patterns = inp.split('\n\n')
    towels = list(map(str.strip, towels.split(',')))
    patterns = patterns.splitlines()
    return towels, patterns


# I love how python just has DP implemented perfectly with just one simple line of code '@cache'
# Slightly over powered ngl.
@cache
def solve_one(patern: Pattern, towels: tuple[Towel], func: SolveFunc) -> ReturnType:
    """Solves the problem for one patern.
    If func is 'any': returns True if patern can be created (part 1).
    If func is 'sum': returns all possible ways patern can be created (part 2).
    """
    if not patern:
        return True
    
    # OK this line is banger ngl. (it is functionall so you have to read it backwards :))
    #                                                                               First We try to remove the prefix (towel) for every towel in towels.
    #                                          We then filter out those that did not match the prefix                               |
    #                                          (by checking if the length of patern is still the same <=> prefix wasn't removed).   |
    #               Then recursevely call solve_one with the rest of patern             |                                           |
    # Finally apply func to the result          |                                       |                                           |
    #        |                                  |                                       |                                           |
    #      /-V-\/-------------------------------V--------------\  /---------------------V------------------\  /---------------------V--------\
    return func(map(partial(solve_one, towels=towels, func=func), filter(lambda pat: len(patern) != len(pat), map(patern.removeprefix, towels))))


def solve(towels: list[Towel], patterns: list[Towel], find_all: bool) -> int:
    """Solves part 1 and 2.
    If find_all = True, solves part 2 else solves part 1.
    """
    if find_all:
        func = sum
    else:
        func = any
    # tuple(towels) is needed since list isnt hashable so it doesnt work with @cache
    return sum(map(partial(solve_one, towels=tuple(towels), func=func), patterns))


if __name__ == '__main__':
    inp = format_input(get_input('day19.txt'))
    res = solve(*inp, False)
    print(res)
    res = solve(*inp, True)
    print(res)