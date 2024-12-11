import aoc
from itertools import product


def is_in_bounds(lines, x, y):
    return 1 <= y < len(lines) and 0 <= x < len(lines[y])


def is_word(lines, word, x, y, direction):
    if not word:
        return True
    elif is_in_bounds(lines, x, y) and lines[y][x] == word[0]:
        x = x + direction[0]
        y += direction[1]
        return is_word(lines, word[1:], x, y, direction)
    else:
        return False


def solve(lines: list[str]) -> int:
    sum = 0
    for y in range(len(lines)):
        for x in range(len(lines[y])):
            for direction in product((-1, 0, 1), (-1, 0, 1)):
                sum += is_word(lines, 'XMAS', x, y, direction)
    return sum


def solve2(lines: list[str]) -> int:
    sum = 0
    for y in range(len(lines)):
        for x in range(len(lines[y])):
            crosses = 0
            for direction in ((-1, -1), (1, -1), (1, 1), (-1, 1)):
                start_x = x - direction[0]
                start_y = y - direction[1]
                crosses += is_word(lines, 'MAS', start_x, start_y, direction)
            if crosses >= 2:
                sum += 1
    return sum


if __name__ == '__main__':
    print(solve2(aoc.get_input_lines('day4.txt')))