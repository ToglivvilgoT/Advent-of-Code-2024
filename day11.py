"""Solution for day 11 part 1 and 2."""
from aoc import get_input


Stone = int
Amount = int
Stones = dict[Stone, Amount]


def format_input(inp: str) -> Stones:
    """Formats the input."""
    inp = list(map(int, inp.split()))
    return {k: 1 for k in inp}


def str_to_int(string: str) -> int:
    """Helper function to turn str to int."""
    string = string.lstrip('0')
    return int(string) if string else 0


def dict_add(dic: Stones, key: Stone, val: Amount) -> None:
    """Adds key and val to dic.
    If key is in dic, sum the current val with new val.
    """
    if key in dic:
        dic[key] += val
    else:
        dic[key] = val


def solve(stones: Stones, iters: int = 25) -> int:
    """Solves part 1 and part 2, just change iters to 75 for part 2."""
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


if __name__ == '__main__':
    inp = format_input(get_input('day11.txt'))
    res = solve(inp)
    print(res)
    res = solve(inp, 75)
    print(res)