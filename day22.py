from aoc import get_input, get_input_lines, uncurry, in_bounds, get_neighbours
from functools import partial, reduce
from itertools import product


def format_input(inp: str):
    return map(int, inp.splitlines())


def next_secret(secret_number):
    secret_number ^= secret_number << 6
    secret_number %= 16777216
    secret_number ^= secret_number >> 5
    secret_number ^= secret_number << 11
    secret_number %= 16777216
    return secret_number


def solveOne(secret_number):
    for _ in range(2000):
        secret_number = next_secret(secret_number)
    return secret_number


def solve(secret_numbers):
    return sum(map(solveOne, secret_numbers))


def secret_range(secret_number, iters):
    yield secret_number
    for _ in range(iters):
        secret_number = next_secret(secret_number)
        yield secret_number


def solve2(secret_numbers):
    secret_ranges = map(partial(secret_range, iters=2000), secret_numbers)
    change_to_prices = []
    i = 0
    for secret_rng in secret_ranges:
        print('Generating stockmarket:', i)
        prices = list(map(lambda n: n % 10, secret_rng))
        price_changes = list(map(lambda tup: tup[1] - tup[0], zip(prices, prices[1:])))
        change_to_prices.append({(a, b, c, d): val for a, b, c, d, val in reversed(list(zip(price_changes, price_changes[1:], price_changes[2:], price_changes[3:], prices[4:])))})
        i += 1

    bananas = []
    state_check = None
    for change in product(range(-9, 10), repeat=4):
        if state_check != change[:2]:
            state_check = change[:2]
            print('Performing monkey buisness', state_check)
        bananas.append(sum(map(lambda monkey: monkey.get(change, 0), change_to_prices)))

    return max(bananas)


if __name__ == '__main__':
    inp = format_input(get_input('day22.txt'))
    #res = solve(inp)
    #print(res)
    res = solve2(inp)
    print(res)