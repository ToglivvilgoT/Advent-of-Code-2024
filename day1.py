import aoc

        
def solve(lines: list[str]):
    """Solves part 1"""
    left = []
    right = []
    for line in lines:
        num1, num2 = line.split()
        left.append(int(num1))
        right.append(int(num2))

    left.sort()
    right.sort()

    sum = 0
    for l, r in zip(left, right):
        sum += abs(l - r)

    return sum


def solve2(lines: list[str]):
    left = []
    right = {}
    for line in lines:
        l, r = line.split()
        left.append(int(l))
        r = int(r)
        if r in right:
            right[r] += 1
        else:
            right[r] = 1

    sum = 0
    for val in left:
        try:
            sum += val * right[val]
        except KeyError:
            continue

    return sum


if __name__ == '__main__':
    print(solve(aoc.get_input_lines('day1.txt')))