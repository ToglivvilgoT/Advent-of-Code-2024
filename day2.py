import aoc


def testSafety(levels: list[int]) -> bool:
    inc = dec = True

    for i in range(1, len(levels)):
        diff = abs(levels[i-1] - levels[i])

        if diff == 0 or diff > 3:
            inc = dec = False
            break

        if levels[i-1] < levels[i]:
            inc &= True
            dec = False
        elif levels[i-1] > levels[i]:
            dec &= True
            inc = False
        else:
            inc = dec = False
            break

    return inc or dec


def solve(lines: list[str]):
    safe = 0

    for line in lines:
        levels = line.split()
        levels = list(map(int, levels))
        if testSafety(levels):
            safe += 1
            continue

        # Part 2
        for rem in range(len(levels)):
            new_levels = levels.copy()
            new_levels.pop(rem)

            if testSafety(new_levels):
                safe += 1
                break
        
        
    return safe




if __name__ == '__main__':
    print(solve(aoc.get_input_lines('day2.txt')))