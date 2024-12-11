import aoc


def input_format(lines):
    rules = {}
    manuals = []

    for i in range(len(lines)):
        line = lines[i]
        if line == '\n':
            break

        before, after = map(int, line.split('|'))
        if before in rules:
            rules[before].append(after)
        else:
            rules[before] = [after]

    for i in range(i+1, len(lines)):
        manuals.append(list(map(int, lines[i].split(','))))

    return rules, manuals


def solve(rules, manuals) -> int:
    ans = 0
    incorrect = []
    for manual in manuals:
        ordered = True
        prev = set()
        for page in manual:
            for illegal in rules.get(page, []):
                if illegal in prev:
                    ordered = False
                    break
            prev.add(page)
            if not ordered:
                break
        if ordered:
            ans += manual[len(manual) // 2]
        else:
            incorrect.append(manual)

    return ans, incorrect


def solve2(rules, manuals) -> int:
    ans = 0
    for manual in manuals:
        positions = {}
        for page in manual:
            positions[page] = 0
        
        for page in manual:
            for after in rules.get(page, []):
                if after in positions:
                    positions[after] += 1

        for page, pos in positions.items():
            if pos == len(manual) // 2:
                ans += page

    return ans


if __name__ == '__main__':
    ans1, incorrect = solve(*input_format(aoc.get_input_lines('day5.txt')))
    print(ans1)
    ans2 = solve2(input_format(aoc.get_input_lines('day5.txt'))[0], incorrect)
    print(ans2)