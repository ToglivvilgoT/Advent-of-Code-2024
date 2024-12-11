import aoc


def is_digit(text: str) -> bool:
    try:
        num = int(text)
        return 0 < num < 1000
    except ValueError:
        return False


def cartes(a: list, b: list) -> list:
    pairs = []
    for first in a:
        for last in b:
            pairs.append((first, last))

    return pairs


def test_substring(sub_str: str) -> bool:
    for num_len_1, num_len_2 in cartes((1, 2, 3), (1, 2, 3)):
        try:
            first_num = sub_str[4:4+num_len_1]
            second_num = sub_str[4+num_len_1+1:4+num_len_1+1+num_len_2]

            correct = sub_str[:4] == 'mul('
            correct &= is_digit(first_num)
            correct &= sub_str[4+num_len_1] == ','
            correct &= is_digit(second_num)
            correct &= sub_str[4+num_len_1+1+num_len_2] == ')'

            if correct:
                return int(first_num), int(second_num)
        except IndexError:
            continue
        
    return (False, False)


def is_do(sub_str):
    try:
        return sub_str[:4] == 'do()'
    except IndexError:
        return False


def is_dont(sub_str):
    try:
        return sub_str[:7] == "don't()"
    except IndexError:
        return False


def solve(lines: list[str]) -> int:
    result = 0
    active = True

    for line in lines:
        for i in range(len(line)):
            sub_str = line[i:]
            if is_do(sub_str):
                active = True
            elif is_dont(sub_str):
                active = False

            if active:
                first, second = test_substring(sub_str)
                if first and second:
                    result += first * second

    return result


if __name__ == '__main__':
    print(solve(aoc.get_input_lines('day3.txt')))