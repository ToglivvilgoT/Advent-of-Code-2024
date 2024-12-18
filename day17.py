"""Solution for day 17."""
from aoc import get_input
from itertools import product


Registers = dict[str, int]
Program = list[int]

def format_input(inp: str) -> tuple[Registers, Program]:
    """Formats the input and returns it."""
    lines = inp.splitlines()
    regs = {
        'A': int(lines[0].lstrip('Register A: ')),
        'B': int(lines[1].lstrip('Register B: ')),
        'C': int(lines[2].lstrip('Register C: ')),
    }
    prog = list(map(int, lines[4].lstrip('Program: ').split(',')))
    return regs, prog


def solve(regs: Registers, prog: Program) -> int:
    """Solves part 1 and returns the answer."""
    def combo(operand: int) -> int:
        """Given an operand, return the combo-operand."""
        if operand <= 3:
            return operand
        elif operand != 7:
            return regs[chr(ord('A') + operand - 4)]
        else:
            raise ValueError('Program failed, invalid combo operand 7')

    i = 0
    out = []
    while i < len(prog):
        match prog[i]:
            case 0:
                regs['A'] = int(regs['A'] / 2**combo(prog[i + 1]))
            case 1:
                regs['B'] = regs['B'] ^ prog[i + 1]
            case 2:
                regs['B'] = combo(prog[i + 1]) % 8
            case 3:
                if regs['A']:
                    i = prog[i + 1]
                    continue
            case 4:
                regs['B'] = regs['B'] ^ regs['C']
            case 5:
                out.append(combo(prog[i + 1]) % 8)
            case 6:
                regs['B'] = int(regs['A'] / 2**combo(prog[i + 1]))
            case 7:
                regs['C'] = int(regs['A'] / 2**combo(prog[i + 1]))
        i += 2
    return str.join(',', map(str, out))


def solve2(prog, out="") -> int | None:
    """Solves part 2 and returns the answer.
    out is the value of the A register.
    """
    if not prog:
        return int(out, base=2)

    # Slightly weird line, basically loops over all binary numbers from 0 to 7
    for test in map(''.join, product('01', repeat=3)):
        out += test
        a = int(out, base=2)
        b = a % 8
        b = b ^ 1
        c = a // 2**b
        b = b ^ c
        b = b ^ 4
        if prog[-1] == b % 8:
            result = solve2(prog[:-1], out)
            if result == None:
                out = out[:-3]
            else:
                return result
        else:
            out = out[:-3]
    else:
        return None


if __name__ == '__main__':
    inp = format_input(get_input('day17.txt'))
    res = solve(*inp)
    print(res)
    res = solve2(inp[1])
    print(res)