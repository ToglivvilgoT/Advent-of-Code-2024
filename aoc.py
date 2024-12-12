"""Module containing functions often used in AOC."""
import sys


sys.setrecursionlimit(40000)


def uncurry(func):
    return lambda tup: func(*tup)


def in_bounds(x: int, y: int, max_x: int, max_y: int) -> bool:
    """Return True if (x, y) is above or equal to (0, 0) and strictly below (max_x, max_y), False otherwise."""
    is_x = 0 <= x < max_x
    is_y = 0 <= y < max_y
    return is_x and is_y


Position = tuple[int, int]
def get_neighbours(pos: Position) -> list[Position]:
    """Returns a list of all neighbours of pos  (diagonals not included)."""
    neighbours = []
    for dx, dy in (0, 1), (1, 0), (-1, 0), (0, -1):
        x, y = pos
        neighbours.append((x + dx, y + dy))        
    return neighbours


def get_input(file_path: str) -> str:
    """Reads all content from file."""
    with open(file_path, 'r') as file:
        return file.read()
    

def get_input_lines(file_path: str) -> list[str]:
    """Reads all lines from file."""
    with open(file_path) as file:
        return file.readlines()