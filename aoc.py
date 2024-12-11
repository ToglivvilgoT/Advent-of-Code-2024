"""Module containing functions often used in AOC."""
import sys


sys.setrecursionlimit(40000)


def uncurry(func):
    return lambda tup: func(*tup)


def get_input(file_path: str) -> str:
    """Reads all content from file."""
    with open(file_path, 'r') as file:
        return file.read()
    

def get_input_lines(file_path: str) -> list[str]:
    """Reads all lines from file."""
    with open(file_path) as file:
        return file.readlines()