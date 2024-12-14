from aoc import get_input, get_input_lines, uncurry, in_bounds, get_neighbours
from functools import partial, reduce
from itertools import product
import pygame
pygame.display.set_mode((101*4, 103*4))


def format_input(inp: str):
    formated = []
    lines = inp.splitlines()
    for line in lines:
        pos, vel = line.split(' ')
        pos = pos[2:]
        vel = vel[2:]
        pos = tuple(map(int, pos.split(',')))
        vel = tuple(map(int, vel.split(',')))
        formated.append((pos, vel))
    return formated


def update_positions(robots, width, height, iters):
    new_robots = []
    for (x, y), (dx, dy) in robots:
        x = (x + dx * iters) % width
        y = (y + dy * iters) % height
        new_robots.append(((x, y), (dx, dy)))
    return new_robots


def quadrant_count(robots, width, height):
    quadrants = [0, 0, 0, 0]
    half_width = width // 2
    half_height = height // 2
    for (x, y), _ in robots:
        if x < half_width:
            if y < half_height:
                quadrants[0] += 1
            elif y > half_height:
                quadrants[1] += 1
        elif x > half_width:
            if y < half_height:
                quadrants[2] += 1
            elif y > half_height:
                quadrants[3] += 1
    return reduce(lambda a, b: a * b, quadrants, 1)


def solve(robots, width, height, iters):
    return quadrant_count(update_positions(robots, width, height, iters), width, height)


def solve2(robots, width, height):
    running = True
    iters = 0
    surface = pygame.surface.Surface((101, 103))
    while running:
        for event in pygame.event.get():
            if event.type == pygame.KEYDOWN:
                if event.key == pygame.K_d:
                    robots = update_positions(robots, width, height, 1)
                    iters += 1
                if event.key == pygame.K_a:
                    robots = update_positions(robots, width, height, -1)
                    iters -= 1
                if event.key == pygame.K_w:
                    robots = update_positions(robots, width, height, 101)
                    iters += 101
                if event.key == pygame.K_s:
                    robots = update_positions(robots, width, height, -101)
                    iters -= 101
                print(iters)
                surface.fill((0, 0, 0))
                for (x, y), _ in robots:
                    surface.set_at((x, y), (0, 127, 23))
                pygame.transform.scale(surface,  pygame.display.get_window_size(), pygame.display.get_surface())
                pygame.display.update()
            elif event.type == pygame.QUIT:
                running = False
    return iters


if __name__ == '__main__':
    inp = format_input(get_input('day14.txt'))
    res = solve(inp, 101, 103, 100)
    print(res)
    res = solve2(inp, 101, 103)
    print(res)