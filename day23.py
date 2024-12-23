from aoc import get_input, get_input_lines, uncurry, in_bounds, get_neighbours
from functools import partial, reduce
from itertools import product
from collections import defaultdict


def format_input(inp: str):
    network = defaultdict(set)
    for line in inp.splitlines():
        fst, snd = line.split('-')
        network[fst].add(snd)
        network[snd].add(fst)
    return network


def is_complete(computers, network):
    if not computers:
        return True

    head, tail = computers[0], computers[1:]
    for neighbour in tail:
        if neighbour not in network[head]:
            return False

    return is_complete(tail, network)


def solve(network):
    triples = set()
    for computer in network:
        if computer[0] != 't':
            continue
        for comp1, comp2 in product(network[computer], repeat=2):
            if comp1 in network[comp2]:
                triples.add(frozenset((computer, comp1, comp2)))
    return len(triples)


def connected_to(computer, group, network):
    for neighbour in group:
        if computer not in network[neighbour]:
            return False
    return True


def solve2(network: dict[str, str]):
    groups: set[frozenset[str]] = set()
    for computer in network:
        for comp1, comp2 in product(network[computer], repeat=2):
            if comp1 in network[comp2]:
                groups.add(frozenset((computer, comp1, comp2)))

    for _ in range(100):
        new_groups = set()
        for group in groups:
            group = set(group)
            fst = group.pop()
            for neighbour in network[fst]:
                if neighbour not in group and connected_to(neighbour, group, network):
                    new_groups.add(frozenset(group | {fst} | {neighbour}))
        if new_groups:
            groups = new_groups
        else:
            return str.join(',', sorted(list(max(groups))))

    #while groups:
    #    last_groups = groups
    #    groups = set()
    #    for group1, group2 in product(last_groups, repeat=2):
    #        diff = set(frozenset.symmetric_difference(group1, group2))
    #        if len(diff) == 2 and diff.pop() in network[diff.pop()]:
    #            groups.add(group1 | group2)
    


if __name__ == '__main__':
    inp = format_input(get_input('day23.txt'))
    res = solve(inp)
    print(res)
    res = solve2(inp)
    print(res)