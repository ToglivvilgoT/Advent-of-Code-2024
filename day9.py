from aoc import get_input


class File:
    """Struct for Files, used for part 2"""
    def __init__(self, size: int, id: int) -> None:
        self.size = size
        self.id = id

    def __le__(self, other: 'File') -> bool:
        """Used to check if file can fit in free space."""
        return self.size <= other.size

    def __str__(self) -> str:
        """Used for debug only."""
        return f'({self.size}, {self.id})'

    def __repr__(self) -> str:
        """Used for debug only."""
        return f'({self.size}, {self.id})'


class Free:
    """Didn't end up using this one, used only Files instead."""
    def __init__(self, size: int) -> None:
        self.size = size


class DiskMap:
    """Struct for storing a list of Files. Used for part 2."""
    def __init__(self):
        self.files = []


def format_input(inp: str):
    """Format input for part 1
    for part one I did not use any of my fansy classes
    instead the file system is represented by a long list of id's where
    -1 as id represents free space.
    """
    disk_map = []
    length = 0
    for i, char in enumerate(inp.strip()):
        if i % 2 == 0:
            # It is a file
            for _ in range(int(char)):
                disk_map.append(i // 2)
            length += int(char)
        else:
            # It is free space
            for _ in range(int(char)):
                disk_map.append(-1)
    return disk_map, length


def disk_insert(disk_map: list[int], reversed_disk_map: list[int]) -> list[int]:
    """Inserts files into free space for part 1.
    I tried to do this recursively but reached recursive depth limit even after expanding
    it to 40000."""
    # Recursive solution that cooked (cooked my computer)
    """
    if not disk_map:
        return reversed_disk_map
    elif not reversed_disk_map:
        return disk_map

    if disk_map[0] == -1:
        return reversed_disk_map[:1] + disk_insert(disk_map[1:], reversed_disk_map[1:])
    else:
        return disk_map[:1] + disk_insert(disk_map[1:], reversed_disk_map)
    """
    # Cringe imperative solution that actually works
    j = 0
    for i in range(len(disk_map)):
        if disk_map[i] == -1:
            disk_map[i] = reversed_disk_map[j]
            j += 1
    return disk_map


def index_sum(lst):
    """Return the checksum for part 1. Note that lst should only contain the files
    and not any empty space."""
    return sum(map(lambda tup : tup[0] * tup[1], enumerate(lst)))


def solve(disk_map: list[int], length: int):
    """Solves part 1."""
    compact = disk_insert(disk_map, list(filter(lambda x: x != -1, reversed(disk_map))))[:length]
    return index_sum(compact)


def format_input2(inp: str):
    """Formats input for part 2 using the fansy classes.
    Could I have used a list instead of DiskMap? Yes. Did I? No.
    """
    disk_map = DiskMap()
    for i, char in enumerate(inp.strip()):
        if i % 2 == 0:
            disk_map.files.append(File(int(char), i // 2))
        else:
            disk_map.files.append(File(int(char), -1))
    return disk_map


def index_sum2(disk_map: DiskMap):
    """Calculates the checksum for part 2."""
    index = 0
    sum = 0
    for file in disk_map.files:
        for _ in range(file.size):
            if file.id != -1:
                sum += file.id * index
            index += 1
    return sum


def solve2(disk_map: DiskMap):
    """Solves part 2."""
    i = len(disk_map.files) - 1
    while i > 0:
        curr_file = disk_map.files[i]
        if curr_file.id != -1:
            for j in range(i):
                free = disk_map.files[j]
                if free.id == -1 and curr_file <= free:
                    disk_map.files.insert(j, curr_file)
                    disk_map.files[i+1] = File(curr_file.size, -1)
                    free.size -= curr_file.size
                    i += 1
                    break
        i -= 1

    return index_sum2(disk_map)

if __name__ == '__main__':
    inp = format_input(get_input('day9.txt'))
    res = solve(*inp)
    print(res)
    inp = format_input2(get_input('day9.txt'))
    res = solve2(inp)
    print(res)