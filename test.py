from functools import partial, cache, reduce
from collections import Counter, defaultdict


def test_match():
    lines = [
        'Non Object 345, 325. Is on this "abc" tile',
        'Non Object 567, 9. Is not on this "dfg" tile',
        'Non Object -4, 29348. Is on that "hjk" tile',
    ]


    for line in lines:
        match line:
            case 'Non Object', first, second, *rest:
                print(first, second)


def test_matrix():
    input = [
        '....#.....',
        '.........#',
        '..........',
        '..#.......',
        '.......#..',
        '..........',
        '.#..^.....',
        '........#.',
        '#.........',
        '......#...',
    ]

    for line in list(map(lambda line: list(map(lambda char: '.' if char == '^' else char, line)), input)):
        print(line)


def test_partial():
    def foo(a, b, c):
        return a + b * c
    
    bar = partial(foo, a=1, c=2)
    
    try:
        print(bar(3))
    except:
        print('bar(3) was not allowed')

    try:
        print(bar(b=3))
    except:
        print('bar(b=3) was not allowed')


def test_kwarg_arg():
    def foo(*args, optioanl_side_content: bool):
        print(args)

    def bar(**kwargs):
        print(kwargs)

    def foo_bar(must_have, *args, **kwargs):
        print(must_have, args, kwargs)

    foo('hej', 3, optioanl_side_content=True)
    bar(nej='hej', number=7)
    foo_bar('hej', 'då', name='Janson', surname='Jacob')


def get_incrementor():
    count = 0
    @cache
    def increase_by(n):
        nonlocal count
        count += n
        return count
    return increase_by


def test_cache():
    counter = get_incrementor()
    print(counter(3))
    print(counter(4))
    print(counter(3))
    print(counter(4))
    print(counter(0))


def test_counter():
    numbers = []
    for line in open('day18.txt').readlines():
        numbers += list(map(int, line.split(',')))
    counter = Counter(numbers)
    print(counter.most_common(3))
    print(counter.total())
    counter.clear()
    counter['a'] += 1
    counter.update(['a', 'b', 'c'])
    print(counter.most_common(3))


def test_default_dict():
    dic = defaultdict(lambda: 0)
    dic['hi'] += 1
    print(dic.items())


if __name__ == '__main__':
    test_counter()
