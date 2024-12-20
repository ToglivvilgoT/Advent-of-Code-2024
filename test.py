from functools import partial, cache


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


if __name__ == '__main__':
    test_cache()
