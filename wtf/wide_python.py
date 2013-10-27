letters = {
    2: ['a','b','c'],
    3: ['d','e','f'],
    4: ['g','h','i'],
    5: ['j','k','l'],
    6: ['m','n','o'],
    7: ['p','q','r','s'],
    8: ['t','u','v'],
    9: ['w','x','y','z']
}

def findword(needle, haystack):
    words = lambda number: (''.join(w) for w in itertools.product(*[letters[int(n)] for n in number]))
    return filter(lambda number: any(needle in word for word in words(number)), haystack)