from collections import Counter
from itertools import permutations, product


# Expand these for more units and prefixes. I'm a lazy bastard so
# I won't include all the units you might be interested in.
units = ['m', 'Pa', 'N']
prefixes = ['k', 'm', 'M', 'p', 'P']



def collisions(units, prefixes=prefixes):
    counts = Counter(a+b for (a,b) in list(permutations(units, 2)) + list(product(prefixes, units)))
    return sum(counts.values()) - len(counts)

if __name__ == '__main__':
    print("Collisions currently: {}".format(collisions(units)))
    print("Collisions with lowercase: {}".format(collisions([e.lower() for e in units])))