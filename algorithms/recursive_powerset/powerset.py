# Python 2.7
# real    0m5.421s

def powerset(set):
    if not set:
        return [[]]
    else:
        elem, remaining = set[0], set[1:]
        subpowersets = powerset(remaining)

        return [[elem] + subpowerset for subpowerset in subpowersets] + subpowersets

max = 21
print(len(powerset(range(1, max + 1))))