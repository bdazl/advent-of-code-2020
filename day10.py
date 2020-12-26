# Read ints from file
def readInts(filename):
    ints = []
    with open(filename, 'r') as f:
        ints = [int(l) for l in f]
    return ints

# Ints are sorted and extended (add 0 at start and (last sorted element)+3)
def normalize(ints):
    sints = sorted(ints)
    return [0] + sints + [sints[-1] + 3]

# All elements n reaches into nints (must have a diff of at most 3)
def reaches(n, nints):
    return [m for m in nints if m - n <= 3]

# For each element in nints return a list of what elements are reached
def reachMap(nints):
    return [reaches(v, nints[i+1:]) for i,v in enumerate(nints)]

# Count elements of list in list
def counts(rm):
    return [len(m) for m in rm]

# Given a list that counts how far that element reaches, calculate the branch
# count of the tree that would be produced
def branches(cnts):
    rev = list(reversed(cnts))
    out = [1]
    for r in rev[1:]:
        out = [sum(out[:r])] + out
    return out

ints = readInts("day10.txt")
nints = normalize(ints)
rints = reachMap(nints)
cints = counts(rints)
bints = branches(cints)

print(bints[0])
