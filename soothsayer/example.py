import fileinput

for line in fileinput.input():
    print(line.rstrip(), flush=True)
