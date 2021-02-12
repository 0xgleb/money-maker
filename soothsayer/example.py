import fileinput

for line in fileinput.input():
    print(line.rstrip() + ' from python', flush=True)
