# The Computer Language Benchmarks Game
# https://salsa.debian.org/benchmarksgame-team/benchmarksgame/
#
# Translated from Mr Ledrug's C program by Jeremy Zerfas.
# Transliterated from GMP to built-in by Isaac Gouy

from sys import argv


def extract_Digit(nth):
    global tmp1, tmp2, acc, den, num
    tmp1 = num * nth
    tmp2 = tmp1 + acc
    tmp1 = tmp2 // den

    return tmp1


def eliminate_Digit(d):
    global acc, den, num
    acc = acc - den * d
    acc = acc * 10
    num = num * 10


def next_Term(k):
    global acc, den, num
    k2 = k*2+1
    acc = acc + num * 2
    acc = acc * k2
    den = den * k2
    num = num * k


def main():
    global tmp1, tmp2, acc, den, num
    n = int(argv[1])

    tmp1 = 0
    tmp2 = 0

    acc = 0
    den = 1
    num = 1

    i = 0
    k = 0
    while i < n:
        k += 1
        next_Term(k)

        if num > acc:
            continue

        d = extract_Digit(3)
        if d != extract_Digit(4):
            continue

        print(chr(48 + d), end="")
        i += 1
        if i % 10 == 0:
            print(f"\t:{i}")
        eliminate_Digit(d)
    remainder = i % 10
    if remainder != 0:
        print(f"{' ' * (10 - remainder)}\t:{i}")


if __name__ == '__main__':
    main()
