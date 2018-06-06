#!/usr/bin/env python
import sys
import subprocess

def main():
    base_args = ['stack', 'exec', '--', 'supercompile']
    args = base_args
    args.extend(sys.argv[1:])

    print("[+] Compiling once", args)
    print(args)

    subprocess.call(args)

    print("[+] Extracting code into new file")
    file_name = sys.argv[1]

    lines = []
    with open("output/" + file_name.replace(".core", ".hs"), "r") as f:
        for line in f:
            # Strip off any optional type signatures
            if "::" in line:
                line = line[:line.index("::")]
            lines.append(line.rstrip('\n'))

    start_idx = -1
    end_idx = -1
    for i, line in enumerate(lines):
        if line == "root = let":
            start_idx = i
        if line == "tests = let":
            end_idx = i
    print(start_idx, end_idx)

    with open("./tmp/input.core", "w") as f:
        f.write('\n'.join(lines[start_idx:end_idx]))
        f.write('\n')
        f.write('tests = []\n')

    args = base_args
    args.append("tmp/input.core")
    args.extend(sys.argv[2:])

    subprocess.call(args)

if __name__ == '__main__':
    if len(sys.argv) < 1:
        print("Usage: compileTwice [file.hs]")
        sys.exit(-1)
    main()
