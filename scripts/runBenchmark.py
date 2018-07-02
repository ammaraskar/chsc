#!/usr/bin/env python3
import sys
import subprocess
import os
from pathlib import Path

def compile(file_name):
    command = ["stack", "exec", "--", 
               "supercompile", "--time-limit", sys.argv[2], 
               "--json", "statistics/" + file_name + ".compile.json"]
    print("> " + (" ".join(command)))

    subprocess.check_call(
        command,
        env=dict(os.environ, BENCHMARK_FILE=sys.argv[1])
    )

    command = ["stack", "exec", "--", "supercompile", sys.argv[1]]
    print("> " + (" ".join(command)))
    subprocess.call(command)

def main():
    if len(sys.argv) < 3:
        print("Usage: runBenchmark.py <file> <time>")
        sys.exit(1)

    path = Path(sys.argv[1])
    file_name = path.name
    print("[+] Benchmarking compile time for " + file_name)

    if "--skip-compile" not in sys.argv:
        compile(file_name)

    # Compile the regular program
    command = ["stack", "ghc", "--", "-O2", 
               "-o", "statistics/" + file_name + ".compiled", 
               str(Path("input") / path.parent / (path.stem + ".hs"))]
    print("[+] Compiling input program")
    print("> " + (" ".join(command)))

    subprocess.check_call(command)

    # Compile the supercompiled program
    command = ["stack", "ghc", "--", "-O2", 
               "-o", "statistics/" + file_name + ".supercompiled", 
               str(Path("output") / path.parent / (path.stem + ".hs"))]
    print("[+] Supercompiling input program")
    print("> " + (" ".join(command)))

    subprocess.check_call(command)

    # Run the regular program
    command = ["statistics/" + file_name + ".compiled",
               "--time-limit", sys.argv[2],
               "--json", "statistics/" + file_name + ".compiled.json"]
    print("> " + (" ".join(command)))
    subprocess.check_call(command)

    # Run the supercompiled program
    command = ["statistics/" + file_name + ".supercompiled",
               "--time-limit", sys.argv[2],
               "--json", "statistics/" + file_name + ".supercompiled.json"]
    print("> " + (" ".join(command)))
    subprocess.check_call(command)

if __name__ == "__main__":
    main()