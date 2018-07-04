#!/usr/bin/env python3
import sys
import subprocess
import os
import json
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

def memory_info(file_name):
    command = ["statistics/" + file_name + ".compiled", "-n", "1", "+RTS", "-s"]
    print("> " + (" ".join(command)))
    output = subprocess.run(command, stderr=subprocess.PIPE).stderr.decode().splitlines()

    bytes_line = [x for x in output if "bytes allocated" in x][0]
    bytes_allocated = bytes_line.split()[0].replace(",", "")
    comp_bytes_allocated = int(bytes_allocated)

    command = ["statistics/" + file_name + ".supercompiled", "-n", "1", "+RTS", "-s"]
    print("> " + (" ".join(command)))
    output = subprocess.run(command, stderr=subprocess.PIPE).stderr.decode().splitlines()

    bytes_line = [x for x in output if "bytes allocated" in x][0]
    bytes_allocated = bytes_line.split()[0].replace(",", "")
    supercomp_bytes_allocated = int(bytes_allocated)

    with open("statistics/" + file_name + ".memory.json", "w") as f:
        data = {
            'supercompiled': supercomp_bytes_allocated,
            'compiled': comp_bytes_allocated
        }
        json.dump(data, f)

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

    memory_info(file_name)

if __name__ == "__main__":
    main()