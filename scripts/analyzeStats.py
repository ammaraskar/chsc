#!/usr/bin/env python3
import sys
import subprocess
import os
import json
from pathlib import Path
from dcstats.fieller import Fieller

def main():
    if len(sys.argv) < 2:
        print("Usage: analyzeStats.py <file>")
        sys.exit(1)

    verbose = "--verbose" in sys.argv

    path = Path(sys.argv[1])
    file_name = path.name
    print("[+] Analzying stats for " + file_name)

    with open("statistics/" + file_name + ".compile.json") as f:
        compile_info = json.load(f)

    supercompile_measurements = compile_info[2][0]['reportAnalysis']
    supercompile_number_observations = len(compile_info[2][0]['reportMeasured'])
    compile_measurements = compile_info[2][1]['reportAnalysis']
    compile_number_observations = len(compile_info[2][1]['reportMeasured'])

    supercompile_time = supercompile_measurements['anMean']['estPoint']
    supercompile_time_std = supercompile_measurements['anStdDev']['estPoint']
    compile_time = compile_measurements['anMean']['estPoint']
    compile_time_std = supercompile_measurements['anStdDev']['estPoint']

    # Change in time with supercompilation enabled
    flr = Fieller(
        supercompile_time, compile_time,
        supercompile_time_std, compile_time_std, 
        0, 0.05, 
        compile_number_observations + supercompile_number_observations)

    if verbose:
        print(flr)
        print()
    
    upper_interval = (flr.ratio - flr.clower)
    lower_interval = (flr.cupper - flr.ratio)
    up_down_limit = (upper_interval + lower_interval) / 2
    print("Compile time: {:.1f}% ± {:.2f}".format(flr.ratio * 100, up_down_limit * 100))

    supercomp_file_size = os.path.getsize(str(Path("output") / path.parent / (path.stem + ".hs")))
    comp_file_size = os.path.getsize(str(Path("input") / path.parent / (path.stem + ".hs")))

    file_size_ratio = (supercomp_file_size - comp_file_size) / comp_file_size
    print("Size change: {:.2f}%".format(file_size_ratio * 100))

    with open("statistics/" + file_name + ".memory.json") as f:
        memory = json.load(f)
    memory_ratio = (memory['supercompiled'] - memory['compiled']) / memory['compiled']
    print("Memory change: {:.2f}%".format(memory_ratio * 100))

    with open("statistics/" + file_name + ".supercompiled.json") as f:
        supercomp_run_info = json.load(f)
        supercomp_num_observations = len(supercomp_run_info[2][0]['reportMeasured'])
        supercomp_run_info = supercomp_run_info[2][0]['reportAnalysis']

        supercomp_run_time = supercomp_run_info['anMean']['estPoint']
        supercomp_run_std = supercomp_run_info['anStdDev']['estPoint']

    with open("statistics/" + file_name + ".compiled.json") as f:
        comp_run_info = json.load(f)
        comp_num_observations = len(comp_run_info[2][0]['reportMeasured'])
        comp_run_info = comp_run_info[2][0]['reportAnalysis']

        comp_run_time = comp_run_info['anMean']['estPoint']
        comp_run_std = comp_run_info['anStdDev']['estPoint']
    
    change_run_time = supercomp_run_time - comp_run_time

    flr = Fieller(
        change_run_time, supercomp_run_time,
        supercomp_run_std, comp_run_std, 
        0, 0.05,
        comp_num_observations + supercomp_num_observations)

    if verbose:
        print(flr)
        print()

    upper_interval = (flr.ratio - flr.clower)
    lower_interval = (flr.cupper - flr.ratio)
    up_down_limit = (upper_interval + lower_interval) / 2
    print("Run time: {:.1f}% ± {:.2f}".format(flr.ratio * 100, up_down_limit * 100))

if __name__ == "__main__":
    main()