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

if __name__ == "__main__":
    main()