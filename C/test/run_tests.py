#!/usr/bin/python3

import os
import subprocess
import re

f_null = open(os.devnull, 'w')
root = './test_bin/'
green = '\033[92m'
red = '\033[91m'
template = '{FILE:20}{COL}{RES:4}\033[0m'

print('TESTING')
for f in os.listdir(root):
    out = None
    try:
        out = subprocess.check_output(os.path.join(root, f), stderr = f_null).decode('ascii')
    except subprocess.CalledProcessError:
        print(template.format(FILE=f, RES='ERROR', COL=red))
        continue
    out = re.sub('.\b', '', out) # Remove backspaces
    with open('./' + f + '_correct.txt') as expected_out_f:
        expected_out = expected_out_f.read()

        if out == expected_out:
            print(template.format(FILE=f, RES='OK', COL=green))
        else:
            print(template.format(FILE=f, RES='ERROR', COL=red))
