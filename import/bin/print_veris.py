#!/usr/bin/python
from pprint import pprint
import json
import sys

with open(sys.argv[1], 'r') as f:
    pprint(json.load(f))