#!/usr/bin/python

import argparse

parser = argparse.ArgumentParser()
parser.add_argument('msg', metavar='MSG')
parser.add_argument('-C', '--caps', action='store_true')
parser.add_argument('-X', '--this-is-a-very-long-option',
                    help=("This is some super long help as well - let's see "
                          "well you deal with this!"))

print parser.parse_args()
