#!/usr/bin/python

import argparse

parser = argparse.ArgumentParser()
parser.add_argument('msg', help="message to print", metavar='MSG')
parser.add_argument('-C', '--caps', help="print message in caps")
parser.add_argument('-X', metavar='MYSTERY',
                    help="the X factor - no one really knows "
                         "what this option is for")
print parser.parse_args()
