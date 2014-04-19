#!/usr/bin/python
import argparse
 
parser = argparse.ArgumentParser(description='This is a test script by jenny.')
parser.add_argument('-g','--game', help='Game identifier', required=True)
args = parser.parse_args()
 
## show values ##
print ("Game identifier: %s" % args.game )

