#!/usr/bin/python

import sys, getopt

# set up the game, e.g. 2014-04-12_vanNH-at-pdxST
game = ''

total = len(sys.argv)
cmdargs = str(sys.argv)
print ("The total numbers of args passed to the script: %d " % total)
print ("Args list: %s " % cmdargs)
print ("Script name: %s" % str(sys.argv[0]))
for i in xrange(total):
    print ("Argument # %d : %s" % (i, str(sys.argv[i])))

# Read command line args
myopts, args = getopt.getopt(sys.argv[1:],"g:")
 
###############################
# o == option
# a == argument passed to the o
###############################
for o, a in myopts:
    if o == '-g':
        game = a
    else:
        print("Usage: %s -i input -o output" % sys.argv[0])
 
# Display input and output file name passed as the args
print ("Game : %s" % (game) )
