#!/Users/jenny/anaconda/bin/python

import gspread
import argparse, sys

parser = argparse.ArgumentParser(description='This is a script by jenny.')
parser.add_argument('-g','--game', help='Game identifier', required=True)
parser.add_argument('-p','--point', help='Point', type=int, default = 0)
args = parser.parse_args()

## advertise resolved command line args
print ("Game identifier: %s" % args.game )
if args.point:
    print ("Point: %d" % args.point )

## get ready to talk to the Google
f = open('google-credentials.txt', 'rb')
username = f.readline()
username = username.rstrip()
password = f.readline()
password = password.rstrip()

## access the Google doc
gc = gspread.login(username, password)
sh = gc.open(args.game)
worksheet_list = sh.worksheets()

## determine which point(s) to process
num_worksheets = len(worksheet_list)
print ("Worksheets found : 1 + %s " % (num_worksheets - 1) )
if args.point == 0:
    points_to_process = range(num_worksheets)
else:
    points_to_process = [args.point - 1]

#for sheet_num in range(num_worksheets):
#for sheet_num in range(5):
for sheet_num in points_to_process:

    # get naming correct. Also, Python indexes by zero!
    if sheet_num < 9:
        file_num = "0" + str(sheet_num+1)
    else:
        file_num = str(sheet_num+1)

    try:
        ## Loop over the sheets that actually describe points
        worksheet = sh.worksheet(str(sheet_num+1))
    except gspread.exceptions.WorksheetNotFound:
        break

    ## some identifiers for point-level info ... currently not used
    #point_level_identifier = worksheet.col_values(3)
    ## actual point-level info
    point_level_info = worksheet.col_values(4)

    team_name, period, clock_before, clock_after = point_level_info[0:4]

    if team_name is None:
        print ("team_name is None ... quitting")
        break

    print (file_num)
    sys.stdout.flush()

    ## getting offense data
    offensive = worksheet.col_values(1)
    ## getting defense data
    defensive = worksheet.col_values(2)

    #make both lists of the same length, for zip to work later
    lendiff = len(offensive)-len(defensive)
    if lendiff > 0:
        for i in range(abs(lendiff)):
            defensive.extend([""])
    elif lendiff < 0:
        for i in range(abs(lendiff)):
            offensive.extend([""])

    out_file = file("../games/" + args.game + "/rawGoogleExtract/" + args.game + "_point" + file_num + ".txt",'w')
    out_file.write("Pulling team: " + team_name + "\n")
    out_file.write("Period: " + period + "\n")
    out_file.write("Clock before point: " + clock_before + "\n")
    out_file.write("Clock after point: " + clock_after + "\n")
    out_file.write("Offense" + "\t" + "Defense" + "\n")
    for offense, defense in zip(offensive[2:], defensive[2:]): #skip 2 header lines
        if offense is None:
            offense = ""
        if defense is None:
            defense = ""
        out_file.write(offense + "\t" + defense + "\n")
    out_file.close()
