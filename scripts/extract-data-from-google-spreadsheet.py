#!/Users/jenny/anaconda/bin/python

import gspread
import argparse

parser = argparse.ArgumentParser(description='This is a script by jenny.')
parser.add_argument('-g','--game', help='Game identifier', required=True)
args = parser.parse_args()
 
## show values ##
print ("Game identifier: %s" % args.game )

gc = gspread.login('jenny@stat.ubc.ca','???')
sh = gc.open(args.game)
worksheet_list = sh.worksheets()

num_spreadsheets = len(worksheet_list)

#for sheet_num in range(num_spreadsheets):
for sheet_num in range(5):

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

    ## some identifiers for point-level info
    point_level_indentifier = worksheet.col_values(3)
    ## actual point-level info
    point_level_info = worksheet.col_values(4)

    team_name, period, clock_before, clock_after = point_level_info[0:4]

    if team_name is None:
        continue

    out_file = file("../games/" + args.game + "/" + args.game + "_point" + file_num + ".txt",'w')
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
