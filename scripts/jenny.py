import gspread
gc = gspread.login('bernhard.konrad@gmail.com','???????')

pre_name = "2014-04-12_vanNH-at-pdxST"
sh = gc.open(pre_name)
worksheet_list = sh.worksheets()

num_spreadsheets = len(worksheet_list)

for sheet_num in range(num_spreadsheets):
#for sheet_num in range(5):

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

    out_file = file(pre_name + "_point" + file_num + ".txt",'w')
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
## I would like to loop over the sheets = points and write one file per point
## naming scheme: 2014-04-12_vanNH-at-pdxST_point01.txt
## ideally the Python script would take Google spreadsheet name as argument
## and use it programmatically from there on out

## point-level info stored in first few lines, like so:
## Pulling team: Vancouver Nighthawks
## Period: 1
## Clock before point: 10:00:00
## Clock after point: 9:23:00

## then the rest of file would give the offense and defense data in tab delimited form, just like the spreadsheet
## Offense	Defense
## 	8P
## 7	
## 24	
## 13	
## 7	
## 24	81D
## 	18
## 	45
## 	31
## 	8
## 	81LG
