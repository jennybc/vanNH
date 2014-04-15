import gspread
gc = gspread.login('jenny@stat.ubc.ca','???????')
sh = gc.open("2014-04-12_vanNH-at-pdxST")
worksheet_list = sh.worksheets()
worksheet_list
## I will need to loop over the sheets that actually describe points, e.g. '1', '2', etc.
## Example for the first point
worksheet = sh.worksheet("1")
## getting offense data
worksheet.col_values(1)
## getting defense data
worksheet.col_values(2)
## some identifiers for point-level info
worksheet.col_values(3)
## actual point-level info
worksheet.col_values(4)

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
