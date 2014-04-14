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
