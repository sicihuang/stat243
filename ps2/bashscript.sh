wget www.stat.berkeley.edu/share/paciorek/ss13hus.csv.bz2

bzcat ss13hus.csv.bz2 | wc -l > nRows.txt

bzcat ss13hus.csv.bz2 | head -n1 | tr ',' '\n'| wc -l > nCols.txt

bzcat ss13hus.csv.bz2 | head -n1 > colNames.txt

awk -F',' '{
for(i=1; i<=NF; i++){
if ($i == "ST" || $i == "NP"|| $i == "BDSP"|| $i == "BLD"|| $i == "RMSP"|| $i == "TEN"|| $i == "FINCP"|| $i == "FPARC"|| $i == "HHL"|| $i == "NOC"|| $i == "MV"|| $i == "VEH"|| $i == "YBL" ){
print(i)
}
}
}' < colNames.txt >> sampleColNum.txt