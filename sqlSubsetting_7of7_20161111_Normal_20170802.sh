#!/bin/sh
cd /Volumes/Databases/Stanford_iPOP_Project
file="/Users/Home/Desktop/BasisSubsets/20170609_RefSheet.csv"
sed 1d $file | while read eachLine
do
MD5=$(echo "$eachLine" | awk -F ',' '{print $3;}')
MD5=$(echo "$MD5" | tr -d '"' | tr -d "'")
while echo $MD5 | grep -E ' ' >/dev/null
do
MD5=${MD5/ /"','"}
done
FilePath="/Volumes/Databases/Stanford_iPOP_Project/BasisSubsets/FromBasisData_20161111_Normal_20170802/AllData_Files/"
FileNumber=$(echo "$eachLine" | awk -F ',' '{print $4;}')
FileName=$FilePath$FileNumber
FileName=$(echo "$FileName" | tr -d '"' | tr -d "'")
sqlite3 iPOPwearables.db << EOF
.headers on
.mode csv
.output '$FileName.csv'
SELECT Wearable_Account_MD5,Timestamp_Local,GSR,Heart_Rate,Skin_Temperature_F,Steps FROM BasisData_20161111_Normal_20170802 WHERE Wearable_Account_MD5 IN ('$MD5');
.output stdout
EOF
done