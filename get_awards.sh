#! /bin/bash

mkdir -p ./data/input/awards

for i in {1959..2017}
do
  curl -o ./data/input/awards/$i.zip "https://www.nsf.gov/awardsearch/download?DownloadFileName=$i&All=true"
done

curl -o /home/simon/Downloads/apoc-3.1.3.7-all.jar

cp /apoc-3.1.3.7-all.jar" 
