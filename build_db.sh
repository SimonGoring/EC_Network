#! /bin/bash
set -e

ulimit -n 40000

neo4j stop
neo4j start

sleep 10s

# Clean up the DB and then set the constraints (only happens once):
neo4j-shell -host 127.0.0.1 -port 1337 -name shell -file cql_folder/clear.cql -v 

echo Finished clearing the old db.

neo4j-shell -host 127.0.0.1 -port 1337 -name shell -file cql_folder/set_constraints.cql -v

echo Finished setting constraints.

# wc -l /var/lib/neo4j/import/award_file.csv 
# There are 1221151 lines in the file (including the header)

# I was running into memory errors, I'm going to try cutting the award file into smaller chunks:
for i in {1..20}
	do
		echo ######################################################		
		echo Starting run $i

		START=$((($i-1)*50000+1))
		END=$(($i*50000))

		# Pass in the start and ending lines, print the first line, exit after the `end`
		# and start running from the first line in the start.
		if [ $START -eq 1 ]; then
			awk -v start=$START -v end=$END 'NR > end { exit } NR >= start' data/output/award_file.csv > /var/lib/neo4j/import/award_file.csv
		else
			awk -v start=$START -v end=$END 'NR==1 {print $0} NR > end { exit } NR >= start' data/output/award_file.csv > /var/lib/neo4j/import/award_file.csv
		fi
		echo Finished file copy.

		# Clean escaped quotation marks that make the script fail.		
		sed -i 's/\\"/"/g' /var/lib/neo4j/import/award_file.csv
		echo Finished file cleaning.

		# Runs the award loading - just award nodes.
		neo4j-shell -host 127.0.0.1 -port 1337 -name shell -file cql_folder/load_awards.cql -v
		echo Finished loading all the awards.

		# Creates spatial nodes (country, state, institution) and links them to each other.
		# (country)--(state)--(institution)--(award)
		neo4j-shell -host 127.0.0.1 -port 1337 -name shell -file cql_folder/load_spatial.cql -v
		echo Finished loading all the spatial information.

		neo4j-shell -host 127.0.0.1 -port 1337 -name shell -file cql_folder/load_temporal.cql -v
		echo Finished loading temporal information.

		neo4j-shell -host 127.0.0.1 -port 1337 -name shell -file cql_folder/load_people.cql -v
		echo Finished loading person information.

		neo4j-shell -host 127.0.0.1 -port 1337 -name shell -file cql_folder/load_directorates.cql -v
		echo Finished loading directorate information.
	done
