#! /bin/bash

# This is a function to let us run things in parallel:
runxml () 
{
	runit=1
	while true; do
		echo "WITH 'file://$PWD/$1' AS url" >> $1_cql.cql
		cat cql_folder/xml_direct.cql >> $1_cql.cql

		# Runs the award loading - just award nodes.
		neo4j-shell -host 127.0.0.1 -port 1337 -name shell -file $1_cql.cql && break
	done
	
	echo "Finished importing $1."

	rm $1_cql.cql
}

export -f runxml

set -e

ulimit -n 40000

neo4j stop
neo4j start

echo "Sleeping for 10 seconds..."
sleep 10s

# Clean up the DB and then set the constraints (only happens once):
neo4j-shell -host 127.0.0.1 -port 1337 -name shell -file cql_folder/clear.cql -v 

echo Finished clearing the old db.

neo4j-shell -host 127.0.0.1 -port 1337 -name shell -file cql_folder/set_constraints.cql -v

echo Finished setting constraints.

# wc -l /var/lib/neo4j/import/award_file.csv 
# There are 1221151 lines in the file (including the header)

XMLFILES=data/input/awards/*.zip

# This is a test with XML files instead of CSV files:
for i in $XMLFILES
	do
		echo *****************************************************
		echo "Starting to run $i"
		echo *****************************************************

		# Now we unzip the zip files into the awards directory.

		unzip $i -d data/input/awards/

		AWARDS=data/input/awards/*.xml
		
		echo "Now we're about to loop inside."

		find ./data/input/awards -name *.xml | parallel --jobs 3 "runxml {}"

		rm $AWARDS
	done

neo4j-shell -host 127.0.0.1 -port 1337 -name shell -file cql_folder/cleaning.cql
