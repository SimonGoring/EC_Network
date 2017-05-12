# EC_Network

This repository is used to build and analyse a large graph database (using neo4j) in R, to examine the impact of EarthCube on investigator networks.  There are two components.  The first is the construction of the graph database, which is accomplished using the `bash` script `build_db.sh`.  

`build_db.sh` was built itteratively over the course of several trials.  In part I was faced with memory overhead issues that I haven't been able to resolve, hence the need for `for` loops in th `bash` script.

**The bash script will not run from this cloned directory.** That's a TODO, I need to script the downloading of the raw NSF award files.

## Development

  * Simon Goring - University of Wisconsin - Madison
  
## Contributors

I welcome contributions from developers, or non-developers.  Please feel free to raise issues or contribute code.

## TODO