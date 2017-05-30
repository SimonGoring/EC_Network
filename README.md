# EC_Network

This repository is used to build and analyse a large graph database (using neo4j) in R, to examine the impact of EarthCube on investigator networks.  There are two components.  One to build the database, and one to analyze the constructed database.  See the Details below for more information.

## Development

  * Simon Goring - University of Wisconsin - Madison
    * [Website Documents](http://www.goring.org/resources/neo4j_engagement.html)  
  
## Contributors

I welcome contributions from developers, or non-developers.  Please feel free to raise issues or contribute code and text.  Please do so as a Pull Request.

# Details

## Obtaining the Data

The database is built using XML data downloaded from the [National Sciences Foundation](http://nsf.org).  For convenience, a `bash` script, [`get_awards.sh`](https://github.com/SimonGoring/EC_Network/blob/master/get_awards.sh) can be used to download all the required files to a new directory at `./data/input/awards`.  Each individual zipped file represents all awards for a particular year.  To execute the file from the command line simply enter:

```bash
bash ./get_awards.sh
```

This does not require the use of `neo4j` and can be the basis of any kind of database you'd like to use.

## Building the Database

The database can be built by executing the [`build_db_xml.sh`](https://github.com/SimonGoring/EC_Network/blob/master/build_db_xml.sh) bash script.  Additionally, the script to generate the database requires use of the [`apoc` plugin for Neo4j](https://neo4j-contrib.github.io/neo4j-apoc-procedures/).  Be sure to install the plugin before using the scripts to build the database.

The database will be built wherever you have your `neo4j.conf` file set up to find it.  If you wish to put this database somewhere else on your system, simply edit `etc/neo4j/neo4j.conf` to point to the proper location.

Once you have things set up, execute:

```bash
sudo bash build_db_xml.sh
```

This assumes you use `root` privileges to start and stop the `neo4j` database (we re-start the service) and also to manage the database itself.

## Optimizing

There is probably a lot of work that can be done to optimize the core `CQL` file, [`cql_files/xml_direct.cql`](https://github.com/SimonGoring/EC_Network/blob/master/cql_folder/xml_direct.cql), but I'm still learning Cypher.  

Additionally, although this code brings down the processing speed for each file to about 100ms *per* XML file, there are hundreds of thousands of files.  To optimize this operation I used GNU Parallel<a name="gnu"><sup>1</sup></a>.  You can see how the code for `build_db_xml.sh` changed by looking at the [commit history](https://github.com/SimonGoring/EC_Network/commit/e11a17ddefd080f1941af73708cc8fbfb19fab7e).  In particular, we make use of the `parallel` function (install using `apt install parallel` on linux systems) in the following way:

```bash
find ./data/input/awards -name *.xml | parallel --jobs 3 "runxml {}"
```

This was a pretty fun discovery and it seems to have sped things up a bit for me.

<hr>

<sup>[1](#gnu)</sup>O. Tange (2011): <a href="https://www.usenix.org/publications/login/february-2011-volume-36-number-1/gnu-parallel-command-line-power-tool">GNU Parallel - The Command-Line Power Tool</a>. <i>;login: The USENIX Magazine</i>, February 2011:42-47.

data/input/awards/7407911.xml <- Error.