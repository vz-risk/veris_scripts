# VERIS Files
This README describes the content and use of the VERIS import directory in the veris_scripts repository.  Ultimately this functionality will likely be merged with the VERIS repository proper.

# Folder layout

 * **/vcdb**: holds the schema, the enumeration file, the plus section and a completely merged json file of the current schema being used to validate incidents for the VCDB
 * **/bin**: Holds import scripts, the validation script, and the configuration file.
 * **/node_modules**: Holds libraries required for nCheckValidity.js.
 
# Commands

**stdexcel.py** converts a csv file from the standard excel format into json files.  By default, it will read configuration settings from the _checkvalidity.cfg config file.  All command line arguments overwrite config file arguments.

    python stdexcel.py --input=~/Documents/Github/veris_scripts/import/VERIS_Standard_Excel_Example.csv --output=~/Documents/Data/VERIS/dump/ --schemafile=../vcdb/veris.json --enumfile=../vcdb/veris-enum.json --countryfile=../all.json  --conf=./_checkValidity.cfg


**nCheckValidity.js** will look at a set of json files and verify that they conform to the VERIS specification and that they have internal consistency. The command can accept a path or list of paths to folders to search for incident son files and a fully merged schema file.  The _checkvalidity.cfg file can specify the paths to look at for json files and can also set a default merged schema file. (the defaults are overridden by options specified at the command line.) The only output is errors in the files which must be manually fixed.  (Note, this requires node.js be installed on the system to run.  It also requires moment and verisnaics which are in the node_modules folder. momentum can be installed using `npm install moment` and verisnaics with `npm install verisnaics`.)

See nCheckValidity.js help:
`node nCheckValidity.js -h`

Run using _checkvalidity.cfg settings
`node nCheckValidity.js`

Run using command line settings:
`node nCheckValidity.js -p /path/to/json/incident/files/ /another/path/to/json/incident/files/ -s /path/to/merged/schema.json