# VERIS Files
We have a repository on github called verbs and we have a private repo called verism-private. Over the years these two repos have started to get a little smelly. It's not always clear which schema file is the most current one to use and sometimes we go looking in verism-private for scripts that are actually in veris. The purpose of **this** repository is to be the **current** (_not historical_) repo holding our tools and make things a bit more convenient for Kevin.

If it isn't more convenient for you then by all means keep doing things the way you were.

# Folder layout

 * **/vcdb**: holds the schema, the enumeration file, the plus section and a completely merged json file of the current schema being used to validate incidents for the VCDB
 * **/bin**: Holds import scripts, the validation script, and the configuration file.
 * **/node_modules**: Holds libraries required for nCheckValidity.js.
 
# Commands
**nCheckValidity.js** will look at a set of json files and verify that they conform to the VERIS specification and that they have internal consistency. The command can accept a path or list of paths to folders to search for incident son files and a fully merged schema file.  The _checkvalidity.cfg file can specify the paths to look at for json files and can also set a default merged schema file. (the defaults are overridden by options specified at the command line.) The only output is errors in the files which must be manually fixed.  (Note, this requires node.js be installed on the system to run.  It also requires moment and verisnaics which are in the node_modules folder. momentum can be installed using `npm install momentum` and verisnaics with `npm install verisnaics`.)

See nCheckValidity.js help:
`node nCheckValidity.js -h`

Run using _checkvalidity.cfg settings
`node nCheckValidity.js`

Run using command line settings:
`node nCheckValidity.js -p /path/to/json/incident/files/ /another/path/to/json/incident/files/ -s /path/to/merged/schema.json


**sg_to_vcdb1_3.py** converts the VCDB data on SurveyGizmo into json files for the VCDB project. This script is not suitable for converting DBIR partner entries because that form is different and has different field names. So this script is only for VCDB data. To get the source material, log into the SurveyGizmo admin console, go to the VCDB survey. Navigate to reports and select Quick Excel/CSV.

    python sg-to-vcdb1_3.py -l info -o /path/for/output/files /path/to/csv/file
    

# Depreciated
**checkValidity1_3.py** will look at a set of json files and verify that they conform to the VERIS specification and that they have internal consistency. The command can accept a path to a schema file, and enumeration file, and a plus file. Alternatively, all of these can be merged into a single file that can be specified and will override the settings for the other three file types. The _checkvalidity.cfg file can specify the paths to look at for json files and can also set a default schema, enum and plus file (the defaults are overridden by options specified at the command line.)

    # verify the vcdb data (from the bin folder)
    python checkValidity1_3.py -l info -m ../vcdb/merge.json -p /path/to/vcdb/data
    
    # verify the drib data (from the bin folder)
    python checkValidity1_3.py -l info -m ../dbir/merge.json