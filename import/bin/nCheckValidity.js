'use strict';

//var fs = require('fs');
var fs = require('graceful-fs')
var ini = require('ini');
var path = require('path');
var us = require('underscore');
var validate = require('jsonschema').validate;
var ArgumentParser = require('argparse').ArgumentParser;
var vn = require('verisnaics').validateNAICS;
var moment = require('moment');

// Things I read to help understand this
// http://stackoverflow.com/questions/27004272/javascript-equivalent-to-pythons-dictionary-get?noredirect=1#comment42537987_27004272

// Set up the command line arguments
var parser = new ArgumentParser({
  version: '0.0.1',
  addHelp: true,
  description: "Checks a set of json files to see if they are valid VERIS incidents."
});

// read the configuration file if it is present
//var config = ini.parse(fs.readFileSync('./_checkValidity.cfg', 'utf8'));

parser.addArgument(['-p', '--path'], {help:"list of folders to search for incident files. e.g. /path/to/files/", nargs: '+'})
parser.addArgument(['-s', '--schema'], {help:"Fully-merged schema file to validate incidents."})
parser.addArgument(['-c', '--config'], {help:"The config file to use.", 'defaultValue':null})
var args = parser.parseArgs();

if (args.config !== null) {
  // read the configuration file if it is present
  var config = ini.parse(fs.readFileSync(args.config, 'utf8'));
}

// options specified at the command line trump whatever is in the
// configuration file
var schemaFile = (args.schema || config.NODE.schema);
var pathList = (args.path || config.NODE.path);
var schema = JSON.parse(fs.readFileSync(schemaFile,'utf8'));


var invalidSourceIDs = ["na", "none", "null", "undefined"]; // needs to be sorted lexically
// function to check source ID
var checkSourceID = function(filename, inIncident) {
    var sourceID = inIncident.source_id;
    if(!sourceID || us.indexOf(invalidSourceIDs,sourceID.toLowerCase(),true) != -1) {
        console.log('Error in ' + filename + ": has an invalid source_id.");
    }
}

// function to check the NAICS codes
var checkNAICS = function(filename, inIncident) {
  var naics = ((inIncident.victim || {}).industry || '000');
  var partner = (((inIncident.actor || {}).partner || {}).industry || '000');
  if (!vn(naics)) {
    console.log('Error in ' + filename + ": has an invalid victim naics code.");
  }
  if (!vn(partner)) {
    console.log('Error in ' + filename + ": has an invalid partner naics code.")
  }

}

// functions to check internal consistency of incidents
var checkMalwareIntegrity = function(filename, inIncident) {
  if (inIncident.action.hasOwnProperty('malware') && (((inIncident.attribute || {})
                                                .integrity || {})
                                                .variety || []).indexOf('Software installation') == -1) {
    console.log('Error in ' + filename + ": " + "Malware present, but no Software installation in attribute.integrity.variety.");
  }
}

var checkSocialIntegrity = function(filename, inIncident) {
  if (inIncident.action.hasOwnProperty('social') && (((inIncident.attribute || {})
                                                        .integrity || {})
                                                        .variety || []).indexOf('Alter behavior') == -1) {
    console.log('Error in ' + filename + ": " + "acton.social present, but Alter behavior not in attribute.integrity.variety.")
  }
}

var checkSQLiRepurpose = function(filename, inIncident) {
  if ( (((inIncident.action || {}).hacking || {}).variety || []).indexOf('SQLi') > -1) {
    if ( (((inIncident.attribute || {}).integrity || {}).variety || []).indexOf('Repurpose') == -1) {
      console.log('Error in ' + filename + ": " + "action.hacking.SQLi present but Repurpose not in attribute.integrity.variety.")
    }
  }
}

var checkSecurityIncident = function(filename, inIncident) {
  if ((inIncident.security_incident || '') == 'Confirmed') {
    if (!inIncident.hasOwnProperty('attribute')) {
      console.log('Error in ' + filename + ": " + "security_incident Confirmed but attribute section not present.")
    }
  }
}

var checkLossTheftAvailability = function(filename, inIncident) {
  var expectLoss = false;
  if( (((inIncident.action || {}).physical || {}).variety || []).indexOf('Theft') > -1) {
    expectLoss = true;
  }
  if( (((inIncident.action || {}).error || {}).variety || []).indexOf('Loss') > -1) {
    expectLoss = true;
  }
  if (expectLoss) {
    if ( (((inIncident.attribute || {}).availability || {}).variety || []).indexOf('Loss') == -1) {
      console.log('Error in ' + filename + ": " + "action.physical.theft or action.error.loss present but attribute.availability.loss not present.")
    }
  }
}

var checkWebVectorAsset = function(filename, inIncident) {
  if ( (((inIncident.action || {}).hacking || {}).vector || []).indexOf('Web application') > -1) {
    var noWebAsset = true;
    ((inIncident.asset || {}).assets || []).forEach(function(asset) {
      if (asset.variety == "S - Web application") {
        noWebAsset = false;
      }
    })
    if (noWebAsset) {
      console.log('Error in ' + filename + ": " + "action.hacking.vector.Web application present but asset.assets does not include S - Web application.");
    }
  }
}

var checkPlusAttributeConsistency = function(filename, inIncident) {
  if( ((inIncident.plus || {}).attribute || {}).hasOwnProperty('confidentiality')) {
    if( !((inIncident.attribute || {}).hasOwnProperty('confidentiality'))) {
      console.log('Error in ' + filename + ": " + "plus.attribute.confidentiality present but confidentiality is not an affected attribute.")
    }
  }
}

var checkIncidentDate = function(filename, inIncident) {
  // I had to check the month and day because the format
  // will trunctate what is passed in. So a date of
  // 2014-09-2013 will get turned into 2014-09-20
  // which is a valid date.

  var year = inIncident.timeline.incident.year
  var month = (inIncident.timeline.incident.month || 1)
  var day = (inIncident.timeline.incident.day || 1)
  var fullDate = year + "-" + month + "-" + day

  var validDate = true;
  if (month > 12) { validDate = false}
  if (month < 1)  { validDate = false}
  if (day > 31)   { validDate = false}
  if (day < 1)    { validDate = false}

  if(!moment(fullDate, "YYYY-MM-DD").isValid()) {
    validDate = false;
  }
  if(!validDate){
    console.log('Error in ' + filename + ": " + "timeline.incident is not a valid date: " + fullDate + ".")
  }
}

// Loop through every path provided at the command line
pathList.forEach(function(directory) {
  //console.log(directory)
  if (fs.lstatSync(directory).isDirectory()) {
    fs.readdir(directory, function(error, list) {
        if (error) {
          throw error;
        }

    // For every file in the list
      list.forEach(function (file) {
        if (path.extname(file) != ".json") {
          return;
        }
        var fullFile = directory + file;
        //console.log(fullFile)
        fs.readFile(fullFile, 'utf8', function(error, data) {
          if (error) {
            throw error;
          }
          var obj = JSON.parse(data);
          var results = validate(obj, schema);
          checkSourceID(fullFile, obj);
          checkMalwareIntegrity(fullFile, obj);
          checkSocialIntegrity(fullFile, obj);
          checkSQLiRepurpose(fullFile, obj);
          checkSecurityIncident(fullFile, obj);
          checkLossTheftAvailability(fullFile, obj);
          checkPlusAttributeConsistency(fullFile, obj);
          checkNAICS(fullFile, obj);
          checkIncidentDate(fullFile, obj);
          checkWebVectorAsset(fullFile, obj);
          results.errors.forEach(function (message) {
            console.log('Error in ' + fullFile + ": " + message.stack);
          })
        })
      })
    })
  } else if (fs.lstatSync(directory).isFile()) {
    fs.readFile(directory, 'utf8', function(error, data) {
      if (error) {
        throw error;
      }

      var outJSON = JSON.parse(data);
      for (var fullFile in outJSON) {
        if (outJSON.hasOwnProperty(fullFile)) {
          var obj = outJSON[fullFile];
          var results = validate(obj, schema);
          checkSourceID(fullFile, obj);
          checkMalwareIntegrity(fullFile, obj);
          checkSocialIntegrity(fullFile, obj);
          checkSQLiRepurpose(fullFile, obj);
          checkSecurityIncident(fullFile, obj);
          checkLossTheftAvailability(fullFile, obj);
          checkPlusAttributeConsistency(fullFile, obj);
          checkNAICS(fullFile, obj);
          checkIncidentDate(fullFile, obj);
          checkWebVectorAsset(fullFile, obj);
          results.errors.forEach(function (message) {
            console.log('Error in ' + fullFile + ": " + message.stack);
          })
        }
      }
    })
  }
});
