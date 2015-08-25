// borrowed heavily from http://quickleft.com/blog/creating-and-publishing-a-node-js-module
/**
* Valiate a NAICS codes using 2012 NAICS codes
*
* @param  {String} 2 to 6 digit numeric
* @return {Boolean}
*/

var fs = require('fs');
var obj = JSON.parse(fs.readFileSync('codes.json', 'utf8'));

module.exports = {
  validateNAICS: function(inNAICS) {

    inNAICS = inNAICS || "";
    if (inNAICS.length > 6) { return false };
    if (inNAICS.length < 2) { return false };
    if (obj.hasOwnProperty(inNAICS)) { return true };
    return false;
  },

  /**
  * Given a NAICS code return the industry name
  *
  * @param  {String} 2 to 6 digit numeric
  * @return {String}
  */

  translateNAICS: function(inNAICS) {
    inNAICS = inNAICS || "000";

    if (obj.hasOwnProperty(inNAICS)) { return obj[inNAICS] };

    return "Unknown";
  }
};
