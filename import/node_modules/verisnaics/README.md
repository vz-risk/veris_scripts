# verisNAICS
A very simple module that can validate that a string is a valid North American
Industry Classification Standard code. Also export a function to give the
industry name when a valid NAICS code is given to it.

## Installation

    npm install verisnaics

## Usage

    var vn = require('verisnaics'),
    validate = vn.validateNAICS,
    translate = vn.translateNAICS;

    if (validate("123456")) {
      console.log("123456 is a valid NAICS code")
    } else {
      console.log("123456 is not a valid NAICS code")
    };

    console.log(translate("31-33"));

## Tests

    npm test
