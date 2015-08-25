var should = require('chai').should(),
  veris = require('../index'),
  validateNAICS = veris.validateNAICS;
  translateNAICS = veris.translateNAICS;

describe('#validateNAICS', function() {
  it('returns false if code too short', function() {
    validateNAICS('1').should.equal(false);
  });

  it('returns false if code too long', function() {
    validateNAICS('1234567').should.equal(false);
  });

  it('returns false if no value is passed to function', function() {
    validateNAICS().should.equal(false);
  });

  it('returns false if an invalid codes is passed to function', function() {
    validateNAICS('12345').should.equal(false);
  });

  it('returns true if a valid code is passed to function', function() {
    validateNAICS('3114').should.equal(true);
  });

  it('handles zero codes properly', function() {
    validateNAICS('00').should.equal(true);
    validateNAICS('000').should.equal(true);
    validateNAICS('0000').should.equal(true);
    validateNAICS('00000').should.equal(true);
    validateNAICS('000000').should.equal(true);
  });

  it('treats integers like strings', function() {
    validateNAICS(3113).should.equal(true);
  });
});


describe('#translateNAICS', function () {
  it('converts an unknown naics to to Unknown', function() {
    translateNAICS("0").should.equal("Unknown");
  });

  it('converts an empty naics to Unknown', function() {
    translateNAICS().should.equal("Unknown");
  });

  it('understands 2-digit naics codes', function() {
    translateNAICS('11').should.equal('Agriculture, Forestry, Fishing and Hunting');
  });

  it('understands 3-digit naics codes', function() {
    translateNAICS('111').should.equal('Crop Production');
  });

  it('understands 4-digit naics codes', function() {
    translateNAICS('1111').should.equal('Oilseed and Grain Farming');
  });

  it('understands 5-digit naics codes', function() {
    translateNAICS('11111').should.equal('Soybean Farming');
  });

  it('understands 6-digit naics codes', function() {
    translateNAICS('111110').should.equal('Soybean Farming');
  });

  it('understands naics codes with dashes', function() {
    translateNAICS('31-33').should.equal('Manufacturing');
  });

  it('treats integers like strings', function() {
    translateNAICS(11111).should.equal('Soybean Farming');
  })
});
