var R = require('../')
    , expect = require('expect.js')
    , five, negone, one, six, ten, third, thirty, three, two, x, y, zero
    , inf, origo
    ;

describe("R", function() {

    it("should construct a new number object, based on input", function() {
        expect(R(1)).to.be.an('object')
        expect(R(1).val()).to.be(1)
        expect(R.checkInput(R(1))).to.be(R(1))
    })

})

negone = R(-1)
zero = R(0)
one = R(1)
two = R(2)
three = R(3)
five = R(5)
six = R(6)
ten = R(10)
thirty = R(30)
x = R(3, 10)
y = R(3, 14)
third = R(1, 3)


describe('common usage', function(){
    it('recognizes decimal numbers, converts them into fractions', function(){
        expect(R(0.5)).to.be(R(1,2))
        expect(R(0.33)).to.be(R(33,100))
        expect(R(0.4,0.1)).to.be(R(4))
    })
})
describe('elementary arithmetic', function() {
    it('addition', function() {
        expect(one.plus(two)).to.be(three)
        expect(one.plus(negone)).to.be(zero)
        expect(x.plus(y)+'').to.be('18/35')
    })
    it('subtraction', function() {
        expect(one.minus(two)).to.be(negone)
        expect(one.minus(negone)).to.be(two)
        expect(x.minus(y)+'').to.be('3/35')
        expect(two.minus(third)+'').to.be('5/3')
    })
    it('multiplication', function() {
        expect(five.times(six)).to.be(thirty)
        expect(one.times(negone)).to.be(negone)
        expect(x.times(y)+'').to.be('9/140')
    })
    it('division', function() {
        expect(five.per(ten)+'').to.be('1/2')
        expect(one.per(negone)).to.be(negone)
        expect(x.per(y)+'').to.be('7/5')
    })
    it('large number calculations', function(){
        var x = R(1123875)
        var y = R(1238750184)
        var z = R(1657134)
        var r = R(77344464613500, 92063)
        expect(x.times(y).per(z)).to.be(r)
    })

})

inf = R(1,0)
origo = R(0,0)

describe('operations with infinity and the origo → ', function() {

    it('adds to ∞', function(){
        expect(inf.add(x)).to.be(inf)
    })
    it('adds ∞', function(){
        expect(x.add(inf)).to.be(inf)
    })
    it('adds to origo', function(){
        expect(x.add(origo)).to.be(origo)
    })
    it('adds origo', function(){
        expect(origo.add(x)).to.be(origo)
    })

    it('subtracts from ∞', function(){
        expect(inf.sub(x)).to.be(inf)
    })
    it('subtracts ∞', function(){
        expect(x.sub(inf)).to.be(inf)
    })
    it('subtracts from origo', function(){
        expect(x.sub(origo)).to.be(origo)
    })
    it('subtracts origo', function(){
        expect(origo.sub(x)).to.be(origo)
    })

    it('multiplies with ∞', function(){
        expect(inf.mul(x)).to.be(inf)
    })
    it('multiplies ∞', function(){
        expect(x.mul(inf)).to.be(inf)
    })
    it('multiplies with origo', function(){
        expect(x.mul(origo)).to.be(origo)
    })
    it('multiplies origo', function(){
        expect(origo.mul(x)).to.be(origo)
    })

    it('divides with ∞', function(){
        expect(inf.per(x)).to.be(inf)
    })
    it('divides ∞', function(){
        expect(x.div(inf)).to.be(zero)
    })
    it('divides with origo', function(){
        expect(x.div(origo)).to.be(origo)
    })
    it('divides origo', function(){
        expect(origo.div(x)).to.be(origo)
    })

})

describe('compare rational numbers', function() {
    it('returns -1, 0 or 1 if a is smaller, equal or larger than b', function(){
        expect(R(-999,605).compare(R(272,835))).to.be(-1)
        expect(R(-966,743).compare(R(-632,198))).to.be(1)
        expect(R(-3,9).compare(R(12,-36))).to.be(0)
        expect(R(742,-185).compare(R(319,-830))).to.be(-1)
        expect(R(-999,605).compareAbs(R(272,835))).to.be(1)
    })
})
