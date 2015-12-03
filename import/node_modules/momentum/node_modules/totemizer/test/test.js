var expect = require('expect.js')
    , u = require('../')
    ;

describe('liberate', function(){
    it('binds bind to function', function(){
        var slice = u.liberate([].slice);
        expect(slice([1, 2], 1)[0]).to.be(2);
    })
})

describe('detach', function(){
    it('same as liberate, but arguments are given in an array', function(){
        var slice = u.detach([].slice);
        expect(slice([1, 2, 3, 4], [1,3])).to.eql([2,3]);
    })
})

describe('slice', function(){
    it('slices into array ', function(){
        expect(u.slice([1, 2], 1)[0]).to.be(2);
    })
})

describe('times', function(){
    it('is the child of for & map, loops and returns an array', function(){
        expect(u.times(7, function(count){return count})).to.eql([0,1,2,3,4,5,6])
    })
})

describe('longest', function(){
    it('returns longest array', function(){
        expect(u.longest([1],[],[2,3],[[4,4,5,6]],[7,8,9])).to.eql([7,8,9])
    })
})

describe('span', function(){
    it('creates array between values', function(){
        expect(u.span(3, -3, function(x){return x-1})).to.eql([3,2,1,0,-1,-2,-3])
    })
})

describe('zipWith', function(){
    it('applies function simultaneously on multiple arrays', function(){
        expect(u.zipWith(function(){return u.slice(arguments).reduce(function(p,c){return p-c})}
                , [ 1, 2, 3]
                , [-9,-8,-7]
                , [ 5,10]).toString()).to.be([5,0,NaN].toString())
    })
})

describe('zipWithArray', function(){
    it('applies function simultaneously on multiple arrays, given in a single array', function(){
        expect(u.zipWithArray(function(){return u.slice(arguments).reduce(function(p,c){return p-c})}
                , [[ 1, 2, 3]
                , [-9,-8,-7]
                , [ 5,10]]).toString()).to.be([5,0,NaN].toString())
    })
})

describe('zip', function(){
    it('creates new array from multiple arrays', function(){
        expect(u.zip([1,2,3],[-9,-8,-7],[5,10])).to.eql([[1,-9,5],[2,-8,10],[3,-7,undefined]])
    })
})

describe('partition', function(){
    it('splits a long array into pieces', function(){
        expect(u.partition([1,2,3,4,5,6,7],2)).to.eql([[1,2],[3,4],[5,6],[7]])
    })
})

describe('arrayMax', function(){
    it('returns largest value from an array array', function(){
        expect(u.arrayMax([4,1,4,1,45,21])).to.be(45)
    })
})

describe('indexOfMax', function(){
    it('returns largest value from an array array', function(){
        expect(u.indexOfMax([4,1,4,1,45,21])).to.be(4)
    })
})

describe('indexOfAbsMax', function(){
    it('returns largest value from an array array', function(){
        expect(u.indexOfAbsMax([4,-1,1,-45,21])).to.be(3)
    })
})

describe('isInt', function(){
    it('returns true if it\'s integer ', function(){
        expect(u.isInt(1)).to.be(true)
    })
    it('returns false if it\'s integer ', function(){
        expect(u.isInt(0.3)).to.be(false)
    })
})
describe('allIntegers', function(){
    it('returns true if all values are integers', function(){
        expect(u.allIntegers([3,1,0])).to.be(true)
    })
    it('returns false if not all values are integers', function(){
        expect(u.allIntegers([3,1, 1.5,0])).to.be(false)
    })
})

describe('bind', function(){
    it('partially applies arguments to a function', function(){
        var t = u.bind(function(a, b, c){ return a + b + c }, 1, 1);
        expect(t(2)).to.be(4)
    })
})

describe('arrayWithLength', function(){
    it('creates new array with specified length', function(){
        var arr = u.arrayWithLength(5);
        expect(arr.join('x')).to.be('xxxx')
        expect(arr.map(function(){return 'x'}).join('')).to.be('xxxxx')
    })
})
