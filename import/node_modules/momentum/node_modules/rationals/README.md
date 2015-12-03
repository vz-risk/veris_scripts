# rationals
_extended rational number type for javascript_

Why I think this lib is great:

- Representing fractions is not just easy, but it's natural. This is what
rational numbers do best. Operations like `0.1 + 0.2` are a pain in js,
but using rational numbers, it's just: `r(1,10).plus(1,5) === r(3,10)`

- This lib provides a type of object which will work with 1/0 or 0/0 just as well.
This is especially useful if you plan to plot values on a graph.

I've got the inspiration for this lib from here: [MF105: The extended rational numbers in practice](http://www.youtube.com/watch?v=YMQkLojL2ek)

# Support
[![browser support](https://ci.testling.com/ashnur/rationals.png)](https://ci.testling.com/ashnur/rationals)




# Examples
```
var r = require('rationals');

// denominator of 1 is optional
r(1) === r(1); // true

r(1,1) === r(1); // true

// all rationals will be always reduced
r(2,4) === r(13,26); // true

r(100,50) === r(2); // true

// chaining works
r(1,2).plus(r(1,3)).minus(r(1,4)).times(r(1,5)).per(r(1,6)) === r(49,70); // true

// my personal favorite aproximation of Pi, from ancient china
r(355,113).val(); // 3.1415929203539825

// you can give floats and they will be converted into rationals
rats(0.4,0.1) == rats(4);

```

# API
- _a & b are objects created with the rationals() function_
- in the parentheses you have some common aliases for the methods


#### Addition
- add _(plus)_

    `a.add(b)`

#### Subtraction
- subtract _(minus, sub)_

    `a.sub(b)`

#### Multiplication
- multiply _(times, mul)_

    `a.mul(b)`

#### Division
- divide _(per, div)_

    `a.div(b)`

#### Examining
- toString

    Examining an object can be hard, but if you cast it to a string: `r(355,113)+''` will return `'355/113'`.

    So `r(625,125).toString()` will return `'5/1'`.

#### Display
- display

    Just like toString(), but the numerator will be shown only if it's not 1.
    That is, integers will appear without a slash symbol and a denominator.

    `r(625,125).display()` returns `'5'`.

#### Approximation
- val _(value)_

    Will return the numerator divided with the denominator.

    `r(355,113).val(); //3.1415929203539825`

#### Ordering
- compare _(value)_

    Will return -1, 0 or 1 if the rational is smaller, equal or larger than the rational it is compared to

    `r(-999,605).compare(R(272,835)) // -1`
    `r(-966,743).compare(R(-632,198)) // 1`
    `r(-3,9).compare(R(12,-36)) // 0`

#### Compare absolute values
- compareAbs _(value)_

    Same as compare but without signs.

    `r(-999,605).compareAbs(R(272,835)) // 1`

# Good to know
If you provide anything else as the numerator, than an integer, float or a numerica string an exception will be thrown.
On the other hand, if you do the same with the denominator, it will be cast to 1. This is because
I am lazy, and I do not want to handle wrong values and undefined values differently throwing for the former
and casting to 1 for the latter.

# Install
```
npm install rationals
```

**You can use it in the browser with [browserify](http://browserify.org/)**
