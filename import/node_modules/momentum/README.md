# momentum

Basic vector operations over the rational numbers.

# Support
[![browser support](https://ci.testling.com/ashnur/momentum.png)](https://ci.testling.com/ashnur/momentum)

# Examples
where `m.r` is the [rationals](https://github.com/ashnur/rationals/) used by momentum
```
var m = require('momentum');
m([1])[0] === m.r(1); // true
```

while in the case of *rationals* it made sense to keep direct equality between the
created objects, I think this is not the case with momentumi.
However this might change some time in the future.

Another issue is that no matter how much I wish, there isn't a good, widely supported
way to subclass array (not until es6 is widely supported). The workarounds available
kinda create more problems than they solve, so after a day of thinking I went
in the minimalist direction. This also means, that while momentum methods expect
arrays and scalars with add, sub, scale, disperse, dot methods on them, the module's
user should provide these methods.

Probably worth to mention that the API gives you easy solution to this:

```
var m = require('momentum');
var equal = require('deep-equal');
var r = m.r;
equal(m.disperse(m([1,4,7,9,16]), r(12)), m([r(1,12),r(1,3),r(7,12),r(3,4),r(4,3)])); // true
```

# API
all methods on momentum objects  will return an array of rationals
with the exception of `.dot`, which will return a single rational

#### Casting an array of integers to rationals
##### `momentum()`
```
m([3])[0].toString()  // 3/1
```

#### Casting an integer to a rational
##### `momentum.r()`
```
m.r(5).toString()  // 5/1
```

#### Scaling up
##### `scale`
```
m.scale(m([1,3,5]), m.r(7))  // [7/1,21/1,35/1]
```

#### Scaling down
##### `disperse`
```
m.disperse(m([1,3,5]), m.r(9)) // [1/9,1/3,5/9]
```

#### Addition
##### `add`
```
m.add(m([1,3,5]), m([2,4,6])) // [3/1,7/1,11/1]
```

#### Subtraction
##### `sub`
```
m.sub(m([3,7,11]), m([2,4,6])) // [1/1,3/1,5/1]
```

#### Dot product
##### `dot`
```
m.dot(m([1,3,5]), m([7,11,13])) // 105/1
```

#### Cross product
##### `cross`
```
m.cross(m([1,3,5]), m([7,11,13])) // [-16/1, 22/1, -10/1]
```

# Install
```
npm install momentum
```


**You can use it in the browser with [browserify](http://browserify.org/)**

