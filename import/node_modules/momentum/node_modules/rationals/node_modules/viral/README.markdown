# Viral

Viral is a tiny, pure prototypal OO library for javascript; taking the best parts of [boo](https://github.com/killdream/boo).

[![browser support](https://ci.testling.com/hughfdjackson/viral.png)](https://ci.testling.com/hughfdjackson/viral)

## Why

The most consistent, easiest way to OO in javascript is pure prototypally - and Viral makes this a snap (and packs a tiny punch in the process - < 0.5kb minified).

## API

Viral is a simple base object with two methods:

### .extend

.extend creates an object that inherits from the object on which it's called, and copies any
properties passed to .extend into that new object:

```javascript
var Person = Viral.extend({
	init: function(firstName, lastName){
		this.firstName = firstName
		this.lastName = lastName
	},
	fullName: function(){ return this.firstName + this.lastName }
})

// extend is inherited by Person, so we can extend further:
var Coder = Person.extend({
	likesCode: true
})
```

Because Viral uses pure prototypal inheritance - objects inheriting directly from objects - Coder and Person are just objects like any other:

```javascript
console.log(Person)

//  {
//  	init: [Function],
//  	fullName: [Function]
//  }

console.log(Coder)

//  {
//  	init: [Function],
//  	fullName: [Function],
//  	likesCode: true
//  }
```

### .make

.make creates an object that inherits from the object on which it's called, and calls the init method
of this new object with any arguments you pass in.


```javascript
// using Coder from the above example:
var hugh = Coder.make('hugh', 'jackson')

hugh.fullName() //= 'hugh jackson'
hugh.likesCode  //= true
```

## Install

### node

`npm install viral`, then require:

```javascript
var Viral = require('viral')

// use `Viral` here
```

### browser

include as a script tag:

```html
<!doctype html>
<html>
	<head></head>
	<body>
		<script src="libs/viral.min.js"></script>
		<script>
		// use `Viral` here
		</script>
	</body>
</html>
```

### requirejs

include as a script.  e.g., from the libs/ folder:

```javascript
require(['libs/viral.min'], function(Viral){
	// use `Viral` here
})
```
