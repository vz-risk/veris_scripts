var Viral = require('../viral.js')
var a = require('assert')

var isPrototypeOf = function(child, parent){
	var F = function(){}
	F.prototype = parent
	return child instanceof F
}

'use strict'

describe('Viral', function(){

	describe('.extend', function(){

		it('should make a new object, inheriting from the previous one', function(){
			var Person = Viral.extend()

			a.ok(isPrototypeOf(Person, Viral))
		})

		it('should mix in properties passed in', function(){
			var Person = Viral.extend({
				walks: function(){ return true }
			})

			a.equal(typeof Person.walks, 'function')
			a.equal(Viral.walks, undefined)
		})

	})

	describe('.make', function(){

		it('should make a new instance of a prototype', function(){
			var Person = Viral.extend({})
			var hugh = Person.make()

			a.ok(isPrototypeOf(hugh, Person))
		})

		it('should call init function', function(){
			var Person = Viral.extend({
				init: function(name){ this.name = name; }
			})

			var hugh = Person.make('hugh')

			a.equal(hugh.name, 'hugh')
			a.equal(Person.name, undefined)
			a.ok(isPrototypeOf(hugh, Person))
		})
	})
})
