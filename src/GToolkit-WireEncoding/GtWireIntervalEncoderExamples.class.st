"
Examples for interval wire encoding: original constructor parameters, payload boundaries, empty and descending intervals, and non-integer steps.
"
Class {
	#name : #GtWireIntervalEncoderExamples,
	#superclass : #Object,
	#category : #'GToolkit-WireEncoding-Examples'
}

{ #category : #examples }
GtWireIntervalEncoderExamples >> roundTrips [
	<gtExample>
	| parameters results |
	GtWireEncoderDecoder reset.
	GtWireObjectEncoder validateTypeIdentifiers.
	parameters := #(#(1 10 2) #(10 1 -2) #(5 1 1) #(1 5 -1) #(3 3 1) #(-10 -1 3) #(0 1 0.25)).
	parameters := parameters , { { 1 / 3. 7 / 3. 1 / 2 }. { 1. 10 raisedTo: 30. 3 } }.
	results := parameters collect: [ :triple | | interval encoder decoder decoded |
		interval := Interval from: triple first to: triple second by: triple third.
		encoder := GtWireEncoder onByteArray.
		encoder nextPut: interval; nextPut: #sentinel.
		decoder := GtWireDecoder on: encoder contents readStream.
		decoded := decoder next.
		self assert: decoded class equals: Interval.
		self assert: (self gtDo: [ decoded first ] gemstoneDo: [ decoded _from ]) equals: triple first.
		self assert: (self gtDo: [ decoded stop ] gemstoneDo: [ decoded _to ]) equals: triple second.
		self assert: decoded increment equals: triple third.
		self assert: decoded size equals: interval size.
		self assert: decoder next equals: #sentinel.
		decoded ].
	^ results
]

{ #category : #examples }
GtWireIntervalEncoderExamples >> wireFormat [
	"Exactly three ordinary wire values preserve the original bounds, even when the upper bound is not an element."
	<gtExample>
	| encoder decoder |
	GtWireEncoderDecoder reset.
	encoder := GtWireEncoder onByteArray.
	encoder nextPut: (Interval from: 1 to: 10 by: 2); nextPut: #sentinel.
	self assert: encoder contents first equals: GtWireIntervalEncoder typeIdentifier.
	decoder := GtWireDecoder on: encoder contents allButFirst readStream.
	self assert: decoder next equals: 1.
	self assert: decoder next equals: 10.
	self assert: decoder next equals: 2.
	self assert: decoder next equals: #sentinel.
	^ encoder contents
]
