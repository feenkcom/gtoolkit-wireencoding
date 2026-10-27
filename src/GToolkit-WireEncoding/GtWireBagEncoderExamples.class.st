"
Portable bag round trips and object/count wire-format examples.
"
Class {
	#name : #GtWireBagEncoderExamples,
	#superclass : #Object,
	#category : #'GToolkit-WireEncoding-Examples'
}

{ #category : #examples }
GtWireBagEncoderExamples >> equalButNotIdentical [
	<gtExample>
	| first second results |
	GtWireEncoderDecoder reset.
	first := String with: $a.
	second := String with: $a.
	self assert: first equals: second.
	self assert: first ~~ second.
	results := { Bag. IdentityBag } collect: [ :bagClass | | bag encoder decoder decoded counts |
		bag := bagClass new add: first withOccurrences: 2; add: second withOccurrences: 3; yourself.
		encoder := GtWireEncoder onByteArray.
		encoder nextPut: bag.
		decoder := GtWireDecoder on: encoder contents readStream.
		decoded := decoder next.
		counts := (bagClass == Bag ifTrue: [ GtWireBagEncoder new ] ifFalse: [ GtWireIdentityBagEncoder new ]) countsFor: decoded.
		self assert: decoded size equals: 5.
		self assert: counts size equals: (bagClass == Bag ifTrue: [ 1 ] ifFalse: [ 2 ]).
		self assert: counts values asArray sorted equals: (bagClass == Bag ifTrue: [ #(5) ] ifFalse: [ #(2 3) ]).
		decoded ].
	^ results
]

{ #category : #examples }
GtWireBagEncoderExamples >> invalidCounts [
	<gtExample>
	GtWireEncoderDecoder reset.
	{ GtWireBagEncoder new. GtWireIdentityBagEncoder new } do: [ :wireEncoder |
		{ 0. -1. 1.5. #invalid } do: [ :count | | encoder decoder rejected |
			encoder := GtWireEncoder onByteArray.
			encoder putTypeIdentifier: wireEncoder typeIdentifier; putSize: 1; nextPut: #object; nextPut: count.
			decoder := GtWireDecoder on: encoder contents readStream.
			rejected := [ decoder next. false ] on: Error do: [ :error | error messageText = 'Bag occurrence count must be a positive integer' ].
			self assert: rejected ] ].
	^ self
]

{ #category : #examples }
GtWireBagEncoderExamples >> nestedBags [
	<gtExample>
	| inner outer encoder decoder decoded |
	GtWireEncoderDecoder reset.
	inner := Bag new add: #inner withOccurrences: 4; yourself.
	outer := IdentityBag new add: inner withOccurrences: 3; yourself.
	encoder := GtWireEncoder onByteArray.
	encoder nextPut: outer; nextPut: #sentinel.
	decoder := GtWireDecoder on: encoder contents readStream.
	decoded := decoder next.
	self assert: decoded class equals: IdentityBag.
	self assert: decoded size equals: 3.
	decoded do: [ :bag |
		self assert: bag class equals: Bag.
		self assert: (bag occurrencesOf: #inner) equals: 4.
		self assert: (decoded occurrencesOf: bag) equals: 3 ].
	self assert: decoder next equals: #sentinel.
	^ decoded
]

{ #category : #examples }
GtWireBagEncoderExamples >> roundTrips [
	<gtExample>
	| results |
	GtWireEncoderDecoder reset.
	GtWireObjectEncoder validateTypeIdentifiers.
	results := { Bag. IdentityBag } collect: [ :bagClass |
		{ bagClass new. (bagClass new add: #one withOccurrences: 3; add: #two withOccurrences: 7; yourself). (bagClass new add: #many withOccurrences: 1000000; yourself) }
			collect: [ :bag | | encoder decoder decoded |
				encoder := GtWireEncoder onByteArray.
				encoder nextPut: bag; nextPut: #sentinel.
				decoder := GtWireDecoder on: encoder contents readStream.
				decoded := decoder next.
				self assert: decoded class equals: bagClass.
				self assert: decoded size equals: bag size.
				bag do: [ :object | self assert: (decoded occurrencesOf: object) equals: (bag occurrencesOf: object) ].
				self assert: decoder next equals: #sentinel.
				decoded ] ].
	^ results
]

{ #category : #examples }
GtWireBagEncoderExamples >> wireFormat [
	<gtExample>
	| results |
	GtWireEncoderDecoder reset.
	results := { Bag. IdentityBag } collect: [ :bagClass | | encoder decoder bag wireEncoder |
		bag := bagClass new add: #repeated withOccurrences: 1000000; yourself.
		wireEncoder := bagClass == Bag ifTrue: [ GtWireBagEncoder new ] ifFalse: [ GtWireIdentityBagEncoder new ].
		encoder := GtWireEncoder onByteArray.
		encoder nextPut: bag; nextPut: #sentinel.
		self assert: encoder contents first equals: wireEncoder typeIdentifier.
		decoder := GtWireDecoder on: encoder contents allButFirst readStream.
		self assert: decoder nextSize equals: 1.
		self assert: decoder next equals: #repeated.
		self assert: decoder next equals: 1000000.
		self assert: decoder next equals: #sentinel.
		encoder contents ].
	^ results
]
