"
Portable Set and IdentitySet wire-format and round-trip examples.
"
Class {
	#name : #GtWireSetEncoderExamples,
	#superclass : #Object,
	#category : #'GToolkit-WireEncoding-Examples'
}

{ #category : #examples }
GtWireSetEncoderExamples >> duplicateElements [
	"Decode every declared element even when insertion collapses duplicates, leaving the next wire object intact."
	<gtExample>
	| results |
	GtWireEncoderDecoder reset.
	results := { GtWireSetEncoder. GtWireIdentitySetEncoder } collect: [ :encoderClass | | encoder decoder decoded expectedClass |
		expectedClass := encoderClass == GtWireSetEncoder ifTrue: [ Set ] ifFalse: [ IdentitySet ].
		encoder := GtWireEncoder onByteArray.
		encoder putTypeIdentifier: encoderClass typeIdentifier; putSize: 3.
		encoder nextPut: #repeated; nextPut: #repeated; nextPut: #repeated; nextPut: #sentinel.
		decoder := GtWireDecoder on: encoder contents readStream.
		decoded := decoder next.
		self assert: decoded class equals: expectedClass.
		self assert: decoded size equals: 1.
		self assert: (decoded includes: #repeated).
		self assert: decoder next equals: #sentinel.
		decoded ].
	^ results
]

{ #category : #examples }
GtWireSetEncoderExamples >> equalButNotIdentical [
	"Equal strings collapse in Set but remain separate elements in IdentitySet, including after decoding."
	<gtExample>
	| first second results |
	GtWireEncoderDecoder reset.
	first := String with: $a.
	second := String with: $a.
	self assert: first equals: second.
	self assert: first ~~ second.
	results := { Set. IdentitySet } collect: [ :setClass | | set encoder decoder decoded elements expectedSize |
		set := setClass new add: first; add: second; yourself.
		expectedSize := setClass == Set ifTrue: [ 1 ] ifFalse: [ 2 ].
		self assert: set size equals: expectedSize.
		encoder := GtWireEncoder onByteArray.
		encoder nextPut: set; nextPut: #sentinel.
		decoder := GtWireDecoder on: encoder contents readStream.
		decoded := decoder next.
		self assert: decoded class equals: setClass.
		self assert: decoded size equals: expectedSize.
		decoded do: [ :each |
			self assert: each equals: first.
			self assert: (decoded includes: each) ].
		elements := decoded asArray.
		setClass == IdentitySet ifTrue: [
			self assert: elements first ~~ elements second.
			self assert: elements first equals: elements second ].
		self assert: decoder next equals: #sentinel.
		decoded ].
	^ results
]

{ #category : #examples }
GtWireSetEncoderExamples >> nestedSets [
	"Both kinds of set can contain the other kind, reconstructed through their public collection APIs."
	<gtExample>
	| results |
	GtWireEncoderDecoder reset.
	results := { Set. IdentitySet } collect: [ :outerClass | | innerClass inner outer encoder decoder decoded decodedInner |
		innerClass := outerClass == Set ifTrue: [ IdentitySet ] ifFalse: [ Set ].
		inner := innerClass new add: #inner; add: 42; yourself.
		outer := outerClass new add: inner; add: #outer; yourself.
		encoder := GtWireEncoder onByteArray.
		encoder nextPut: outer; nextPut: #sentinel.
		decoder := GtWireDecoder on: encoder contents readStream.
		decoded := decoder next.
		self assert: decoded class equals: outerClass.
		self assert: decoded size equals: 2.
		self assert: (decoded includes: #outer).
		decodedInner := decoded detect: [ :each | each class == innerClass ].
		self assert: decodedInner size equals: 2.
		self assert: (decodedInner includes: #inner).
		self assert: (decodedInner includes: 42).
		self assert: (decoded includes: decodedInner).
		self assert: decoder next equals: #sentinel.
		decoded ].
	^ results
]

{ #category : #examples }
GtWireSetEncoderExamples >> roundTrips [
	<gtExample>
	| results |
	GtWireEncoderDecoder reset.
	GtWireObjectEncoder validateTypeIdentifiers.
	results := { Set. IdentitySet } collect: [ :setClass |
		{ setClass new.
			(setClass new add: #one; yourself).
			(setClass new add: nil; add: true; add: false; add: 42; add: #symbol; add: 'text'; yourself).
			(setClass new addAll: (1 to: 300); yourself) }
			collect: [ :set | | encoder decoder decoded |
				encoder := GtWireEncoder onByteArray.
				encoder nextPut: set; nextPut: #sentinel.
				decoder := GtWireDecoder on: encoder contents readStream.
				decoded := decoder next.
				self assert: decoded class equals: setClass.
				self assert: decoded size equals: set size.
				self assert: (Set withAll: decoded) equals: (Set withAll: set).
				decoded do: [ :each | self assert: (decoded includes: each) ].
				self assert: decoder next equals: #sentinel.
				decoded ] ].
	^ results
]

{ #category : #examples }
GtWireSetEncoderExamples >> wireFormat [
	"The payload is a logical element count followed by ordinary wire objects, not backing storage. Element order is unspecified."
	<gtExample>
	| results |
	GtWireEncoderDecoder reset.
	self assert: GtWireSetEncoder typeIdentifier equals: 11.
	self assert: GtWireIdentitySetEncoder typeIdentifier equals: 36.
	results := { Set. IdentitySet } collect: [ :setClass |
		{ setClass new. (setClass new addAll: #(one two three); yourself) }
			collect: [ :set | | encoder decoder elements typeIdentifier |
				typeIdentifier := setClass == Set
					ifTrue: [ GtWireSetEncoder typeIdentifier ]
					ifFalse: [ GtWireIdentitySetEncoder typeIdentifier ].
				encoder := GtWireEncoder onByteArray.
				encoder nextPut: set; nextPut: #sentinel.
				self assert: encoder contents first equals: typeIdentifier.
				decoder := GtWireDecoder on: encoder contents allButFirst readStream.
				self assert: decoder nextSize equals: set size.
				elements := (1 to: set size) collect: [ :index | decoder next ].
				self assert: elements asSet equals: set asSet.
				self assert: decoder next equals: #sentinel.
				encoder contents ] ].
	^ results
]
