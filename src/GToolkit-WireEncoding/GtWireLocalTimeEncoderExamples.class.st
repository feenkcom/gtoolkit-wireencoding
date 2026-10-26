"
Examples of local-time wire encoding, including payload order, precision conversion, and day boundaries.
"
Class {
	#name : #GtWireLocalTimeEncoderExamples,
	#superclass : #Object,
	#category : #'GToolkit-WireEncoding-Examples'
}

{ #category : #examples }
GtWireLocalTimeEncoderExamples >> decodesDifferentScales [
	"Read both platforms' payloads as well as other decimal scales using exact arithmetic."
	<gtExample>
	| results |
	GtWireEncoderDecoder reset.
	results := { { 0. 0 }. { 125. 3 }. { 123456. 6 }. { 123456789. 9 } } collect: [ :pair |
		| encoder decoder decoded expectedFraction |
		encoder := GtWireEncoder onByteArray.
		encoder
			putTypeIdentifier: GtWireLocalTimeEncoder typeIdentifier;
			putPackedInteger: 45296;
			putPackedInteger: pair first;
			putPackedInteger: pair second;
			putPackedInteger: 77.
		decoder := GtWireDecoder on: encoder contents readStream.
		decoded := decoder next.
		expectedFraction := pair first / (10 raisedTo: pair second).
		self assert: decoded asSeconds equals: 45296.
		self
			gtDo: [ self assert: decoded nanoSecond equals: (expectedFraction * 1000000000) truncated ]
			gemstoneDo: [ self assert: decoded asMicroseconds equals: 45296000000 + (expectedFraction * 1000000) truncated ].
		self assert: decoder nextPackedInteger equals: 77.
		decoded ].
	^ results
]

{ #category : #examples }
GtWireLocalTimeEncoderExamples >> roundTrips [
	<gtExample>
	| times results |
	GtWireEncoderDecoder reset.
	GtWireObjectEncoder validateTypeIdentifiers.
	times := self
		gtDo: [ { Time midnight. Time noon. Time seconds: 45296 nanoSeconds: 123456789. Time seconds: 86399 nanoSeconds: 999999999 } ]
		gemstoneDo: [ { Time fromMicroseconds: 0. Time fromMicroseconds: 43200000000. Time fromMicroseconds: 45296123456. Time fromMicroseconds: 86399999999 } ].
	results := times collect: [ :time | | encoder decoded |
		encoder := GtWireEncoder onByteArray.
		encoder nextPut: time.
		decoded := (GtWireDecoder on: encoder contents readStream) next.
		self assert: decoded equals: time.
		decoded ].
	^ results
]

{ #category : #examples }
GtWireLocalTimeEncoderExamples >> wireFormat [
	"The payload contains exactly three packed integers, followed by a sentinel to verify its boundary."
	<gtExample>
	| encoder decoder time fraction scale |
	GtWireEncoderDecoder reset.
	time := self
		gtDo: [ Time seconds: 45296 nanoSeconds: 123456789 ]
		gemstoneDo: [ Time fromMicroseconds: 45296123456 ].
	fraction := self gtDo: [ 123456789 ] gemstoneDo: [ 123456 ].
	scale := self gtDo: [ 9 ] gemstoneDo: [ 6 ].
	encoder := GtWireEncoder onByteArray.
	encoder nextPut: time; putPackedInteger: 77.
	self assert: encoder contents first equals: GtWireLocalTimeEncoder typeIdentifier.
	decoder := GtWireDecoder on: encoder contents allButFirst readStream.
	self assert: decoder nextPackedInteger equals: 45296.
	self assert: decoder nextPackedInteger equals: fraction.
	self assert: decoder nextPackedInteger equals: scale.
	self assert: decoder nextPackedInteger equals: 77.
	^ encoder contents
]
