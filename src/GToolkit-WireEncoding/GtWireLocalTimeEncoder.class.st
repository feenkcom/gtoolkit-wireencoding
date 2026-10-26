"
Encodes a local time of day as three packed integers: whole seconds since midnight, fractionalSeconds, and scale. The fractional component is fractionalSeconds / (10 raisedTo: scale). GT uses scale 9 (nanoseconds); GemStone uses scale 6 (microseconds). Decoding converts to the receiving platform's precision, truncating sub-microsecond digits on GemStone. No timezone conversion is performed.
"
Class {
	#name : #GtWireLocalTimeEncoder,
	#superclass : #GtWireObjectEncoder,
	#category : #'GToolkit-WireEncoding'
}

{ #category : #accessing }
GtWireLocalTimeEncoder class >> typeIdentifier [
	^ 32
]

{ #category : #encoding }
GtWireLocalTimeEncoder >> decodeWith: aContext [
	| seconds fractionalSeconds scale fraction |
	seconds := aContext nextPackedInteger.
	fractionalSeconds := aContext nextPackedInteger.
	scale := aContext nextPackedInteger.
	fraction := fractionalSeconds / (10 raisedTo: scale).
	^ self
		gtDo: [ Time seconds: seconds nanoSeconds: (fraction * 1000000000) truncated ]
		gemstoneDo: [ Time fromMicroseconds: (seconds * 1000000 + (fraction * 1000000) truncated) ]
]

{ #category : #encoding }
GtWireLocalTimeEncoder >> encode: aTime with: aContext [
	| seconds fractionalSeconds scale |
	seconds := aTime asSeconds truncated.
	self
		gtDo: [ fractionalSeconds := aTime nanoSecond. scale := 9 ]
		gemstoneDo: [ fractionalSeconds := aTime asMicroseconds \\ 1000000. scale := 6 ].
	aContext
		putTypeIdentifier: self typeIdentifier;
		putPackedInteger: seconds;
		putPackedInteger: fractionalSeconds;
		putPackedInteger: scale
]
