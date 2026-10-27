"
Encodes an Interval as its original from:, to:, and by: parameters, in that order, using three wire objects. Preserves empty intervals and bounds that are not elements of the interval. Uses platform-specific accessors on GT and GemStone; decoding uses the common Interval from:to:by: constructor.
"
Class {
	#name : #GtWireIntervalEncoder,
	#superclass : #GtWireObjectEncoder,
	#category : #'GToolkit-WireEncoding'
}

{ #category : #accessing }
GtWireIntervalEncoder class >> typeIdentifier [
	^ 33
]

{ #category : #encoding }
GtWireIntervalEncoder >> decodeWith: aGtWireEncoderContext [
	^ Interval
		from: aGtWireEncoderContext next
		to: aGtWireEncoderContext next
		by: aGtWireEncoderContext next
]

{ #category : #encoding }
GtWireIntervalEncoder >> encode: anInterval with: aGtWireEncoderContext [
	| start stop |
	start := self gtDo: [ anInterval first ] gemstoneDo: [ anInterval _from ].
	stop := self gtDo: [ anInterval stop ] gemstoneDo: [ anInterval _to ].
	aGtWireEncoderContext putTypeIdentifier: self typeIdentifier.
	aGtWireEncoderContext
		nextPut: start;
		nextPut: stop;
		nextPut: anInterval increment
]
