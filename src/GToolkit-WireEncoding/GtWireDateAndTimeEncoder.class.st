Class {
	#name : #GtWireDateAndTimeEncoder,
	#superclass : #GtWireObjectEncoder,
	#category : #'GToolkit-WireEncoding'
}

{ #category : #accessing }
GtWireDateAndTimeEncoder class >> typeIdentifier [

	^ 12
]

{ #category : #encoding }
GtWireDateAndTimeEncoder >> decodeWith: aGtWireEncoderContext [
	"Restore the encoded instant and offset, not the local wall-clock representation."
	| unixSeconds nanoSeconds offset |

	unixSeconds := aGtWireEncoderContext nextPackedInteger.
	nanoSeconds := aGtWireEncoderContext nextPackedInteger.
	offset := aGtWireEncoderContext next.
	^ self
		gtDo: [ (DateAndTime fromUnixTime: unixSeconds)
			setNanoSeconds: nanoSeconds;
			offset: (Duration seconds: offset) ]
		gemstoneDo: [ DateAndTime posixSeconds: (unixSeconds + (nanoSeconds / (10 raisedTo: 9))) offset: (Duration seconds: offset) ].
]

{ #category : #'encoding - decoding' }
GtWireDateAndTimeEncoder >> encode: aDateAndTime with: aGtWireEncoderContext [

	aGtWireEncoderContext
		putTypeIdentifier: self class typeIdentifier;
		putPackedInteger: aDateAndTime asUnixTime truncated;
		putPackedInteger: aDateAndTime nanoSecond;
		nextPut: aDateAndTime offset asSeconds.
]
