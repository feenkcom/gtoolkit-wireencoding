Class {
	#name : #GtWireDecoder,
	#superclass : #GtWireEncoderDecoder,
	#instVars : [
		'proxyObjectMap'
	],
	#category : #'GToolkit-WireEncoding'
}

{ #category : #'instance creation' }
GtWireDecoder class >> on: aReadStream [

	^ self basicNew initialize stream:
		(GtWireStream on: aReadStream)
]

{ #category : #accessing }
GtWireDecoder >> next [
	| type |

	type := self nextTypeIdentifier.
	^ (self reverseMap at: type) decodeWith: self.
]

{ #category : #decoding }
GtWireDecoder >> nextByteArray [

	^ stream next: self nextSize
]

{ #category : #decoding }
GtWireDecoder >> nextFloat64 [

	^ stream float64
]

{ #category : #decoding }
GtWireDecoder >> nextInt64 [

	^ stream int64
]

{ #category : #decoding }
GtWireDecoder >> nextPackedInteger [

	^ stream packedInteger
]

{ #category : #decoding }
GtWireDecoder >> nextSize [

	^ stream packedInteger
]

{ #category : #decoding }
GtWireDecoder >> nextString [
	"Answer the next string.
	GemStone requires conversion from a Unicode object to a String (asString)"

	^ (stream next: self nextSize) utf8Decoded asString
]

{ #category : #decoding }
GtWireDecoder >> nextTypeIdentifier [

	^ stream packedInteger
]
