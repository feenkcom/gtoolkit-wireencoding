Class {
	#name : #GtWireDecoder,
	#superclass : #GtWireEncoderDecoder,
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

{ #category : #'as yet unclassified' }
GtWireDecoder >> nextByteArray [

	^ stream next: self nextSize
]

{ #category : #'as yet unclassified' }
GtWireDecoder >> nextFloat64 [

	^ stream float64
]

{ #category : #'as yet unclassified' }
GtWireDecoder >> nextInt64 [

	^ stream int64
]

{ #category : #'as yet unclassified' }
GtWireDecoder >> nextPackedInteger [

	^ stream packedInteger
]

{ #category : #'as yet unclassified' }
GtWireDecoder >> nextSize [

	^ stream packedInteger
]

{ #category : #'as yet unclassified' }
GtWireDecoder >> nextTypeIdentifier [

	^ stream packedInteger
]
