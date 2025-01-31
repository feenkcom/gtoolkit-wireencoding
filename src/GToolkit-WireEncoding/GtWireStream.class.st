Class {
	#name : #GtWireStream,
	#superclass : #Stream,
	#instVars : [
		'wrappedStream'
	],
	#category : #'GToolkit-WireEncoding'
}

{ #category : #'instance creation' }
GtWireStream class >> on: aStream [

	^ self basicNew on: aStream
]

{ #category : #accessing }
GtWireStream >> contents [

	^ wrappedStream contents
]

{ #category : #'encoding - decoding' }
GtWireStream >> float64 [
	| byteArray |

	byteArray := self next: 8.
	^ byteArray doubleAt: 1.
]

{ #category : #'encoding - decoding' }
GtWireStream >> float64: aFloat [
	| byteArray |

	byteArray := ByteArray new: 8.
	byteArray doubleAt: 1 put: aFloat.
	self nextPutAll: byteArray.
]

{ #category : #'as yet unclassified' }
GtWireStream >> int64 [
	"Answer the next signed, 32-bit integer from this (binary) stream."
	"Details: As a fast check for negative number, check the high bit of the first digit"
	| n firstDigit |
	n := firstDigit := self next.
	n := (n bitShift: 8) + self next.
	n := (n bitShift: 8) + self next.
	n := (n bitShift: 8) + self next.
	n := (n bitShift: 8) + self next.
	n := (n bitShift: 8) + self next.
	n := (n bitShift: 8) + self next.
	n := (n bitShift: 8) + self next.
	firstDigit >= 128 ifTrue: [n := -16r10000000000000000 + n].  "decode negative 64-bit integer"
	^ n
]

{ #category : #'as yet unclassified' }
GtWireStream >> int64: anInteger [
	| n |
	(anInteger < -16r8000000000000000) | (anInteger >= 16r8000000000000000)
		ifTrue: [self error: 'outside 64-bit integer range'].

	anInteger < 0
		ifTrue: [n := 16r10000000000000000 + anInteger]
		ifFalse: [n := anInteger].
	self nextPut: (n digitAt: 8).
	self nextPut: (n digitAt: 7).
	self nextPut: (n digitAt: 6).
	self nextPut: (n digitAt: 5).
	self nextPut: (n digitAt: 4).
	self nextPut: (n digitAt: 3).
	self nextPut: (n digitAt: 2).
	self nextPut: (n digitAt: 1).
]

{ #category : #accessing }
GtWireStream >> next [

	^ wrappedStream next
]

{ #category : #accessing }
GtWireStream >> next: anInteger [
	"Answer the next anInteger number of objects accessible by the receiver."

	^ wrappedStream next: anInteger
]

{ #category : #accessing }
GtWireStream >> nextPut: aByte [

	wrappedStream nextPut: aByte
]

{ #category : #'as yet unclassified' }
GtWireStream >> on: aStream [

	wrappedStream := aStream
]

{ #category : #'encoding - decoding' }
GtWireStream >> packedInteger [
	| result |

	result := 0.
	[ | byte |
	byte := self next.
	result := (result bitShift: 7) + (byte bitAnd: 16r7F).
	byte >= 16r80 ] whileTrue.
	^ result
]

{ #category : #'encoding - decoding' }
GtWireStream >> packedInteger: anInteger [
	| bitCount byteCount |

	anInteger isInteger ifFalse:
		[ self error: anInteger asString, ' isn''t integer' ].
	anInteger < 0 ifTrue:
		[ self error: anInteger asString, ' is less than 0' ].
	anInteger = 0 ifTrue:
		[ self nextPut: 0.
		^ self ].

	bitCount := (anInteger log: 2) floor + 1.
	byteCount := (bitCount / 7) ceiling.
	byteCount to: 1 by: -1 do: [ :byteOffset |
		| byte |
		byte := (anInteger bitShift: (byteOffset - 1 * -7)) bitAnd: 16r7F.
		byteOffset > 1 ifTrue: [ byte := byte bitOr: 16r80 ].
		self nextPut: byte ].
]

{ #category : #accessing }
GtWireStream >> position [

	^ wrappedStream position
]

{ #category : #'as yet unclassified' }
GtWireStream >> reset [

	wrappedStream reset
]
