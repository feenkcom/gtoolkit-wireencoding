Class {
	#name : #GtWireReadStream,
	#superclass : #ReadStream,
	#category : #'GToolkit-WireEncoding-Streams'
}

{ #category : #'encoding - decoding' }
GtWireReadStream >> float64 [
	| byteArray |

	byteArray := self next: 8.
	^ byteArray doubleAt: 1.
]

{ #category : #'encoding - decoding' }
GtWireReadStream >> float64: aFloat [
	| byteArray |

	byteArray := ByteArray new: 8.
	byteArray doubleAt: 1 put: aFloat.
	self nextPutAll: byteArray.
]

{ #category : #'encoding - decoding' }
GtWireReadStream >> packedInteger [
	| integer count |
	integer := 0.
	count := self next.
	1 to: count do: [ :index |
		integer := integer + (self next bitShift: (count - index) * 8) ].
	^ integer
]

{ #category : #'encoding - decoding' }
GtWireReadStream >> packedInteger: anInteger [
	| count |

	self assert: anInteger >= 0
		description: 'Packed integers must be positive'.
	count := anInteger bytesCount.
	self assert: count < 256
		description: 'Integers must be less than 256 bytes in length'.
	self nextPut: count.
	count to: 1 by: -1 do: [ :digitIndex |
		self nextPut: (anInteger byteAt: digitIndex) ].
]
