Extension { #name : #GtWireStream }

{ #category : #'*GToolkit-WireEncoding-GemStone' }
GtWireStream >> float64 [
	| byteArray |

	byteArray := self next: 8.
	^ byteArray doubleAt: 1.
]

{ #category : #'*GToolkit-WireEncoding-GemStone' }
GtWireStream >> float64: aFloat [
	| byteArray |

	byteArray := ByteArray new: 8.
	byteArray doubleAt: 1 put: aFloat.
	self nextPutAll: byteArray.
]
