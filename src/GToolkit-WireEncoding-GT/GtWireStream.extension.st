Extension { #name : #GtWireStream }

{ #category : #'*GToolkit-WireEncoding-GT' }
GtWireStream >> float64 [
	| byteArray |

	byteArray := self next: 8.
	^ byteArray doubleAt: 1 bigEndian: true.
]

{ #category : #'*GToolkit-WireEncoding-GT' }
GtWireStream >> float64: aFloat [
	| byteArray |

	byteArray := ByteArray new: 8.
	byteArray doubleAt: 1 put: aFloat bigEndian: true.
	self nextPutAll: byteArray.
]
