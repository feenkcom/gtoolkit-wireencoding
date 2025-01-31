Extension { #name : #GtWireEncoder }

{ #category : #'*GToolkit-WireEncoding-GT' }
GtWireEncoder >> putNullTerminatedUtf8: aString [

	ZnCharacterEncoder utf8
		next: aString size 
		putAll: aString 
		startingAt: 1 
		toStream: stream.
	stream nextPut: 0.
]
