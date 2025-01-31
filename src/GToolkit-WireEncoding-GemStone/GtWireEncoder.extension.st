Extension { #name : 'GtWireEncoder' }

{ #category : '*GToolkit-WireEncoding-GemStone' }
GtWireEncoder >> putNullTerminatedUtf8: aString [

	stream nextPutAll: aString utf8Encoded.
	stream nextPut: 0.
]
