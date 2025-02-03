Extension { #name : 'GtWireDecoder' }

{ #category : '*GToolkit-WireEncoding-GemStone' }
GtWireDecoder >> nextNullTerminatedUtf8 [
	| ch wStream |

	wStream := WriteStream on: (ByteArray new: 1024).
	[ (ch := stream next) = 0 ] whileFalse:
		[ wStream nextPut: ch ].
	^ wStream contents utf8Decoded asString
]
