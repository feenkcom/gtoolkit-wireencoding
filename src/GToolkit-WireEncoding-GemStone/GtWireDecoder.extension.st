Extension { #name : #GtWireDecoder }

{ #category : #'*GToolkit-WireEncoding-GemStone' }
GtWireDecoder >> nextNullTerminatedUtf8 [
	| ch byteArray wStream |

	byteArray := ByteArray new: 1024.
	wStream := byteArray writeStream.
	[ (ch := stream next) codePoint = 0 ] whileFalse:
		[ wStream nextPut: ch ].
	^ wStream contents utf8Decoded
]
