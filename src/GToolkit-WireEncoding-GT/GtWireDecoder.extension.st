Extension { #name : #GtWireDecoder }

{ #category : #'*GToolkit-WireEncoding-GT' }
GtWireDecoder >> nextNullTerminatedUtf8 [
	| utf8Encoder ch null |

	utf8Encoder := ZnCharacterEncoder utf8.
	null := Character null.
	^ String streamContents: [ :wStream |
		[ ch := utf8Encoder nextFromStream: stream.
		ch = null ifFalse:
			[ wStream nextPut: ch ].
		ch ~= null ] whileTrue ].
]
