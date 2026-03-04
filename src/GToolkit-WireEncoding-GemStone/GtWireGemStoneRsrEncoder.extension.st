Extension { #name : #GtWireGemStoneRsrEncoder }

{ #category : #'*GToolkit-WireEncoding-GemStone' }
GtWireGemStoneRsrEncoder >> connection [

	^ (SessionTemps current at: #GtRsrServer) connection
]

{ #category : #'*GToolkit-WireEncoding-GemStone' }
GtWireGemStoneRsrEncoder >> currentWireService [

	^ SessionTemps current at: #GtRsrCurrentWireService
]

{ #category : #'*GToolkit-WireEncoding-GemStone' }
GtWireGemStoneRsrEncoder >> decodeWith: aGtWireEncoderContext [
	"It is up to the user to ensure the Object isn't GCd during transfer and decoding
	(which would allow the oop to be reused and the wrong object returned), or that the
	session is aborted."

	^ (self connection serviceAt: aGtWireEncoderContext next) asGtGsArgument
]
