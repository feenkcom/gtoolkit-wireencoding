Extension { #name : #GtWireGemStoneWithRsrEncoder }

{ #category : #'*GToolkit-WireEncoding-GemStone' }
GtWireGemStoneWithRsrEncoder >> connection [

	^ (SessionTemps current at: #GtRsrServer) connection
]

{ #category : #'*GToolkit-WireEncoding-GemStone' }
GtWireGemStoneWithRsrEncoder >> currentWireService [

	^ SessionTemps current at: #GtRsrCurrentWireService
]

{ #category : #'*GToolkit-WireEncoding-GemStone' }
GtWireGemStoneWithRsrEncoder >> decodeWith: aGtWireEncoderContext [

	self notYetImplemented
]
