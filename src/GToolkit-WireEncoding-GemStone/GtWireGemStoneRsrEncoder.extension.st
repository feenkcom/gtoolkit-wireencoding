Extension { #name : #GtWireGemStoneRsrEncoder }

{ #category : #'*GToolkit-WireEncoding-GemStone' }
GtWireGemStoneRsrEncoder >> connection [

	^ (SessionTemps current at: #GtRsrServer) connection
]

{ #category : #'*GToolkit-WireEncoding-GemStone' }
GtWireGemStoneRsrEncoder >> currentWireService [

	^ SessionTemps current at: #GtRsrCurrentWireService
]
