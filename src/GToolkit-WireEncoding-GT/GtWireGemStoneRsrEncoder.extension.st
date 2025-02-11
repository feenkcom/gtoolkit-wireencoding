Extension { #name : #GtWireGemStoneRsrEncoder }

{ #category : #'*GToolkit-WireEncoding-GT' }
GtWireGemStoneRsrEncoder >> connection [

	^ GtGemStoneCurrentSession value connection
]

{ #category : #'*GToolkit-WireEncoding-GT' }
GtWireGemStoneRsrEncoder >> currentWireService [
	^ GtRsrCurrentWireService value
]
