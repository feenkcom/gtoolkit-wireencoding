Extension { #name : #GtWireEncoder }

{ #category : #'*GToolkit-WireEncoding-GT' }
GtWireEncoder class >> gemStoneEncoder [
	"For performance reasons the mapping of proxies is normally statically configured in {{gtMethod:GtWireEncoderDecoder class>>#getDefaultMap}},
	however encoding will fail if a GemStone Rsr proxy class is added without regenerating the mapping.
	This encoder will dynamically handle adding proxy classes at the cost of doing additional lookup for each unmapped object."

	^ [ :anObject | 
		(anObject isKindOf: GtRsrProxyServiceClient)
			ifTrue: [ GtWireGemStoneRsrEncoder new ]
			ifFalse: [ GtWireObjectByNameEncoder new ] ]
]

{ #category : #'*GToolkit-WireEncoding-GT' }
GtWireEncoder class >> setDefaultGemStoneEncoder [
	"Set the default encoder to handle GtRsrProxyServiceClients.
	For performance reasons the mapping of proxies is normally statically configured in {{gtMethod:GtWireEncoderDecoder class>>#getDefaultMap}},
	however encoding will fail if a GemStone Rsr proxy class is added without regenerating the mapping.
	This encoder will dynamically handle adding proxy classes at the cost of doing additional lookup for each unmapped object."

	self defaultEncoder: self gemStoneEncoder
]
