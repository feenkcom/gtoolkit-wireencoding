Extension { #name : #GtWireDecoder }

{ #category : #'*GToolkit-WireEncoding-GT' }
GtWireDecoder >> proxyObjectMap [

	^ proxyObjectMap ifNil: [ GtRsrProxyService gtClientGsServer proxyObjectMap ]
]

{ #category : #'*GToolkit-WireEncoding-GT' }
GtWireDecoder >> proxyObjectMap: anObject [
	proxyObjectMap := anObject
]
