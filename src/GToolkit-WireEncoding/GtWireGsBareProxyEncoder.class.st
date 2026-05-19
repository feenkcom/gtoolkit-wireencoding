Class {
	#name : #GtWireGsBareProxyEncoder,
	#superclass : #GtWireObjectEncoder,
	#category : #'GToolkit-WireEncoding'
}

{ #category : #'encoding - decoding' }
GtWireGsBareProxyEncoder >> decodeWith: aGtWireEncoderContext [

	^ GtWireGemStoneRsrEncoder new
		decodeWith: aGtWireEncoderContext
]

{ #category : #'encoding - decoding' }
GtWireGsBareProxyEncoder >> encode: aBareProxy with: aGtWireEncoderContext [

	^ GtWireGemStoneRsrEncoder new
		encode: aBareProxy asGtProxyObject
		with: aGtWireEncoderContext
]

{ #category : #testing }
GtWireGsBareProxyEncoder >> isProxyObjectEncoder [
	"Answer a boolean indicating whether the receiver is a type of proxy encoder.
	Proxy encoding is platform dependent."

	^ true.
]
