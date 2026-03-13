"
GtWireGemStoneRemoteObjectEncoder is used in a replication spec to specify that instances of the registered class should be returned as remote objects, i.e. they are returned by value and a proxy is registered with GtRsrProxyServiceClient.

Note that GtWireGemStoneRemoteObjectEncoder has no typeIdentifier as it doesn't directly encode objects, they are encoded by GtWireGemStoneWithRsrEncoder and the appropriate value encoder.
"
Class {
	#name : #GtWireGemStoneRemoteObjectEncoder,
	#superclass : #GtWireObjectEncoder,
	#instVars : [
		'encoder'
	],
	#category : #'GToolkit-WireEncoding'
}

{ #category : #'encoding - decoding' }
GtWireGemStoneRemoteObjectEncoder >> decodeWith: aGtWireEncoderContext [

	self error: self class name asString, ' should never need decoding'
]

{ #category : #'encoding - decoding' }
GtWireGemStoneRemoteObjectEncoder >> encode: anObject with: aGtWireEncoderContext [
	"Encode the supplied object as a remote object, i.e. it is returned by value and a proxy is registered with GtRsrProxyServiceClient"

	GtWireGemStoneWithRsrEncoder new
		encode: anObject 
		with: aGtWireEncoderContext
		objectEncoder: encoder.
]

{ #category : #accessing }
GtWireGemStoneRemoteObjectEncoder >> encoder [
	^ encoder
]

{ #category : #accessing }
GtWireGemStoneRemoteObjectEncoder >> encoder: anObject [
	encoder := anObject
]
