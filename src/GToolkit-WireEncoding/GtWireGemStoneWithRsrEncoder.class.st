"
GtWireGemStoneWithRsrEncoder is used to pass a remote object back from GemStone to GT.

A remote object is one which is passed back by value, but is also registered with GtRsrProxyServiceClient so that it's proxy object automatically accessible.

GtWireGemStoneWithRsrEncoder is not used directly in a replication spec, but either by the GtRemoteObjectWireEncoder or GtWireGemStoneRemoteObjectEncoder.
"
Class {
	#name : #GtWireGemStoneWithRsrEncoder,
	#superclass : #GtWireObjectEncoder,
	#category : #'GToolkit-WireEncoding'
}

{ #category : #accessing }
GtWireGemStoneWithRsrEncoder class >> typeIdentifier [

	^ 27
]

{ #category : #testing }
GtWireGemStoneWithRsrEncoder >> isProxyObjectEncoder [
	"Answer a boolean indicating whether the receiver is a type of proxy encoder.
	Proxy encoding is platform dependent."

	^ true.
]
