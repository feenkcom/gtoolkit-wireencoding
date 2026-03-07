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
