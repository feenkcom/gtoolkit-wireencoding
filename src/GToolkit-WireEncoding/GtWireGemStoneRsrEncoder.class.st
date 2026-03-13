"
GtWireGemStoneRsrEncoder is used to pass GemStone Rsr proxy objects via wire encoding.
"
Class {
	#name : #GtWireGemStoneRsrEncoder,
	#superclass : #GtWireObjectEncoder,
	#category : #'GToolkit-WireEncoding'
}

{ #category : #accessing }
GtWireGemStoneRsrEncoder class >> typeIdentifier [

	^ 24
]

{ #category : #testing }
GtWireGemStoneRsrEncoder >> isProxyObjectEncoder [
	"Answer a boolean indicating whether the receiver is a type of proxy encoder.
	Proxy encoding is platform dependent."

	^ true.
]
