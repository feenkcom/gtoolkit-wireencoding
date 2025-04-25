Class {
	#name : #GtWireGemStoneRsrEncoder,
	#superclass : #GtWireObjectEncoder,
	#category : #'GToolkit-WireEncoding'
}

{ #category : #accessing }
GtWireGemStoneRsrEncoder class >> typeIdentifier [

	^ 24
]

{ #category : #'encoding - decoding' }
GtWireGemStoneRsrEncoder >> decodeWith: aGtWireEncoderContext [
	"It is up to the user to ensure the Object isn't GCd during transfer and decoding
	(which would allow the oop to be reused and the wrong object returned), or that the
	session is aborted."

	^ (self connection serviceAt: aGtWireEncoderContext next) asGtGsArgument.
]

{ #category : #'encoding - decoding' }
GtWireGemStoneRsrEncoder >> encode: aRsrService with: aGtWireEncoderContext [
	"It is up to the user to ensure that anObject isn't GCd during transfer and decoding
	(which would allow the oop to be reused and the wrong object returned), or that the
	session is aborted."

	"Ensure that the service is at least registered so that it has an _id
	and has a reference in the service.
	Remaining set up will be done during snapshot analysis."

	self connection _ensureRegistered: aRsrService.
	self currentWireService addRoot: aRsrService.
	aGtWireEncoderContext
		putTypeIdentifier: self class typeIdentifier;
		nextPut: aRsrService _id
]

{ #category : #testing }
GtWireGemStoneRsrEncoder >> isProxyObjectEncoder [
	"Answer a boolean indicating whether the receiver is a type of proxy encoder.
	Proxy encoding is platform dependent."

	^ true.
]
