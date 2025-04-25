Class {
	#name : #GtWireGemStoneOopEncoder,
	#superclass : #GtWireObjectEncoder,
	#category : #'GToolkit-WireEncoding'
}

{ #category : #accessing }
GtWireGemStoneOopEncoder class >> typeIdentifier [

	^ 23
]

{ #category : #'encoding - decoding' }
GtWireGemStoneOopEncoder >> decodeWith: aGtWireEncoderContext [
	"It is up to the user to ensure the Object isn't GCd during transfer and decoding
	(which would allow the oop to be reused and the wrong object returned), or that the
	session is aborted."

	^ self
		gtDo: [ #GtGemStoneCurrentSession asClass value evaluateAndWaitReturnProxy:
			'Object objectForOop: ', aGtWireEncoderContext nextPackedInteger asString ]
		gemstoneDo: [ Object objectForOop: aGtWireEncoderContext nextPackedInteger ]
]

{ #category : #'encoding - decoding' }
GtWireGemStoneOopEncoder >> encode: anObject with: aGtWireEncoderContext [
	"It is up to the user to ensure that anObject isn't GCd during transfer and decoding
	(which would allow the oop to be reused and the wrong object returned), or that the
	session is aborted."

	aGtWireEncoderContext 
		putTypeIdentifier: self class typeIdentifier;
		putPackedInteger: anObject asOop
]

{ #category : #testing }
GtWireGemStoneOopEncoder >> isProxyObjectEncoder [
	"Answer a boolean indicating whether the receiver is a type of proxy encoder.
	Proxy encoding is platform dependent."

	^ true.
]
