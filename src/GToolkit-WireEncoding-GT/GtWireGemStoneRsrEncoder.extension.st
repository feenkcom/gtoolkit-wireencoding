Extension { #name : #GtWireGemStoneRsrEncoder }

{ #category : #'*GToolkit-WireEncoding-GT' }
GtWireGemStoneRsrEncoder >> connection [

	^ GtGemStoneCurrentSession value connection
]

{ #category : #'*GToolkit-WireEncoding-GT' }
GtWireGemStoneRsrEncoder >> currentWireService [
	^ GtRsrCurrentWireService value
]

{ #category : #'*GToolkit-WireEncoding-GT' }
GtWireGemStoneRsrEncoder >> decodeWith: aGtWireEncoderContext [
	"It is up to the user to ensure the Object isn't GCd during transfer and decoding
	(which would allow the oop to be reused and the wrong object returned), or that the
	session is aborted."

	^ (self connection serviceAt: aGtWireEncoderContext next) asGtGsArgument
		gtPharoProxyInitializeWithSession: self gtSession.
]

{ #category : #'*GToolkit-WireEncoding-GT' }
GtWireGemStoneRsrEncoder >> encode: aRsrService with: aGtWireEncoderContext [
	"It is up to the user to ensure that anObject isn't GCd during transfer and decoding
	(which would allow the oop to be reused and the wrong object returned), or that the
	session is aborted."

	aRsrService isGtGemStoneRsrProxy ifFalse:
		[ self error: 'Rsr client encoding not yet supported' ].	

	"Ensure that the service is at least registered so that it has an _id
	and has a reference in the service.
	Remaining set up will be done during snapshot analysis."

	self connection _ensureRegistered: aRsrService.
	self currentWireService addRoot: aRsrService.
	aGtWireEncoderContext
		putTypeIdentifier: self class typeIdentifier;
		nextPut: aRsrService _id
]

{ #category : #'*GToolkit-WireEncoding-GT' }
GtWireGemStoneRsrEncoder >> gtSession [

	^ GtGemStoneCurrentSession value
]
