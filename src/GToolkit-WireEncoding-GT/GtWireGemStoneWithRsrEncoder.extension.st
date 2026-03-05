Extension { #name : #GtWireGemStoneWithRsrEncoder }

{ #category : #'*GToolkit-WireEncoding-GT' }
GtWireGemStoneWithRsrEncoder >> connection [

	^ GtGemStoneCurrentSession value connection
]

{ #category : #'*GToolkit-WireEncoding-GT' }
GtWireGemStoneWithRsrEncoder >> currentWireService [
	^ GtRsrCurrentWireService value
]

{ #category : #'*GToolkit-WireEncoding-GT' }
GtWireGemStoneWithRsrEncoder >> decodeWith: aGtWireEncoderContext [
	"It is up to the user to ensure the Object isn't GCd during transfer and decoding
	(which would allow the oop to be reused and the wrong object returned), or that the
	session is aborted."
	| proxy object |

	proxy := (self connection serviceAt: aGtWireEncoderContext next) asGtGsArgument
		gtPharoProxyInitializeWithSession: self gtSession.
	object := aGtWireEncoderContext next asGtGsArgument
		gtPharoProxyInitializeWithSession: self gtSession.
	aGtWireEncoderContext proxyObjectMap at: object put: proxy asGtBareProxyObject.
	^ object
]

{ #category : #'*GToolkit-WireEncoding-GT' }
GtWireGemStoneWithRsrEncoder >> encode: aRsrService with: aGtWireEncoderContext [

	self error: 'Rsr client encoding not yet supported'.
]

{ #category : #'*GToolkit-WireEncoding-GT' }
GtWireGemStoneWithRsrEncoder >> encode: aRsrService with: aGtWireEncoderContext objectEncoder: objectEncoder [

	self error: 'Rsr client encoding not yet supported'.
]

{ #category : #'*GToolkit-WireEncoding-GT' }
GtWireGemStoneWithRsrEncoder >> gtSession [

	^ GtGemStoneCurrentSession value
]
