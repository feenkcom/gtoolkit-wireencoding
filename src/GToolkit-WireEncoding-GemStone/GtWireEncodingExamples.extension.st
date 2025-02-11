Extension { #name : #GtWireEncodingExamples }

{ #category : #'*GToolkit-WireEncoding-GemStone' }
GtWireEncodingExamples >> assert: aBoolean [

	self
		assert: aBoolean
		description: 'Assertion failed'.
]

{ #category : #'*GToolkit-WireEncoding-GemStone' }
GtWireEncodingExamples >> assert: aBoolean description: aString [

	aBoolean == true ifFalse:
		[ TestResult failure signal: aString value ]
]

{ #category : #'*GToolkit-WireEncoding-GemStone' }
GtWireEncodingExamples >> assert: actual equals: expected [

	self
		assert: actual = expected
		description: actual printString, ' is not equal to ', expected printString.
]
