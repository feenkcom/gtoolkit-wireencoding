Class {
	#name : #GtWireBlockClosureEncoder,
	#superclass : #GtWireObjectEncoder,
	#category : #'GToolkit-WireEncoding'
}

{ #category : #accessing }
GtWireBlockClosureEncoder class >> typeIdentifier [

	^ 21
]

{ #category : #'encoding - decoding' }
GtWireBlockClosureEncoder >> decodeWith: aGtWireEncoderContext [

	^ self
		gtDo: [ BlockClosure evaluate: aGtWireEncoderContext next ]
		gemstoneDo: [ | bindings receiver |
			bindings := GsCurrentSession currentSession symbolList.
			receiver := self.
			aGtWireEncoderContext next evaluate.
				"_compileInContext: receiver symbolList: bindings" ].

]

{ #category : #'encoding - decoding' }
GtWireBlockClosureEncoder >> encode: aBlockClosure with: aGtWireEncoderContext [

	aBlockClosure isClean ifFalse:
		[ self error: 'BlockClosures must be clean' ].
	aGtWireEncoderContext 
		putTypeIdentifier: self class typeIdentifier;
		nextPut: (self
			gtDo: [ aBlockClosure printString ]
			gemstoneDo: [ aBlockClosure method _sourceStringForBlock  ]).
]
