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
	| stream |

	stream := aGtWireEncoderContext stream.
	^ BlockClosure evaluate: aGtWireEncoderContext nextNullTerminatedUtf8.
]

{ #category : #'encoding - decoding' }
GtWireBlockClosureEncoder >> encode: aBlockClosure with: aGtWireEncoderContext [

	aBlockClosure isClean ifFalse:
		[ self error: 'BlockClosures must be clean' ].
	aGtWireEncoderContext 
		putTypeIdentifier: self class typeIdentifier;
		putNullTerminatedUtf8: aBlockClosure printString.
]
