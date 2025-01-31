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
	^ BlockClosure evaluate: (stream next: stream packedInteger) utf8Decoded
]

{ #category : #'encoding - decoding' }
GtWireBlockClosureEncoder >> encode: aBlockClosure with: aGtWireEncoderContext [
	| bcSrc |

	aBlockClosure isClean ifFalse:
		[ self error: 'BlockClosures must be clean' ].
	bcSrc := aBlockClosure printString utf8Encoded.
	aGtWireEncoderContext 
		putTypeIdentifier: self class typeIdentifier;
		putSize: bcSrc size;
		putByteArray: bcSrc.
]
