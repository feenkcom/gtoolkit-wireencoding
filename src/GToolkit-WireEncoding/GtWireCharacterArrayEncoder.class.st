Class {
	#name : #GtWireCharacterArrayEncoder,
	#superclass : #GtWireObjectEncoder,
	#category : #'GToolkit-WireEncoding'
}

{ #category : #'encoding - decoding' }
GtWireCharacterArrayEncoder >> decodeWith: aGtWireEncoderContext [
	| stream |

	stream := aGtWireEncoderContext stream.
	^ (stream next: stream packedInteger) utf8Decoded
]

{ #category : #'encoding - decoding' }
GtWireCharacterArrayEncoder >> encode: aString with: aGtWireEncoderContext [
	| encoded |

	encoded := aString utf8Encoded.
	aGtWireEncoderContext
		putTypeIdentifier: self typeIdentifier;
		putSize: encoded size;
		putByteArray: encoded
]
