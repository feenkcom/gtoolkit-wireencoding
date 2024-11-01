Class {
	#name : #GtWireCharacterArrayEncoder,
	#superclass : #GtWireObjectEncoder,
	#category : #'GToolkit-WireEncoding-ObjectEncoder'
}

{ #category : #'encoding - decoding' }
GtWireCharacterArrayEncoder class >> decodeWith: aGtWireEncoderContext [
	| stream |

	stream := aGtWireEncoderContext stream.
	^ (stream next: stream packedInteger) utf8Decoded
]

{ #category : #'encoding - decoding' }
GtWireCharacterArrayEncoder class >> encode: aString with: aGtWireEncoderContext [
	| encoded |

	encoded := aString utf8Encoded.
	aGtWireEncoderContext stream
		packedInteger: self typeIdentifier;
		packedInteger: encoded size;
		nextPutAll: encoded
]
