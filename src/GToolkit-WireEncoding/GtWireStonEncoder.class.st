Class {
	#name : #GtWireStonEncoder,
	#superclass : #GtWireObjectEncoder,
	#category : #'GToolkit-WireEncoding'
}

{ #category : #accessing }
GtWireStonEncoder class >> typeIdentifier [

	^ 19
]

{ #category : #'encoding - decoding' }
GtWireStonEncoder >> decodeWith: aGtWireEncoderContext [
	| stream stonString|

	stream := aGtWireEncoderContext stream.
	stonString := (stream next: stream packedInteger) utf8Decoded.
	^ STON fromString: stonString.
]

{ #category : #'encoding - decoding' }
GtWireStonEncoder >> encode: anObject with: aGtWireEncoderContext [
	| stonEncoded |

	stonEncoded := (STON toString: anObject) utf8Encoded.
	aGtWireEncoderContext
		putTypeIdentifier: self class typeIdentifier;
		putSize: stonEncoded size;
		putByteArray: stonEncoded.
]
