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

	^ STON fromString: aGtWireEncoderContext nextByteArray utf8Decoded.
]

{ #category : #'encoding - decoding' }
GtWireStonEncoder >> encode: anObject with: aGtWireEncoderContext [
	| stonEncoded |

	stonEncoded := (STON toString: anObject) utf8Encoded.
	aGtWireEncoderContext
		putTypeIdentifier: self class typeIdentifier;
		putByteArray: stonEncoded.
]
