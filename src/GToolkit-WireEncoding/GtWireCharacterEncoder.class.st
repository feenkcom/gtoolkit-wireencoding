Class {
	#name : #GtWireCharacterEncoder,
	#superclass : #GtWireObjectEncoder,
	#category : #'GToolkit-WireEncoding'
}

{ #category : #accessing }
GtWireCharacterEncoder class >> typeIdentifier [

	^ 7
]

{ #category : #'encoding - decoding' }
GtWireCharacterEncoder >> decodeWith: aGtWireEncoderContext [

	^ Character value: aGtWireEncoderContext stream packedInteger
]

{ #category : #'encoding - decoding' }
GtWireCharacterEncoder >> encode: aCharacter with: aGtWireEncoderContext [

	aGtWireEncoderContext
		putTypeIdentifier: self typeIdentifier;
		putPackedInteger: aCharacter codePoint.
]
