Class {
	#name : #GtWireCharacterEncoder,
	#superclass : #GtWireObjectEncoder,
	#category : #'GToolkit-WireEncoding'
}

{ #category : #'encoding - decoding' }
GtWireCharacterEncoder class >> decodeWith: aGtWireEncoderContext [

	^ Character value: aGtWireEncoderContext stream packedInteger
]

{ #category : #'encoding - decoding' }
GtWireCharacterEncoder class >> encode: aCharacter with: aGtWireEncoderContext [

	aGtWireEncoderContext stream
		packedInteger: self typeIdentifier;
		packedInteger: aCharacter codePoint
]

{ #category : #accessing }
GtWireCharacterEncoder class >> typeIdentifier [

	^ 7
]
