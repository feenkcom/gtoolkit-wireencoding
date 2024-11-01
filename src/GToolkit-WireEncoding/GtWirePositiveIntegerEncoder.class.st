Class {
	#name : #GtWirePositiveIntegerEncoder,
	#superclass : #GtWireIntegerEncoder,
	#category : #'GToolkit-WireEncoding-ObjectEncoder'
}

{ #category : #'encoding - decoding' }
GtWirePositiveIntegerEncoder class >> decodeWith: aGtWireEncoderContext [

	^ aGtWireEncoderContext stream packedInteger
]

{ #category : #'encoding - decoding' }
GtWirePositiveIntegerEncoder class >> encode: anInteger with: aGtWireEncoderContext [

	aGtWireEncoderContext stream
		packedInteger: self typeIdentifier;
		packedInteger: anInteger
]

{ #category : #accessing }
GtWirePositiveIntegerEncoder class >> typeIdentifier [

	^ 13
]
