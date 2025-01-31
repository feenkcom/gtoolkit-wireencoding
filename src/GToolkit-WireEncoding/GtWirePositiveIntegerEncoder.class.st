Class {
	#name : #GtWirePositiveIntegerEncoder,
	#superclass : #GtWireIntegerEncoder,
	#category : #'GToolkit-WireEncoding'
}

{ #category : #accessing }
GtWirePositiveIntegerEncoder class >> typeIdentifier [

	^ 13
]

{ #category : #'encoding - decoding' }
GtWirePositiveIntegerEncoder >> decodeWith: aGtWireEncoderContext [

	^ aGtWireEncoderContext stream packedInteger
]

{ #category : #'encoding - decoding' }
GtWirePositiveIntegerEncoder >> encode: anInteger with: aGtWireEncoderContext [

	aGtWireEncoderContext
		putTypeIdentifier: self typeIdentifier;
		putPackedInteger: anInteger
]
