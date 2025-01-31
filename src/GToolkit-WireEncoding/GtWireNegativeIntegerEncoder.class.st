Class {
	#name : #GtWireNegativeIntegerEncoder,
	#superclass : #GtWireIntegerEncoder,
	#category : #'GToolkit-WireEncoding'
}

{ #category : #accessing }
GtWireNegativeIntegerEncoder class >> typeIdentifier [

	^ 14
]

{ #category : #'encoding - decoding' }
GtWireNegativeIntegerEncoder >> decodeWith: aGtWireEncoderContext [

	^ aGtWireEncoderContext stream packedInteger negated
]

{ #category : #'encoding - decoding' }
GtWireNegativeIntegerEncoder >> encode: anInteger with: aGtWireEncoderContext [

	aGtWireEncoderContext
		putTypeIdentifier: self typeIdentifier;
		putPackedInteger: anInteger negated
]
