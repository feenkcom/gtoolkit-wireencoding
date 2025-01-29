Class {
	#name : #GtWireNegativeIntegerEncoder,
	#superclass : #GtWireIntegerEncoder,
	#category : #'GToolkit-WireEncoding'
}

{ #category : #'encoding - decoding' }
GtWireNegativeIntegerEncoder class >> decodeWith: aGtWireEncoderContext [

	^ aGtWireEncoderContext stream packedInteger negated
]

{ #category : #'encoding - decoding' }
GtWireNegativeIntegerEncoder class >> encode: anInteger with: aGtWireEncoderContext [

	aGtWireEncoderContext stream
		packedInteger: self typeIdentifier;
		packedInteger: anInteger negated
]

{ #category : #accessing }
GtWireNegativeIntegerEncoder class >> typeIdentifier [

	^ 14
]
