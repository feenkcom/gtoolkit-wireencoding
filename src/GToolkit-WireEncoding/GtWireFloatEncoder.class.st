Class {
	#name : #GtWireFloatEncoder,
	#superclass : #GtWireObjectEncoder,
	#category : #'GToolkit-WireEncoding-ObjectEncoder'
}

{ #category : #'encoding - decoding' }
GtWireFloatEncoder class >> decodeWith: aGtWireEncoderContext [

	^ aGtWireEncoderContext stream float64.
]

{ #category : #'encoding - decoding' }
GtWireFloatEncoder class >> encode: aFloat with: aGtWireEncoderContext [

	aGtWireEncoderContext stream 
		packedInteger: self typeIdentifier;
		float64: aFloat
]

{ #category : #accessing }
GtWireFloatEncoder class >> typeIdentifier [

	^ 17
]
