Class {
	#name : #GtWireFloatEncoder,
	#superclass : #GtWireObjectEncoder,
	#category : #'GToolkit-WireEncoding'
}

{ #category : #accessing }
GtWireFloatEncoder class >> typeIdentifier [

	^ 17
]

{ #category : #'encoding - decoding' }
GtWireFloatEncoder >> decodeWith: aGtWireEncoderContext [

	^ aGtWireEncoderContext stream float64.
]

{ #category : #'encoding - decoding' }
GtWireFloatEncoder >> encode: aFloat with: aGtWireEncoderContext [

	aGtWireEncoderContext 
		putPackedInteger: self typeIdentifier;
		putFloat64: aFloat
]
