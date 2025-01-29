Class {
	#name : #GtWireIntegerEncoder,
	#superclass : #GtWireObjectEncoder,
	#category : #'GToolkit-WireEncoding'
}

{ #category : #'encoding - decoding' }
GtWireIntegerEncoder class >> encode: anInteger with: aGtWireEncoderContext [

	anInteger >= 0
		ifTrue: [ GtWirePositiveIntegerEncoder encode: anInteger with: aGtWireEncoderContext ]
		ifFalse: [ GtWireNegativeIntegerEncoder encode: anInteger with: aGtWireEncoderContext ]
]

{ #category : #accessing }
GtWireIntegerEncoder class >> typeIdentifier [

	^ 16
]
