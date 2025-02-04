Class {
	#name : #GtWireIntegerEncoder,
	#superclass : #GtWireObjectEncoder,
	#category : #'GToolkit-WireEncoding'
}

{ #category : #accessing }
GtWireIntegerEncoder class >> typeIdentifier [

	^ 16
]

{ #category : #'encoding - decoding' }
GtWireIntegerEncoder >> encode: anInteger with: aGtWireEncoderContext [

	anInteger >= 0
		ifTrue: [ GtWirePositiveIntegerEncoder new encode: anInteger with: aGtWireEncoderContext ]
		ifFalse: [ GtWireNegativeIntegerEncoder new encode: anInteger with: aGtWireEncoderContext ]
]
