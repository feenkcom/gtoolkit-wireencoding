Class {
	#name : #GtWireBooleanEncoder,
	#superclass : #GtWireObjectEncoder,
	#category : #'GToolkit-WireEncoding'
}

{ #category : #'encoding - decoding' }
GtWireBooleanEncoder >> encode: anObject with: aGtWireEncoderContext [

	aGtWireEncoderContext putTypeIdentifier: (anObject
		ifTrue: [ GtWireTrueEncoder ]
		ifFalse: [ GtWireFalseEncoder ])
			typeIdentifier
]
