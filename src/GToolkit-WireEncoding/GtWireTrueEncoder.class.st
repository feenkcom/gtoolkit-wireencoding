Class {
	#name : #GtWireTrueEncoder,
	#superclass : #GtWireBooleanEncoder,
	#category : #'GToolkit-WireEncoding-ObjectEncoder'
}

{ #category : #'encoding - decoding' }
GtWireTrueEncoder class >> decodeWith: aGtWireEncoderContext [

	^ true
]

{ #category : #accessing }
GtWireTrueEncoder class >> typeIdentifier [

	^ 2
]
