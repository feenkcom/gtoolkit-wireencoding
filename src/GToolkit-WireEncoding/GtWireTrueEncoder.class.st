Class {
	#name : #GtWireTrueEncoder,
	#superclass : #GtWireBooleanEncoder,
	#category : #'GToolkit-WireEncoding'
}

{ #category : #'encoding - decoding' }
GtWireTrueEncoder class >> decodeWith: aGtWireEncoderContext [

	^ true
]

{ #category : #accessing }
GtWireTrueEncoder class >> typeIdentifier [

	^ 2
]
