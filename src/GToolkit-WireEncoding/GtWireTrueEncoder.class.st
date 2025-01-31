Class {
	#name : #GtWireTrueEncoder,
	#superclass : #GtWireBooleanEncoder,
	#category : #'GToolkit-WireEncoding'
}

{ #category : #accessing }
GtWireTrueEncoder class >> typeIdentifier [

	^ 2
]

{ #category : #'encoding - decoding' }
GtWireTrueEncoder >> decodeWith: aGtWireEncoderContext [

	^ true
]
