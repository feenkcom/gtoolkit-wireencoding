Class {
	#name : #GtWireFalseEncoder,
	#superclass : #GtWireBooleanEncoder,
	#category : #'GToolkit-WireEncoding-ObjectEncoder'
}

{ #category : #'encoding - decoding' }
GtWireFalseEncoder class >> decodeWith: aGtWireEncoderContext [

	^ false
]

{ #category : #accessing }
GtWireFalseEncoder class >> typeIdentifier [

	^ 3
]
