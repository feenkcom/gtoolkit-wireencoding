Class {
	#name : #GtWireFalseEncoder,
	#superclass : #GtWireBooleanEncoder,
	#category : #'GToolkit-WireEncoding'
}

{ #category : #'encoding - decoding' }
GtWireFalseEncoder class >> decodeWith: aGtWireEncoderContext [

	^ false
]

{ #category : #accessing }
GtWireFalseEncoder class >> typeIdentifier [

	^ 3
]
