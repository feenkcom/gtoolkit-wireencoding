Class {
	#name : #GtWireFalseEncoder,
	#superclass : #GtWireBooleanEncoder,
	#category : #'GToolkit-WireEncoding'
}

{ #category : #accessing }
GtWireFalseEncoder class >> typeIdentifier [

	^ 3
]

{ #category : #'encoding - decoding' }
GtWireFalseEncoder >> decodeWith: aGtWireEncoderContext [

	^ false
]
