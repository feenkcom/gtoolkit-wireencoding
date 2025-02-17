Class {
	#name : #GtWireCharacterArrayEncoder,
	#superclass : #GtWireObjectEncoder,
	#category : #'GToolkit-WireEncoding'
}

{ #category : #'encoding - decoding' }
GtWireCharacterArrayEncoder >> decodeWith: aGtWireEncoderContext [

	^ aGtWireEncoderContext nextString
]

{ #category : #'encoding - decoding' }
GtWireCharacterArrayEncoder >> encode: aString with: aGtWireEncoderContext [

	aGtWireEncoderContext 
		putTypeIdentifier: self typeIdentifier;
		putString: aString
]
