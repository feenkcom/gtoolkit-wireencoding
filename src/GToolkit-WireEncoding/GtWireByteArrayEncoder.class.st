Class {
	#name : #GtWireByteArrayEncoder,
	#superclass : #GtWireObjectEncoder,
	#category : #'GToolkit-WireEncoding'
}

{ #category : #accessing }
GtWireByteArrayEncoder class >> typeIdentifier [

	^ 4
]

{ #category : #'encoding - decoding' }
GtWireByteArrayEncoder >> decodeWith: aGtWireEncoderContext [

	^ aGtWireEncoderContext nextByteArray
]

{ #category : #'encoding - decoding' }
GtWireByteArrayEncoder >> encode: aByteArray with: aGtWireEncoderContext [

	aGtWireEncoderContext
		putTypeIdentifier: self typeIdentifier;
		putByteArray: aByteArray
]
