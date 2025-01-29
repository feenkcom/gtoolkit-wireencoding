Class {
	#name : #GtWireAssociationEncoder,
	#superclass : #GtWireObjectEncoder,
	#category : #'GToolkit-WireEncoding'
}

{ #category : #'encoding - decoding' }
GtWireAssociationEncoder class >> decodeWith: aGtWireEncoderContext [

	^ Association
		key: aGtWireEncoderContext next
		value: aGtWireEncoderContext next
]

{ #category : #'encoding - decoding' }
GtWireAssociationEncoder class >> encode: anInteger with: aGtWireEncoderContext [

	aGtWireEncoderContext stream
		packedInteger: self typeIdentifier.
	aGtWireEncoderContext
		nextPut: anInteger key;
		nextPut: anInteger value
]

{ #category : #accessing }
GtWireAssociationEncoder class >> typeIdentifier [

	^ 15
]
