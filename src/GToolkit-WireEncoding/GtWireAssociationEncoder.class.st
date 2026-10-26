Class {
	#name : #GtWireAssociationEncoder,
	#superclass : #GtWireObjectEncoder,
	#category : #'GToolkit-WireEncoding'
}

{ #category : #accessing }
GtWireAssociationEncoder class >> typeIdentifier [

	^ 15
]

{ #category : #'encoding - decoding' }
GtWireAssociationEncoder >> decodeWith: aGtWireEncoderContext [

	^ Association
		key: aGtWireEncoderContext next
		value: aGtWireEncoderContext next
]

{ #category : #'encoding - decoding' }
GtWireAssociationEncoder >> encode: anAssociation with: aGtWireEncoderContext [

	aGtWireEncoderContext putTypeIdentifier: self typeIdentifier.
	aGtWireEncoderContext
		nextPut: anAssociation key;
		nextPut: anAssociation value
]
