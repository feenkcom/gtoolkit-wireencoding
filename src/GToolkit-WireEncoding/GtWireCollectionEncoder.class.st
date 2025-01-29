Class {
	#name : #GtWireCollectionEncoder,
	#superclass : #GtWireObjectEncoder,
	#category : #'GToolkit-WireEncoding'
}

{ #category : #'encoding - decoding' }
GtWireCollectionEncoder class >> decodeWith: aGtWireEncoderContext [
	"Decode the array on the supplied context"
	| count |

	count := aGtWireEncoderContext stream packedInteger.
	^ Array new: count streamContents: [ :arrayStream |
		count timesRepeat:
			[ arrayStream nextPut: aGtWireEncoderContext next ] ]
]

{ #category : #'encoding - decoding' }
GtWireCollectionEncoder class >> encode: aCollection with: aGtWireEncoderContext [

	aGtWireEncoderContext stream
		packedInteger: self typeIdentifier;
		packedInteger: aCollection size.
	aCollection do: [ :each |
		aGtWireEncoderContext nextPut: each ].
]
