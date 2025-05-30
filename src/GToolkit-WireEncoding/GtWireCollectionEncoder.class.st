Class {
	#name : #GtWireCollectionEncoder,
	#superclass : #GtWireObjectEncoder,
	#category : #'GToolkit-WireEncoding'
}

{ #category : #'encoding - decoding' }
GtWireCollectionEncoder >> decodeWith: aGtWireEncoderContext [
	"Decode the array on the supplied context"
	| count |

	count := aGtWireEncoderContext nextSize.
	^ Array new: count streamContents: [ :arrayStream |
		count timesRepeat:
			[ arrayStream nextPut: aGtWireEncoderContext next ] ]
]

{ #category : #'encoding - decoding' }
GtWireCollectionEncoder >> encode: aCollection with: aGtWireEncoderContext [

	aGtWireEncoderContext
		putTypeIdentifier: self typeIdentifier;
		putSize: aCollection size.
	aCollection do: [ :each |
		aGtWireEncoderContext nextPut: each ].
]
