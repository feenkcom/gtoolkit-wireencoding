Class {
	#name : #GtWireOrderedCollectionEncoder,
	#superclass : #GtWireCollectionEncoder,
	#category : #'GToolkit-WireEncoding'
}

{ #category : #'encoding - decoding' }
GtWireOrderedCollectionEncoder class >> decodeWith: aGtWireEncoderContext [
	"Decode the OrderedCollection on the supplied context"
	| count |

	count := aGtWireEncoderContext stream packedInteger.
	^ OrderedCollection new: count streamContents: [ :arrayStream |
		count timesRepeat:
			[ arrayStream nextPut: aGtWireEncoderContext next ] ]
]

{ #category : #accessing }
GtWireOrderedCollectionEncoder class >> typeIdentifier [

	^ 10
]
