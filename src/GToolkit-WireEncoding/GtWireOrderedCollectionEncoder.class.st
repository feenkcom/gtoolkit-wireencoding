Class {
	#name : #GtWireOrderedCollectionEncoder,
	#superclass : #GtWireCollectionEncoder,
	#category : #'GToolkit-WireEncoding'
}

{ #category : #accessing }
GtWireOrderedCollectionEncoder class >> typeIdentifier [

	^ 10
]

{ #category : #'encoding - decoding' }
GtWireOrderedCollectionEncoder >> decodeWith: aGtWireEncoderContext [
	"Decode the OrderedCollection on the supplied context"
	| count |

	count := aGtWireEncoderContext nextSize.
	^ OrderedCollection new: count streamContents: [ :arrayStream |
		count timesRepeat:
			[ arrayStream nextPut: aGtWireEncoderContext next ] ]
]
