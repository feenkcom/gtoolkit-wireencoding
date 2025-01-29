Class {
	#name : #GtWireSetEncoder,
	#superclass : #GtWireCollectionEncoder,
	#category : #'GToolkit-WireEncoding'
}

{ #category : #'encoding - decoding' }
GtWireSetEncoder class >> decodeWith: aGtWireEncoderContext [
	"Decode the OrderedCollection on the supplied context"
	| count set |

	count := aGtWireEncoderContext stream packedInteger.
	set := Set new: count * 2.
	count timesRepeat:
		[ set add: aGtWireEncoderContext next ].
	^ set
]

{ #category : #accessing }
GtWireSetEncoder class >> typeIdentifier [

	^ 11
]
