Class {
	#name : #GtWireSetEncoder,
	#superclass : #GtWireCollectionEncoder,
	#category : #'GToolkit-WireEncoding'
}

{ #category : #accessing }
GtWireSetEncoder class >> typeIdentifier [

	^ 11
]

{ #category : #'encoding - decoding' }
GtWireSetEncoder >> decodeWith: aGtWireEncoderContext [
	"Decode the OrderedCollection on the supplied context"
	| count set |

	count := aGtWireEncoderContext nextSize.
	set := Set new: count * 2.
	count timesRepeat:
		[ set add: aGtWireEncoderContext next ].
	^ set
]
