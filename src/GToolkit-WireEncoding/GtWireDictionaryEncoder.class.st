Class {
	#name : #GtWireDictionaryEncoder,
	#superclass : #GtWireCollectionEncoder,
	#category : #'GToolkit-WireEncoding'
}

{ #category : #accessing }
GtWireDictionaryEncoder class >> typeIdentifier [

	^ 9
]

{ #category : #'encoding - decoding' }
GtWireDictionaryEncoder >> decodeWith: aGtWireEncoderContext [
	"Decode the dictionary on the supplied context"
	| count dictionary |

	count := aGtWireEncoderContext nextSize.
	dictionary := Dictionary new: count * 2.
	count timesRepeat:
		[ dictionary
			at: aGtWireEncoderContext next
			put: aGtWireEncoderContext next ].
	^ dictionary
]

{ #category : #'encoding - decoding' }
GtWireDictionaryEncoder >> encode: aDictionary with: aGtWireEncoderContext [

	aGtWireEncoderContext
		putTypeIdentifier: self typeIdentifier;
		putSize: aDictionary size.
	aDictionary associationsDo: [ :each |
		aGtWireEncoderContext 
			nextPut: each key;
			nextPut: each value ].
]
