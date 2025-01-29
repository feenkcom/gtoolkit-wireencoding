Class {
	#name : #GtWireDictionaryEncoder,
	#superclass : #GtWireCollectionEncoder,
	#category : #'GToolkit-WireEncoding'
}

{ #category : #'encoding - decoding' }
GtWireDictionaryEncoder class >> decodeWith: aGtWireEncoderContext [
	"Decode the dictionary on the supplied context"
	| count dictionary |

	count := aGtWireEncoderContext stream packedInteger.
	dictionary := Dictionary new: count * 2.
	count timesRepeat:
		[ dictionary
			at: aGtWireEncoderContext next
			put: aGtWireEncoderContext next ].
	^ dictionary
]

{ #category : #'encoding - decoding' }
GtWireDictionaryEncoder class >> encode: aDictionary with: aGtWireEncoderContext [

	aGtWireEncoderContext stream
		packedInteger: self typeIdentifier;
		packedInteger: aDictionary size.
	aDictionary associationsDo: [ :each |
		aGtWireEncoderContext 
			nextPut: each key;
			nextPut: each value ].
]

{ #category : #accessing }
GtWireDictionaryEncoder class >> typeIdentifier [

	^ 9
]
