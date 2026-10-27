Class {
	#name : #GtWireSetEncoder,
	#superclass : #GtWireObjectEncoder,
	#category : #'GToolkit-WireEncoding'
}

{ #category : #accessing }
GtWireSetEncoder class >> typeIdentifier [

	^ 11
]

{ #category : #encoding }
GtWireSetEncoder >> decodeWith: aContext [
	"Rebuild hashing on the receiving platform through the public collection API."
	| count set |
	count := aContext nextSize.
	set := Set new: count.
	count timesRepeat: [ set add: aContext next ].
	^ set
]

{ #category : #encoding }
GtWireSetEncoder >> encode: aSet with: aContext [
	"Use logical elements, never the platform-specific backing storage.
	Keep the existing Set wire format: size followed by ordinary wire objects."
	aContext putTypeIdentifier: self typeIdentifier; putSize: aSet size.
	aSet do: [ :each | aContext nextPut: each ].
]
