"
Portable bag encoding: type identifier, number of distinct entries (wire size), then ordinary wire object/count pairs. Does not transmit platform-specific storage. GemStone bags cannot represent nil elements.
"
Class {
	#name : #GtWireBagEncoder,
	#superclass : #GtWireObjectEncoder,
	#category : #'GToolkit-WireEncoding'
}

{ #category : #accessing }
GtWireBagEncoder class >> typeIdentifier [
	^ 34
]

{ #category : #accessing }
GtWireBagEncoder >> bagClass [
	^ Bag
]

{ #category : #encoding }
GtWireBagEncoder >> countsFor: aBag [
	^ self
		gtDo: [ aBag valuesAndCounts ]
		gemstoneDo: [ | counts |
			counts := Dictionary new.
			aBag _keysAndValuesDo: [ :object :count | counts at: object put: count ].
			counts ]
]

{ #category : #decoding }
GtWireBagEncoder >> decodeWith: aContext [
	| entryCount bag |
	entryCount := aContext nextSize.
	bag := self bagClass new: entryCount.
	entryCount timesRepeat: [ | object count |
		object := aContext next.
		count := aContext next.
		(count isInteger and: [ count > 0 ])
			ifFalse: [ self error: 'Bag occurrence count must be a positive integer' ].
		bag add: object withOccurrences: count ].
	^ bag
]

{ #category : #encoding }
GtWireBagEncoder >> encode: aBag with: aContext [
	"Transmit distinct objects and multiplicities, never the internal bag representation."
	| counts |
	counts := self countsFor: aBag.
	aContext
		putTypeIdentifier: self typeIdentifier;
		putSize: counts size.
	counts keysAndValuesDo: [ :object :count |
		aContext nextPut: object; nextPut: count ].
]
