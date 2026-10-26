"
IdentityBag wire encoding. Each distinct identity is transmitted once with its multiplicity. Inherits the pair format from GtWireBagEncoder; GemStone IdentityBag is not a Bag and its counts are collected in an IdentityDictionary. As with other wire encoders, global shared-reference identity is not preserved.
"
Class {
	#name : #GtWireIdentityBagEncoder,
	#superclass : #GtWireBagEncoder,
	#category : #'GToolkit-WireEncoding'
}

{ #category : #accessing }
GtWireIdentityBagEncoder class >> typeIdentifier [
	^ 35
]

{ #category : #accessing }
GtWireIdentityBagEncoder >> bagClass [
	^ IdentityBag
]

{ #category : #encoding }
GtWireIdentityBagEncoder >> countsFor: aBag [
	"GemStone IdentityBag has no dictionary-backed count iteration. Never coalesce equal but non-identical entries."
	^ self
		gtDo: [ aBag valuesAndCounts ]
		gemstoneDo: [ | counts |
			counts := IdentityDictionary new.
			aBag do: [ :object |
				counts at: object put: (counts at: object ifAbsent: [ 0 ]) + 1 ].
			counts ]
]
