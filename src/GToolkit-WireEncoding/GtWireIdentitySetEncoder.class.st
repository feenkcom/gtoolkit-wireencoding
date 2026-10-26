"
Portable identity-set encoding. Uses the same count/elements format as GtWireSetEncoder with a separate type identifier. Reconstructs through IdentitySet's public API; does not preserve shared references between separately encoded elements.
"
Class {
	#name : #GtWireIdentitySetEncoder,
	#superclass : #GtWireObjectEncoder,
	#category : #'GToolkit-WireEncoding'
}

{ #category : #accessing }
GtWireIdentitySetEncoder class >> typeIdentifier [
	^ 36
]

{ #category : #encoding }
GtWireIdentitySetEncoder >> decodeWith: aContext [
	"IdentitySet is not a subclass of Set on GemStone. Use its public API."
	| count set |
	count := aContext nextSize.
	set := IdentitySet new: count.
	count timesRepeat: [ set add: aContext next ].
	^ set
]

{ #category : #encoding }
GtWireIdentitySetEncoder >> encode: aSet with: aContext [
	"Transmit logical elements rather than platform-specific identity-set storage."
	aContext putTypeIdentifier: self typeIdentifier; putSize: aSet size.
	aSet do: [ :each | aContext nextPut: each ].
]
