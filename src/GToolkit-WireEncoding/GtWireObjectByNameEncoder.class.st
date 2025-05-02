Class {
	#name : #GtWireObjectByNameEncoder,
	#superclass : #GtWireObjectEncoder,
	#category : #'GToolkit-WireEncoding'
}

{ #category : #'instance creation' }
GtWireObjectByNameEncoder class >> typeIdentifier [

	^ 22
]

{ #category : #'encoding - decoding' }
GtWireObjectByNameEncoder >> decodeWith: aGtWireEncoderContext [
	| count instance className cls |

	className := aGtWireEncoderContext nextString.
	"Retrieve the number of variable slots"
	count := aGtWireEncoderContext nextSize.
	cls := self lookupClass: className.
	cls ifNil: [ self error: 'Unknown class: ', className asString ].
	instance := cls isVariable
		ifTrue: [ cls basicNew: count ]
		ifFalse: [ cls basicNew ].
	1 to: count do: [ :i |
		instance basicAt: i put: aGtWireEncoderContext next ].
	count := aGtWireEncoderContext nextSize.
	count timesRepeat:
		[ | instVarName |
		instVarName := aGtWireEncoderContext next.
		instance
			instVarNamed: instVarName
			put: aGtWireEncoderContext next ].
	^ instance
]

{ #category : #'encoding - decoding' }
GtWireObjectByNameEncoder >> encode: anObject with: aGtWireEncoderContext [
	| instVarNames namesAndValues |

	(anObject isKindOf: RsrService) ifTrue:
		[ self error: 'Attempt to encode proxy by name' ].
	instVarNames := anObject class allInstVarNames.
	namesAndValues := OrderedCollection new.
	instVarNames do: [ :name |
		(anObject instVarNamed: name) ifNotNil: [ :value |
			namesAndValues add: name -> value ] ].
	aGtWireEncoderContext
		putTypeIdentifier: self class typeIdentifier;
		putString: anObject class name.
	anObject class isVariable ifTrue:
		[ | basicSize |
		basicSize := anObject basicSize.
		aGtWireEncoderContext putSize: basicSize.
		1 to: basicSize do: [ :i |
			aGtWireEncoderContext nextPut: (anObject basicAt: i) ] ]
	ifFalse:
		[ aGtWireEncoderContext putSize: 0 ].
	aGtWireEncoderContext putSize: namesAndValues size.
	namesAndValues do: [ :assoc |
		aGtWireEncoderContext nextPut: assoc key.
		aGtWireEncoderContext nextPut: assoc value ].
]

{ #category : #private }
GtWireObjectByNameEncoder >> lookupClass: className [
	"Answer the class with the supplied name or nil if not found.
	For GemStone, see STONReader>>lookupClass: for inspiration."

	^ self
		gtDo: [ self class environment classOrTraitNamed: className ]
		gemstoneDo: [ System myUserProfile objectNamed: className asSymbol ]
]
