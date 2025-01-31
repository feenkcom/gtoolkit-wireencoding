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

	className := aGtWireEncoderContext next.
	"Retrieve the number of variable slots"
	count := aGtWireEncoderContext stream packedInteger.
	cls := self class environment classOrTraitNamed: className.
	cls ifNil: [ self error: 'Unknown class: ', className asString ].
	instance := cls isVariable
		ifTrue: [ cls basicNew: count ]
		ifFalse: [ cls basicNew ].
	1 to: count do: [ :i |
		instance basicAt: i put: aGtWireEncoderContext next ].
	count := aGtWireEncoderContext stream packedInteger.
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

	instVarNames := anObject class allInstVarNames.
	namesAndValues := OrderedCollection new.
	instVarNames do: [ :name |
		(anObject instVarNamed: name) ifNotNil: [ :value |
			namesAndValues add: name -> value ] ].
	aGtWireEncoderContext
		putTypeIdentifier: self class typeIdentifier.
	aGtWireEncoderContext nextPut: anObject class name.
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
