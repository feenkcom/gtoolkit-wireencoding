"
GtWireInstVarEncoder uses the specified mapping to encode the supplied objects.  No mapping is required for decoding.
"
Class {
	#name : #GtWireInstVarEncoder,
	#superclass : #GtWireObjectEncoder,
	#instVars : [
		'instVarMap'
	],
	#category : #'GToolkit-WireEncoding'
}

{ #category : #'as yet unclassified' }
GtWireInstVarEncoder class >> typeIdentifier [

	^ 20
]

{ #category : #'as yet unclassified' }
GtWireInstVarEncoder >> decodeWith: aGtWireEncoderContext [
	| count instance className |

	count := aGtWireEncoderContext nextSize.
	className := aGtWireEncoderContext next.
	instance := (self class environment classOrTraitNamed: className) basicNew.
	count timesRepeat:
		[ | instVarName |
		instVarName := aGtWireEncoderContext next.
		instance
			instVarNamed: instVarName
			put: aGtWireEncoderContext next ].
	^ instance
]

{ #category : #'as yet unclassified' }
GtWireInstVarEncoder >> encode: anObject with: aGtWireEncoderContext [

	aGtWireEncoderContext
		putTypeIdentifier: self class typeIdentifier;
		putSize: instVarMap size.
	aGtWireEncoderContext nextPut: anObject class name.
	instVarMap keysAndValuesDo: [ :key :value |
		aGtWireEncoderContext nextPut: key.
		value
			ifNil: [ aGtWireEncoderContext nextPut: (anObject instVarNamed: key) ]
			ifNotNil: [ value encode: (anObject instVarNamed: key) with: aGtWireEncoderContext ] ].
]

{ #category : #accessing }
GtWireInstVarEncoder >> instVarMap [
	^ instVarMap
]

{ #category : #accessing }
GtWireInstVarEncoder >> instVarMap: anObject [
	instVarMap := anObject
]
