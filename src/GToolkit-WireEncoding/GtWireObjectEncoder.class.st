Class {
	#name : #GtWireObjectEncoder,
	#superclass : #Object,
	#classVars : [
		'DefaultMap'
	],
	#category : #'GToolkit-WireEncoding'
}

{ #category : #accessing }
GtWireObjectEncoder class >> typeIdentifier [

	^ self subclassResponsibility
]

{ #category : #accessing }
GtWireObjectEncoder class >> typeIdentifierOrNil [

	^ [ self typeIdentifier ]
		on: Error
		do: [ :ex | 
			self gtDo: [ ex class name = #SubclassResponsibility ifFalse: [ ex pass ] ]
				gemstoneDo: [].
			nil ]
]

{ #category : #test }
GtWireObjectEncoder class >> validateTypeIdentifiers [
	 "Validate that there aren't duplicate type identifiers and answer the maximum value"
	| visited |
	
	visited := Set new.
	self allSubclassesDo: [ :cls | | typeIdentifier |
		typeIdentifier := cls typeIdentifierOrNil.
		typeIdentifier ifNotNil:
			[ (visited includes: typeIdentifier) ifTrue:
				[ self error: 'Duplicate typeIdentifier found' ].
			visited add: typeIdentifier ] ].
	^ visited max
]

{ #category : #'encoding - decoding' }
GtWireObjectEncoder >> decodeWith: aGtWireEncoderContext [
	
	^ self subclassResponsibility
]

{ #category : #'encoding - decoding' }
GtWireObjectEncoder >> encode: anObject with: aGtWireEncoderContext [

	aGtWireEncoderContext putTypeIdentifier: self class typeIdentifier
]

{ #category : #'as yet unclassified' }
GtWireObjectEncoder >> name [

	^ self class name
]

{ #category : #'as yet unclassified' }
GtWireObjectEncoder >> typeIdentifier [

	^ self class typeIdentifier
]
