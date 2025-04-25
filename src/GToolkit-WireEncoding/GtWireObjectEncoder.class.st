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

{ #category : #testing }
GtWireObjectEncoder >> isProxyObjectEncoder [
	"Answer a boolean indicating whether the receiver is a type of proxy encoder.
	Proxy encoding is platform dependent."

	^ false.
]

{ #category : #'as yet unclassified' }
GtWireObjectEncoder >> name [

	^ self class name
]

{ #category : #'private - helpers' }
GtWireObjectEncoder >> replaceMappingsMatching: matchBlock with: replacementBlock [
	"Replace all the mappings matching matchBlock with the encoder returned by replacementBlock.
	Used by examples to avoid needing live servers.
	Overwritten by encoders as required."
]

{ #category : #accessing }
GtWireObjectEncoder >> typeIdentifier [

	^ self class typeIdentifier
]
