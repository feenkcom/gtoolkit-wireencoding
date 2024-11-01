Class {
	#name : #GtWireObjectEncoder,
	#superclass : #Object,
	#classVars : [
		'DefaultMap'
	],
	#category : #'GToolkit-WireEncoding-ObjectEncoder'
}

{ #category : #accessing }
GtWireObjectEncoder class >> cleanUp [

	DefaultMap := nil
]

{ #category : #'encoding - decoding' }
GtWireObjectEncoder class >> decodeWith: aGtWireEncoderContext [
	
	^ self subclassResponsibility
]

{ #category : #accessing }
GtWireObjectEncoder class >> defaultMap [

	^ DefaultMap ifNil: [ DefaultMap := self newDefaultMap ]
]

{ #category : #'encoding - decoding' }
GtWireObjectEncoder class >> encode: anObject with: aGtWireEncoderContext [

	aGtWireEncoderContext stream
		packedInteger: self typeIdentifier
]

{ #category : #accessing }
GtWireObjectEncoder class >> newDefaultMap [

	^ Dictionary new
		yourself
]

{ #category : #accessing }
GtWireObjectEncoder class >> typeIdentifier [

	^ self subclassResponsibility
]
