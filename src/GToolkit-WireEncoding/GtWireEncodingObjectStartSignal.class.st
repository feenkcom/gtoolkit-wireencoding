Class {
	#name : #GtWireEncodingObjectStartSignal,
	#superclass : #GtWireEncodingSignal,
	#instVars : [
		'object'
	],
	#category : #'GToolkit-WireEncoding'
}

{ #category : #accessing }
GtWireEncodingObjectStartSignal >> object [
	^ object
]

{ #category : #accessing }
GtWireEncodingObjectStartSignal >> object: anObject [
	object := anObject
]
