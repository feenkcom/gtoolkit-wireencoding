Class {
	#name : #GtWireUnsupportedObject,
	#superclass : #Error,
	#instVars : [
		'object'
	],
	#category : #'GToolkit-WireEncoding'
}

{ #category : #signaling }
GtWireUnsupportedObject class >> signal: anObject [

	self new
		object: anObject;
		messageText: 'Unable to serialize', anObject class name;
		signal
]

{ #category : #accessing }
GtWireUnsupportedObject >> object [
	^ object
]

{ #category : #accessing }
GtWireUnsupportedObject >> object: anObject [
	object := anObject
]
