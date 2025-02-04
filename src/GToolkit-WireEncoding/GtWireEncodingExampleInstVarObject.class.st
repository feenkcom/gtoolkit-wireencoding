Class {
	#name : #GtWireEncodingExampleInstVarObject,
	#superclass : #Object,
	#instVars : [
		'var1',
		'var2',
		'var3',
		'var4'
	],
	#category : #'GToolkit-WireEncoding'
}

{ #category : #accessing }
GtWireEncodingExampleInstVarObject class >> leJsonV4Name [

	^  #gtWireEncodingExampleInstVarObject
]

{ #category : #'as yet unclassified' }
GtWireEncodingExampleInstVarObject >> = anObject [

	self == anObject ifTrue: [ ^ true ].
	anObject class = self class ifFalse: [ ^ false ].
	^ anObject var1 = var1 and:
		[ anObject var2 = var2 and:
		[ anObject var3 = var3 and:
		[ anObject var4 = var4 ] ] ]
]

{ #category : #'as yet unclassified' }
GtWireEncodingExampleInstVarObject >> hash [

	^ var1 hash bitXor:
		(var2 hash bitXor:
		(var3 hash bitXor:
		var4 hash))
]

{ #category : #accessing }
GtWireEncodingExampleInstVarObject >> var1 [
	^ var1
]

{ #category : #accessing }
GtWireEncodingExampleInstVarObject >> var1: anObject [
	var1 := anObject
]

{ #category : #accessing }
GtWireEncodingExampleInstVarObject >> var2 [
	^ var2
]

{ #category : #accessing }
GtWireEncodingExampleInstVarObject >> var2: anObject [
	var2 := anObject
]

{ #category : #accessing }
GtWireEncodingExampleInstVarObject >> var3 [
	^ var3
]

{ #category : #accessing }
GtWireEncodingExampleInstVarObject >> var3: anObject [
	var3 := anObject
]

{ #category : #accessing }
GtWireEncodingExampleInstVarObject >> var4 [
	^ var4
]

{ #category : #accessing }
GtWireEncodingExampleInstVarObject >> var4: anObject [
	var4 := anObject
]
