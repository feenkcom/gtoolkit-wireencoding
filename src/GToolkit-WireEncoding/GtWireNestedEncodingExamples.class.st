Class {
	#name : #GtWireNestedEncodingExamples,
	#superclass : #Object,
	#instVars : [
		'signals'
	],
	#category : #'GToolkit-WireEncoding'
}

{ #category : #private }
GtWireNestedEncodingExamples >> cleanUp [

	signals ifNotNil: [ signals stop ].
]

{ #category : #examples }
GtWireNestedEncodingExamples >> stonEncoding [
	<gtExample>
	| object encoder decoder byteArray next |

	"signals := CircularMemoryLogger new startFor: GtWireEncodingSignal."
	encoder := GtWireEncoder onByteArray.
	encoder addMapping: GtWireEncodingExampleInstVarObject
		to: GtWireStonEncoder new.
	object := GtWireEncodingExampleInstVarObject new.
	object
		var1: 1;
		var2: 'two'.
	encoder nextPut: object.
	byteArray := encoder contents.
	self assert: byteArray size equals: 57.
	decoder := GtWireDecoder on: byteArray readStream.
	decoder 
		map: encoder map;
		reverseMap: encoder reverseMap.
	next := decoder next.
	self assert: next class equals: GtWireEncodingExampleInstVarObject.
	self assert: next = object.
]
