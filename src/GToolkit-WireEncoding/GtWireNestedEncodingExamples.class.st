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
GtWireNestedEncodingExamples >> defaultMaxDepth [
	<gtExample>
	| array currentArray encoder byteArray next |

	encoder := GtWireEncoder onByteArray.
	encoder 
		maxDepthEncoder: GtWireDummyProxyEncoder new;
		remainingDepth: 4.
	array := Array new: 2.
	currentArray := array.
	1 to: 10 do: [ :i |
		currentArray
			at: 1 put: i;
			at: 2 put: (Array new: 2).
		currentArray := currentArray second ].
	encoder nextPut: array.
	byteArray := encoder contents.
	self assert: byteArray size equals: 60.
	next := (GtWireDecoder on: byteArray readStream) next.
	self assert: next class equals: Array.
	self assert: next first equals: 1.
	next := next second.
	self assert: next class equals: Array.
	self assert: next first equals: 2.
	next := next second second.
	self assert: next size equals: 2.
	self assert: (next allSatisfy: [ :each |
		each class = GtWireEncodingDummyProxy ]).
	^ byteArray
]

{ #category : #examples }
GtWireNestedEncodingExamples >> maxDepth2 [
	<gtExample>
	| array currentArray encoder byteArray next |

	encoder := GtWireEncoder onByteArray.
	array := Array new: 2.
	currentArray := array.
	1 to: 5 do: [ :i |
		currentArray
			at: 1 put: i;
			at: 2 put: (Array new: 2).
		currentArray := currentArray second ].
	encoder addMapping: Array to: 
		(GtWireMaxDepthEncoder
			depth: 2
			encoder: GtWireArrayEncoder new).
	encoder nextPut: array.
	byteArray := encoder contents.
	self assert: byteArray size equals: 12.
	next := (GtWireDecoder on: byteArray readStream) next.
	self assert: next class equals: Array.
	self assert: next first equals: 1.
	next := next second.
	self assert: next class equals: Array.
	self assert: next first equals: 2.
	next := next second.
	self assert: next equals: (Array new: 2).
	^ byteArray
]

{ #category : #examples }
GtWireNestedEncodingExamples >> minDepth2 [
	<gtExample>
	| array currentArray encoder byteArray next |

	encoder := GtWireEncoder onByteArray.
	array := Array new: 2.
	currentArray := array.
	1 to: 5 do: [ :i |
		currentArray
			at: 1 put: i;
			at: 2 put: (Array new: 2).
		currentArray := currentArray second ].
	encoder addMapping: Array to: 
		(GtWireMinDepthEncoder
			depth: 2
			encoder: GtWireArrayEncoder new).
	encoder 
		remainingDepth: 2;
		nextPut: array.
	byteArray := encoder contents.
	self assert: byteArray size equals: 24.
	next := (GtWireDecoder on: byteArray readStream) next.
	self assert: next class equals: Array.
	self assert: next = array.
	^ byteArray
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
