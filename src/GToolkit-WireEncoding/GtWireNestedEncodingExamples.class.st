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
	<return: #ByteArray>
	| array currentArray encoder byteArray next |
	encoder := GtWireEncoder onByteArray.
	encoder
		maxDepthEncoder: GtWireDummyProxyEncoder new;
		remainingDepth: 4.
	array := Array new: 2.
	currentArray := array.
	1
		to: 10
		do: [ :i | 
			currentArray
				at: 1 put: i;
				at: 2 put: (Array new: 2).
			currentArray := currentArray second ].
	encoder nextPut: array.
	byteArray := encoder contents.	"self assert: byteArray size equals: 57."
	next := (GtWireDecoder on: byteArray readStream) next.
	self assert: next class equals: Array.
	self assert: next first equals: 1.
	next := next second.
	self assert: next class equals: Array.
	self assert: next first equals: 2.
	next := next second second.
	self assert: next size equals: 2.
	self assert: next first equals: 4.
	self assert: next second class = GtWireEncodingDummyProxy.
	^ byteArray
]

{ #category : #examples }
GtWireNestedEncodingExamples >> maxDepth2 [
	<gtExample>
	<return: #ByteArray>
	| array currentArray encoder byteArray next object |
	encoder := GtWireEncoder onByteArray
			maxDepthEncoder: GtWireDummyProxyEncoder new.
	array := Array new: 2.
	currentArray := array.
	1
		to: 10
		do: [ :i | 
			currentArray
				at: 1 put: i;
				at: 2 put: (Array new: 2).
			currentArray := currentArray second ].
	object := GtWireEncodingExampleInstVarObject new var1: array.
	encoder
		addMapping: Array
		to: (GtWireMaxDepthEncoder depth: 2 encoder: GtWireArrayEncoder new).
	encoder nextPut: object.
	byteArray := encoder contents.	"self assert: byteArray size equals: 24."
	next := (GtWireDecoder on: byteArray readStream) next.
	self assert: next class equals: GtWireEncodingExampleInstVarObject.
	next := next var1.
	self assert: next class equals: Array.
	self assert: next first equals: 1.
	next := next second.
	self assert: next first equals: 2.
	self assert: next second class equals: GtWireEncodingDummyProxy.
	self
		assert: next second description
		equals: '#(3 #(4 #(5 #(6 #(7 #(8 #(9 #(10 #(nil nil)))))))))'.
	^ byteArray
]

{ #category : #examples }
GtWireNestedEncodingExamples >> maxDepth2RootObject [
	<gtExample>
	<return: #ByteArray>
	| array currentArray encoder byteArray next |
	encoder := GtWireEncoder onByteArray.
	array := Array new: 2.
	currentArray := array.
	1
		to: 10
		do: [ :i | 
			currentArray
				at: 1 put: i;
				at: 2 put: (Array new: 2).
			currentArray := currentArray second ].
	encoder
		addMapping: Array
		to: (GtWireMaxDepthEncoder depth: 2 encoder: GtWireArrayEncoder new).
	encoder nextPut: array.
	byteArray := encoder contents.	"self assert: byteArray size equals: 24."
	next := (GtWireDecoder on: byteArray readStream) next.
	self assert: next class equals: Array.
	self assert: next first equals: 1.
	next := next second.
	self assert: next equals: #(2 nil).
	^ byteArray
]

{ #category : #examples }
GtWireNestedEncodingExamples >> minDepth2 [
	<gtExample>
	<return: #ByteArray>
	| array currentArray encoder byteArray next |
	encoder := GtWireEncoder onByteArray.
	array := Array new: 2.
	currentArray := array.
	1
		to: 5
		do: [ :i | 
			currentArray
				at: 1 put: i;
				at: 2 put: (Array new: 2).
			currentArray := currentArray second ].
	encoder
		addMapping: Array
		to: (GtWireMinDepthEncoder depth: 2 encoder: GtWireArrayEncoder new).
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
GtWireNestedEncodingExamples >> proxyScaledDecimal [
	"Configure the encoding to always return scaled decimals as proxies
	(as opposed to the general GtWireObjectByNameEncoder serialisation)."

	<gtExample>
	<return: #ByteArray>
	| array currentArray encoder byteArray next |
	encoder := GtWireEncoder onByteArray.
	encoder map at: ScaledDecimal put: GtWireDummyProxyEncoder new.
	array := Array new: 2.
	currentArray := array.
	1
		to: 5
		do: [ :i | 
			currentArray
				at: 1 put: i;
				at: 2 put: (Array new: 2).
			i = 3 ifTrue: [ currentArray at: 1 put: 1.25 asScaledDecimal ].
			currentArray := currentArray second ].
	encoder nextPut: array.
	byteArray := encoder contents.
	self assert: byteArray size equals: 43.
	next := (GtWireDecoder on: byteArray readStream) next.
	self assert: next class equals: Array.
	self assert: next first equals: 1.
	next := next second.
	self assert: next class equals: Array.
	self assert: next first equals: 2.
	next := next second.
	self assert: next size equals: 2.
	self assert: next first class equals: GtWireEncodingDummyProxy.
	self assert: next first description equals: '1.25000000000000s14'.
	self assert: next second class equals: Array.
	^ byteArray
]

{ #category : #examples }
GtWireNestedEncodingExamples >> stonEncoding [
	<gtExample>
	<return: #GtWireNestedEncodingExamples>
	| object encoder decoder byteArray next |
	"signals := CircularMemoryLogger new startFor: GtWireEncodingSignal."
	encoder := GtWireEncoder onByteArray.
	encoder
		addMapping: GtWireEncodingExampleInstVarObject
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
	self assert: next = object
]
