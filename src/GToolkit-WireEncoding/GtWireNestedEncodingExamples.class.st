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

{ #category : #'as yet unclassified' }
GtWireNestedEncodingExamples >> leJsonEncoding [
	<gtExample>
	| object encoder byteArray next decoder |

	signals := CircularMemoryLogger new startFor: GtWireEncodingSignal.
	encoder := GtWireEncoder onByteArray.
	encoder addMapping: GtWireEncodingExampleInstVarObject
		to: GtWireLeJsonV4Encoder new.
	object := GtWireEncodingExampleInstVarObject new.
	object
		var1: 1;
		var2: 'two'.
	encoder nextPut: object.
	byteArray := encoder contents.
	self assert: byteArray size equals: 88.
	decoder := GtWireDecoder on: byteArray readStream.
	decoder 
		map: encoder map;
		reverseMap: encoder reverseMap.
	next := decoder next.
	self assert: next class equals: GtWireEncodingExampleInstVarObject.
	self assert: next = object.
]

{ #category : #'as yet unclassified' }
GtWireNestedEncodingExamples >> mixedEncoding [
	<gtExample>
	<after: #cleanUp>
	| object mixedMapping encoder decoder byteArray next |

	signals := CircularMemoryLogger new startFor: GtWireEncodingSignal.
	encoder := GtWireEncoder onByteArray.
	mixedMapping := GtWireInstVarEncoder new.
	mixedMapping instVarMap: {
			#var1 -> nil.
			#var2 -> GtWireLeJsonV4Encoder new.
			#var3 -> GtWireStonEncoder new.
		} asDictionary.
	encoder addMapping: GtWireEncodingExampleInstVarObject
		to: mixedMapping.
	object := GtWireEncodingExampleInstVarObject new.
	object
		var1: #[1 2 3 4];
		var2: #(4 3 2 1);
		var3: 'two';
		var4: 'missing'.
	encoder nextPut: object.
	byteArray := encoder contents.
	self assert: byteArray size equals: 80.
	decoder := encoder decoderOn: byteArray readStream.
	next := decoder next.
	self assert: next class equals: GtWireEncodingExampleInstVarObject.
	self assert: next var1 = object var1.
	self assert: next var2 = object var2.
	self assert: next var3 = object var3.
	self assert: next var4 isNil.
]

{ #category : #'as yet unclassified' }
GtWireNestedEncodingExamples >> stonEncoding [
	<gtExample>
	| object encoder decoder byteArray next |

	signals := CircularMemoryLogger new startFor: GtWireEncodingSignal.
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
