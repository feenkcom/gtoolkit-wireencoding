Class {
	#name : #GtWireEncodingExamples,
	#superclass : #Object,
	#category : 'GToolkit-WireEncoding'
}

{ #category : #examples }
GtWireEncodingExamples >> array [
	<gtExample>
	| array encoder byteArray next |

	encoder := GtWireEncoder onByteArray.
	array := { 1. 'hello'. #hello. }.
	encoder nextPut: array.
	byteArray := encoder contents.
	self assert: byteArray size equals: 18.
	next := (GtWireDecoder on: byteArray readStream) next.
	self assert: next class equals: Array.
	self assert: next = array.
	^ byteArray
]

{ #category : #examples }
GtWireEncodingExamples >> association [
	<gtExample>
	| association encoder byteArray next |

	encoder := GtWireEncoder onByteArray.
	association := 1 -> 'one'.
	encoder nextPut: association.
	byteArray := encoder contents.
	self assert: byteArray size equals: 8.
	next := (GtWireDecoder on: byteArray readStream) next.
	self assert: next class equals: Association.
	self assert: next = association.
]

{ #category : #examples }
GtWireEncodingExamples >> blockClosure [
	<gtExample>
	| blockClosure encoder byteArray next |

	encoder := GtWireEncoder onByteArray.
	blockClosure := [ :a :b | a + b ].
	encoder nextPut: blockClosure.
	byteArray := encoder contents.
	self assert: byteArray size equals: 20.
	next := (GtWireDecoder on: byteArray readStream) next.
	self assert: (#(FullBlockClosure ExecBlock) includes: next class name).
	self assert: (next value: 4 value: 3) equals: 7.
	^ byteArray
]

{ #category : #examples }
GtWireEncodingExamples >> boolean [
	<gtExample>
	| encoder byteArray decoder |

	encoder := GtWireEncoder onByteArray.
	encoder nextPut: true.
	encoder nextPut: false.
	byteArray := encoder contents.
	self assert: byteArray size equals: 2.
	decoder := GtWireDecoder on: byteArray readStream.
	self assert: decoder next.
	self assert: decoder next not.
]

{ #category : #examples }
GtWireEncodingExamples >> byteArray [
	<gtExample>
	| source encoder byteArray next |

	encoder := GtWireEncoder onByteArray.
	source := #[3 1 4 1 5].
	encoder nextPut: source.
	byteArray := encoder contents.
	self assert: byteArray size equals: source size + 2.
	next := (GtWireDecoder on: byteArray readStream) next.
	self assert: next class equals: ByteArray.
	self assert: next equals: source.
]

{ #category : #examples }
GtWireEncodingExamples >> byteString [
	<gtExample>
	| string encoder byteArray next |

	encoder := GtWireEncoder onByteArray.
	string := 'Hello, World'.
	encoder nextPut: string.
	byteArray := encoder contents.
	self assert: byteArray size equals: string size + 2.
	next := (GtWireDecoder on: byteArray readStream) next.
	self assert: (#(ByteString String) includes: next class name).
	self assert: next equals: string.
	^ byteArray
]

{ #category : #examples }
GtWireEncodingExamples >> byteStringWithNull [
	<gtExample>
	| string encoder byteArray next |

	encoder := GtWireEncoder onByteArray.
	string := 'abc', (String with: Character null), 'def'.
	encoder nextPut: string.
	byteArray := encoder contents.
	self assert: byteArray size equals: string size + 2.
	next := (GtWireDecoder on: byteArray readStream) next.
	self assert: (#(ByteString String) includes: next class name).
	self assert: next equals: string.
	^ byteArray
]

{ #category : #examples }
GtWireEncodingExamples >> byteSymbol [
	<gtExample>
	| string encoder byteArray next |

	encoder := GtWireEncoder onByteArray.
	string := #'Hello, World'.
	encoder nextPut: string.
	byteArray := encoder contents.
	self assert: byteArray size equals: string size + 2.
	next := (GtWireDecoder on: byteArray readStream) next.
	"Allow for differences in GT & GS class hierarchy"
	self assert: (#(ByteSymbol Symbol) includes: next class name).
	self assert: next equals: string.
	^ byteArray
]

{ #category : #examples }
GtWireEncodingExamples >> character [
	<gtExample>
	| encoder byteArray next |

	encoder := GtWireEncoder onByteArray.
	encoder nextPut: $§.
	byteArray := encoder contents.
	self assert: byteArray size equals: 3.
	next := (GtWireDecoder on: byteArray readStream) next.
	self assert: next class equals: Character.
	self assert: next == $§.
]

{ #category : #examples }
GtWireEncodingExamples >> dateAndTime [
	<gtExample>
	| dateAndTime encoder byteArray next |

	encoder := GtWireEncoder onByteArray.
	dateAndTime := DateAndTime now.
	encoder nextPut: dateAndTime.
	byteArray := encoder contents.
	self assert: (byteArray size between: 10 and: 25).
	next := (GtWireDecoder on: byteArray readStream) next.
	self assert: (next isKindOf: DateAndTime).
	self assert: next = dateAndTime.
]

{ #category : #examples }
GtWireEncodingExamples >> deepArray [
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
	encoder nextPut: array.
	byteArray := encoder contents.
	self assert: byteArray size equals: 24.
	next := (GtWireDecoder on: byteArray readStream) next.
	self assert: next class equals: Array.
	self assert: next = array.
	^ byteArray
]

{ #category : #examples }
GtWireEncodingExamples >> dictionary [
	<gtExample>
	| dictionary encoder byteArray next |

	encoder := GtWireEncoder onByteArray.
	dictionary := {
		 1 -> 'one'.
		 2 -> 'two' } asDictionary.
	encoder nextPut: dictionary.
	byteArray := encoder contents.
	self assert: byteArray size equals: 16.
	next := (GtWireDecoder on: byteArray readStream) next.
	self assert: next class equals: Dictionary.
	self assert: next = dictionary.
]

{ #category : #examples }
GtWireEncodingExamples >> float [
	<gtExample>
	| encoder byteArray next |

	encoder := GtWireEncoder onByteArray.
	{ Float fmin. Float fmax. 1.25. } doWithIndex: [ :f :i |
		encoder reset.
		encoder nextPut: f.
		byteArray := encoder contents.
		self assert: byteArray size equals: 9.
		next := (GtWireDecoder on: byteArray readStream) next.
		self assert: next equals: f ].
]

{ #category : #examples }
GtWireEncodingExamples >> generalObject [
	<gtExample>
	| object encoder byteArray next root |

	encoder := GtWireEncoder onByteArray.
	object := self gtDo:[ (self class environment classOrTraitNamed: #AdditionalMethodState) new: 3]
		gemstoneDo: [ ^ self ].
	object
		selector: #one;
		method: 'fake'.
	1 to: 3 do: [ :i |
		object basicAt: i put: 2 ** i ].
	encoder nextPut: object.
	byteArray := encoder contents.
	self assert: byteArray size equals: 60.
	root := GtWireInspectionDecoder byteArray: byteArray.
	next := root object.
	self assert: (#(AdditionalMethodState) includes: next class name).
	self assert: next basicSize equals: 3.
	1 to: 3 do: [ :i |
		self assert: (next basicAt: i) equals: 2 ** i ].
	self assert: next selector equals: #one.
	self assert: next method equals: 'fake'.
]

{ #category : #examples }
GtWireEncodingExamples >> maxDepth [
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
	encoder 
		remainingDepth: 2;
		nextPut: array.
	byteArray := encoder contents.
	self assert: byteArray size equals: 8.
	next := (GtWireDecoder on: byteArray readStream) next.
	self assert: next class equals: Array.
	self assert: next first equals: 1.
	next := next second.
	self assert: next equals: (Array new: 2).
	^ byteArray
]

{ #category : #examples }
GtWireEncodingExamples >> nil [
	<gtExample>
	| encoder byteArray |

	encoder := GtWireEncoder onByteArray.
	encoder nextPut: nil.
	byteArray := encoder contents.
	self assert: byteArray size equals: 1.
	self assert: (GtWireDecoder on: byteArray readStream) next isNil.
]

{ #category : #examples }
GtWireEncodingExamples >> orderedCollection [
	<gtExample>
	| orderedCollection encoder byteArray next |

	encoder := GtWireEncoder onByteArray.
	orderedCollection := { 1. 'hello'. #hello. } asOrderedCollection.
	encoder nextPut: orderedCollection.
	byteArray := encoder contents.
	self assert: byteArray size equals: 18.
	next := (GtWireDecoder on: byteArray readStream) next.
	self assert: next class equals: OrderedCollection.
	self assert: next = orderedCollection.
]

{ #category : #examples }
GtWireEncodingExamples >> packedInteger [
	<gtExample>
	| encoder byteArray integer |

	encoder := GtWireEncoder onByteArray.
	encoder nextPut: 0.
	byteArray := encoder contents.
	self assert: byteArray equals: #[13 0].
	self assert: (GtWireDecoder on: byteArray readStream) next equals: 0.
	integer := 1.
	"GemStone is slow at this test, if it works in GT for the full range, testing a small range in
	GemStone is probably enough"
	[ integer < ((self gtDo: [ SmallInteger maxVal ] gemstoneDo: [ 10 ]) * 10) ] whileTrue:
		[ encoder reset.
		encoder nextPut: integer.
		byteArray := encoder contents.
		self assert: (GtWireDecoder on: byteArray readStream) next equals: integer.
		encoder reset.
		encoder nextPut: integer negated.
		byteArray := encoder contents.
		encoder reset.
		self assert: (GtWireDecoder on: byteArray readStream) next equals: integer negated.
		integer := (1.001 * integer) ceiling. ].
]

{ #category : #examples }
GtWireEncodingExamples >> set [
	<gtExample>
	| set encoder byteArray next |

	encoder := GtWireEncoder onByteArray.
	set := { 1. 'hello'. true. } asSet.
	encoder nextPut: set.
	byteArray := encoder contents.
	self assert: byteArray size equals: 12.
	next := (GtWireDecoder on: byteArray readStream) next.
	self assert: next class equals: Set.
	self assert: next = set.
]

{ #category : #examples }
GtWireEncodingExamples >> wideString [
	<gtExample>
	| string encoder byteArray next |

	encoder := GtWireEncoder onByteArray.
	string := 'čtyři'.
	encoder nextPut: string.
	byteArray := encoder contents.
	self assert: byteArray size equals: string asString utf8Encoded size + 2.
	next := (GtWireDecoder on: byteArray readStream) next.
	self assert: (#(WideString DoubleByteString Unicode16) includes: next class name).
	self assert: next equals: string.
	^ byteArray
]

{ #category : #examples }
GtWireEncodingExamples >> wideStringWithNull [
	<gtExample>
	| string encoder byteArray next |

	encoder := GtWireEncoder onByteArray.
	string := 'čty', (String with: Character null), 'ři'.
	encoder nextPut: string.
	byteArray := encoder contents.
	self assert: byteArray size equals: string asString utf8Encoded size + 2.
	next := (GtWireDecoder on: byteArray readStream) next.
	self assert: (#(WideString DoubleByteString Unicode16) includes: next class name).
	self assert: next equals: string.
	^ byteArray
]

{ #category : #examples }
GtWireEncodingExamples >> wideSymbol [
	<gtExample>
	| wideSymbol encoder byteArray next |

	encoder := GtWireEncoder onByteArray.
	wideSymbol := #'kancelař'.
	encoder nextPut: wideSymbol.
	byteArray := encoder contents.
	self assert: byteArray size equals: wideSymbol asString utf8Encoded size + 2.
	next := (GtWireDecoder on: byteArray readStream) next.
	self assert: (#(WideSymbol DoubleByteSymbol) includes: next class name).
	self assert: next equals: wideSymbol.
]
