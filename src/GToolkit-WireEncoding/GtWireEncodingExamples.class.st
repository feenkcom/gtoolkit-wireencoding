Class {
	#name : #GtWireEncodingExamples,
	#superclass : #Object,
	#traits : 'TAssertable',
	#classTraits : 'TAssertable classTrait',
	#category : #'GToolkit-WireEncoding-Examples'
}

{ #category : #examples }
GtWireEncodingExamples >> array [
	<gtExample>
	| array encoder byteArray next |

	encoder := GtWireEncoder new.
	array := { 1. 'hello'. #hello. }.
	encoder nextPut: array.
	byteArray := encoder contents.
	self assert: byteArray size equals: 26.
	next := (GtWireDecoder on: byteArray) next.
	self assert: next class equals: Array.
	self assert: next = array.
]

{ #category : #examples }
GtWireEncodingExamples >> association [
	<gtExample>
	| association encoder byteArray next |

	encoder := GtWireEncoder new.
	association := 1 -> 'one'.
	encoder nextPut: association.
	byteArray := encoder contents.
	self assert: byteArray size equals: 13.
	next := (GtWireDecoder on: byteArray) next.
	self assert: next class equals: Association.
	self assert: next = association.
]

{ #category : #examples }
GtWireEncodingExamples >> boolean [
	<gtExample>
	| encoder byteArray decoder |

	encoder := GtWireEncoder new.
	encoder nextPut: true.
	encoder nextPut: false.
	byteArray := encoder contents.
	self assert: byteArray size equals: 4.
	decoder := GtWireDecoder on: byteArray.
	self assert: decoder next.
	self deny: decoder next.
]

{ #category : #examples }
GtWireEncodingExamples >> byteArray [
	<gtExample>
	| source encoder byteArray next |

	encoder := GtWireEncoder new.
	source := #[3 1 4 1 5].
	encoder nextPut: source.
	byteArray := encoder contents.
	self assert: byteArray size equals: source size + 4.
	next := (GtWireDecoder on: byteArray) next.
	self assert: next class equals: ByteArray.
	self assert: next equals: source.
]

{ #category : #examples }
GtWireEncodingExamples >> byteString [
	<gtExample>
	| string encoder byteArray next |

	encoder := GtWireEncoder new.
	string := 'Hello, World'.
	encoder nextPut: string.
	byteArray := encoder contents.
	self assert: byteArray size equals: string size + 4.
	next := (GtWireDecoder on: byteArray) next.
	self assert: next class equals: ByteString.
	self assert: next equals: string.
]

{ #category : #examples }
GtWireEncodingExamples >> byteSymbol [
	<gtExample>
	| string encoder byteArray next |

	encoder := GtWireEncoder new.
	string := #'Hello, World'.
	encoder nextPut: string.
	byteArray := encoder contents.
	self assert: byteArray size equals: string size + 4.
	next := (GtWireDecoder on: byteArray) next.
	self assert: next class equals: ByteSymbol.
	self assert: next equals: string.
]

{ #category : #examples }
GtWireEncodingExamples >> character [
	<gtExample>
	| encoder byteArray next |

	encoder := GtWireEncoder new.
	encoder nextPut: $§.
	byteArray := encoder contents.
	self assert: byteArray size equals: 4.
	next := (GtWireDecoder on: byteArray) next.
	self assert: next class equals: Character.
	self assert: next == $§.
]

{ #category : #examples }
GtWireEncodingExamples >> dateAndTime [
	<gtExample>
	| dateAndTime encoder byteArray next |

	encoder := GtWireEncoder new.
	dateAndTime := DateAndTime now.
	encoder nextPut: dateAndTime.
	byteArray := encoder contents.
	self assert: byteArray size equals: 23.
	next := (GtWireDecoder on: byteArray) next.
	self assert: next class equals: DateAndTime.
	self assert: next = dateAndTime.
]

{ #category : #examples }
GtWireEncodingExamples >> dictionary [
	<gtExample>
	| dictionary encoder byteArray next |

	encoder := GtWireEncoder new.
	dictionary := {
		 1 -> 'one'.
		 2 -> 'two' } asDictionary.
	encoder nextPut: dictionary.
	byteArray := encoder contents.
	self assert: byteArray size equals: 26.
	next := (GtWireDecoder on: byteArray) next.
	self assert: next class equals: Dictionary.
	self assert: next = dictionary.
]

{ #category : #examples }
GtWireEncodingExamples >> float [
	<gtExample>
	| encoder byteArray next |

	encoder := GtWireEncoder new.
	{ Float fmin. Float fmax. 1.25. } doWithIndex: [ :f :i |
		encoder reset.
		encoder nextPut: f.
		byteArray := encoder contents.
		self assert: byteArray size equals: 10.
		next := (GtWireDecoder on: byteArray) next.
		self assert: next equals: f ].
]

{ #category : #examples }
GtWireEncodingExamples >> nil [
	<gtExample>
	| encoder byteArray |

	encoder := GtWireEncoder new.
	encoder nextPut: nil.
	byteArray := encoder contents.
	self assert: byteArray size equals: 2.
	self assert: (GtWireDecoder on: byteArray) next isNil.
]

{ #category : #examples }
GtWireEncodingExamples >> orderedCollection [
	<gtExample>
	| orderedCollection encoder byteArray next |

	encoder := GtWireEncoder new.
	orderedCollection := { 1. 'hello'. #hello. } asOrderedCollection.
	encoder nextPut: orderedCollection.
	byteArray := encoder contents.
	self assert: byteArray size equals: 26.
	next := (GtWireDecoder on: byteArray) next.
	self assert: next class equals: OrderedCollection.
	self assert: next = orderedCollection.
]

{ #category : #examples }
GtWireEncodingExamples >> packedInteger [
	<gtExample>
	| encoder byteArray |

	encoder := GtWireEncoder new.
	1 to: (2 raisedTo: 18) by: 100 do: [ :i |
		encoder reset.
		encoder nextPut: i.
		byteArray := encoder contents.
		self assert: byteArray size equals: i bytesCount + 3.
		self assert: (GtWireDecoder on: byteArray) next equals: i.
		encoder reset.
		encoder nextPut: i negated.
		byteArray := encoder contents.
		self assert: byteArray size equals: i bytesCount + 3.
		encoder reset.
		self assert: (GtWireDecoder on: byteArray) next equals: i negated ].
]

{ #category : #examples }
GtWireEncodingExamples >> set [
	<gtExample>
	| set encoder byteArray next |

	encoder := GtWireEncoder new.
	set := { 1. 'hello'. true. } asSet.
	encoder nextPut: set.
	byteArray := encoder contents.
	self assert: byteArray size equals: 19.
	next := (GtWireDecoder on: byteArray) next.
	self assert: next class equals: Set.
	self assert: next = set.
]

{ #category : #examples }
GtWireEncodingExamples >> wideString [
	<gtExample>
	| string encoder byteArray next |

	encoder := GtWireEncoder new.
	string := 'Hello, World§'.
	encoder nextPut: string.
	byteArray := encoder contents.
	self assert: byteArray size equals: string utf8Encoded size + 4.
	next := (GtWireDecoder on: byteArray) next.
	self assert: next class equals: ByteString.
	self assert: next equals: string.
]

{ #category : #examples }
GtWireEncodingExamples >> wideSymbol [
	<gtExample>
	| string encoder byteArray next |

	encoder := GtWireEncoder new.
	string := #'Hello, World§'.
	encoder nextPut: string.
	byteArray := encoder contents.
	self assert: byteArray size equals: string utf8Encoded size + 4.
	next := (GtWireDecoder on: byteArray) next.
	self assert: next class equals: ByteSymbol.
	self assert: next equals: string.
]
