Class {
	#name : #GtWireEncoderDecoder,
	#superclass : #Object,
	#instVars : [
		'stream',
		'map',
		'reverseMap'
	],
	#classVars : [
		'DefaultMap',
		'DefaultReverseMap'
	],
	#category : #'GToolkit-WireEncoding-Encoder'
}

{ #category : #accessing }
GtWireEncoderDecoder class >> cleanUp [

	DefaultMap := DefaultReverseMap := nil
]

{ #category : #accessing }
GtWireEncoderDecoder class >> defaultMap [

	^ DefaultMap ifNil: [ DefaultMap := self defaultMapping ].
]

{ #category : #accessing }
GtWireEncoderDecoder class >> defaultMapping [
	"The default mapping only encodes directly supported classes"

	^ IdentityDictionary new
		at: Association put: GtWireAssociationEncoder;
		at: False put: GtWireFalseEncoder;
		at: True put: GtWireTrueEncoder;
		at: ByteArray put: GtWireByteArrayEncoder;
		at: ByteString put: GtWireStringEncoder;
		at: WideString put: GtWireStringEncoder;
		at: ByteSymbol put: GtWireSymbolEncoder;
		at: WideSymbol put: GtWireSymbolEncoder;
		at: Character put: GtWireCharacterEncoder;
		at: Array put: GtWireArrayEncoder;
		at: Dictionary put: GtWireDictionaryEncoder;
		at: OrderedCollection put: GtWireOrderedCollectionEncoder;
		at: Set put: GtWireSetEncoder;
		at: SmallInteger put: GtWireIntegerEncoder;
		at: LargePositiveInteger put: GtWirePositiveIntegerEncoder;
		at: LargeNegativeInteger put: GtWireNegativeIntegerEncoder;
		at: SmallFloat64 put: GtWireFloatEncoder;
		at: BoxedFloat64 put: GtWireFloatEncoder;
		at: UndefinedObject put: GtWireNilEncoder;
		at: DateAndTime put: GtWireDateAndTimeEncoder;
		yourself
]

{ #category : #accessing }
GtWireEncoderDecoder class >> defaultReverseMap [

	^ DefaultReverseMap ifNil: [ DefaultReverseMap := self reverseMapFrom: self defaultMap ].
]

{ #category : #accessing }
GtWireEncoderDecoder class >> reverseMapFrom: aDictionary [
	| reverseMap |

	reverseMap := IdentityDictionary new.
	aDictionary associationsDo: [ :assoc |
		reverseMap at: assoc value typeIdentifier
			put: assoc value ]. 
	^ reverseMap
]

{ #category : #'as yet unclassified' }
GtWireEncoderDecoder >> contents [

	^ stream contents
]

{ #category : #accessing }
GtWireEncoderDecoder >> loadDefaultMap [

	self map: self class defaultMapping
]

{ #category : #accessing }
GtWireEncoderDecoder >> loadReverseMap [

	reverseMap := self class reverseMapFrom: map.
]

{ #category : #accessing }
GtWireEncoderDecoder >> map [
	map ifNil: 
		[ map := self class defaultMap.
		reverseMap := self class defaultReverseMap ].
	^ map
]

{ #category : #accessing }
GtWireEncoderDecoder >> map: aDictionary [

	map := aDictionary.
	self loadReverseMap.
]

{ #category : #'as yet unclassified' }
GtWireEncoderDecoder >> reset [

	stream reset
]

{ #category : #'as yet unclassified' }
GtWireEncoderDecoder >> reverseMap [

	reverseMap ifNil: 
		[ map := self class defaultMap.
		reverseMap := self class defaultReverseMap ].
	^ reverseMap
]

{ #category : #accessing }
GtWireEncoderDecoder >> stream [

	^ stream ifNil: [ stream := GtWireReadWriteStream on:
		(ByteArray new: 64 * 1024) ]
]

{ #category : #accessing }
GtWireEncoderDecoder >> stream: aGtWireReadWriteStream [

	stream := aGtWireReadWriteStream
]
