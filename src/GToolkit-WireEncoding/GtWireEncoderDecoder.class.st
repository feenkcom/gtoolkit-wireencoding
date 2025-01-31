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
	#category : #'GToolkit-WireEncoding'
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
GtWireEncoderDecoder class >> defaultReverseMap [

	^ DefaultReverseMap ifNil: [ DefaultReverseMap := self reverseMapFrom: self defaultMap ].
]

{ #category : #accessing }
GtWireEncoderDecoder class >> reverseMapFrom: aDictionary [
	"Construct the reverse map.
	Build a default reverse map and then  overwrite with the supplied configured encoders."
	| reverseMap |

	reverseMap := IdentityDictionary new.
	GtWireObjectEncoder allSubclassesDo: [ :cls |
		cls typeIdentifierOrNil ifNotNil:
			[ :id | reverseMap at: id put: cls new ] ].
	aDictionary associationsDo: [ :assoc |
		reverseMap at: assoc value class typeIdentifier
			put: assoc value ].
	^ reverseMap
]

{ #category : #accessing }
GtWireEncoderDecoder class >> typeIdentifierClassName: anInteger [
	"Answer the class name with the supplied identifier"

	^ (self defaultReverseMap at: anInteger ifAbsent:
		[ GtWireObjectEncoder allSubclasses
			detect: [ :cls | cls typeIdentifierOrNil = anInteger ]
			ifNone: [ nil ] ])
				ifNil: [ '<not found>' ]
				ifNotNil: #name
]

{ #category : #'as yet unclassified' }
GtWireEncoderDecoder >> addMapping: aClass to: anEncoder [

	self map at: aClass put: anEncoder.
	self reverseMap at: anEncoder class typeIdentifier put: anEncoder
]

{ #category : #'as yet unclassified' }
GtWireEncoderDecoder >> contents [

	^ stream contents
]

{ #category : #accessing }
GtWireEncoderDecoder >> loadDefaultMap [

	self map: self class defaultMapping copy
]

{ #category : #accessing }
GtWireEncoderDecoder >> loadReverseMap [

	reverseMap := self class defaultReverseMap copy.
]

{ #category : #accessing }
GtWireEncoderDecoder >> map [
	map ifNil: [ self loadDefaultMap ].
	^ map
]

{ #category : #accessing }
GtWireEncoderDecoder >> map: aDictionary [

	map := aDictionary.
	self loadReverseMap.
]

{ #category : #initialization }
GtWireEncoderDecoder >> reset [

	stream reset
]

{ #category : #'as yet unclassified' }
GtWireEncoderDecoder >> reverseMap [

	reverseMap ifNil: [ self loadReverseMap ].
	^ reverseMap
]

{ #category : #accessing }
GtWireEncoderDecoder >> reverseMap: anObject [
	reverseMap := anObject
]

{ #category : #accessing }
GtWireEncoderDecoder >> stream [

	^ stream ifNil: [ stream := GtWireStream on:
		(ByteArray new: 64 * 1024) ]
]

{ #category : #accessing }
GtWireEncoderDecoder >> stream: aGtWireReadWriteStream [

	stream := aGtWireReadWriteStream
]
