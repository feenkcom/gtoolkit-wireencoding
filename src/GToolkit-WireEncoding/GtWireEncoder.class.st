Class {
	#name : #GtWireEncoder,
	#superclass : #GtWireEncoderDecoder,
	#instVars : [
		'defaultEncoder',
		'maxObjects',
		'objectCount',
		'remainingDepth',
		'maxDepthEncoder'
	],
	#category : #'GToolkit-WireEncoding'
}

{ #category : #'instance creation' }
GtWireEncoder class >> on: aWriteStream [

	^ self basicNew initialize stream:
		(GtWireStream on: aWriteStream)
]

{ #category : #'instance creation' }
GtWireEncoder class >> onByteArray [

	^ self on: (WriteStream on: (ByteArray new: 100))
]

{ #category : #accessing }
GtWireEncoder >> decoderOn: aReadStream [
	| decoder |
	
	decoder := GtWireDecoder on: aReadStream.
	decoder 
		map: self map;
		reverseMap: self reverseMap.
	^ decoder
]

{ #category : #initialization }
GtWireEncoder >> initialize [

	super initialize.
	defaultEncoder := [ :anObject | GtWireObjectByNameEncoder new ].
	maxObjects := 500000.
	objectCount := 0.
	remainingDepth := maxObjects.
	maxDepthEncoder := GtWireNilEncoder new.
]

{ #category : #accessing }
GtWireEncoder >> isMaxDepthLiteral: anObject [
	"Answer a boolean indicating whether the supplied object is considered a literal, and so can be returned even if the max depth has been reached.
	Based on observed behaviour by GemStone Gbs."

	^ { Number. String. Boolean. } anySatisfy: [ :cls |
		anObject isKindOf: cls ].
]

{ #category : #accessing }
GtWireEncoder >> maxDepthEncoder [
	^ maxDepthEncoder
]

{ #category : #accessing }
GtWireEncoder >> maxDepthEncoder: anObject [
	maxDepthEncoder := anObject
]

{ #category : #accessing }
GtWireEncoder >> maxObjects [
	"Answer the maximum number of objects encoded.
	This is intended as a guard against infinite recursion and is only approximate,
	as enforcing max or min depth is currently counted as an object."
	<return: #Integer>

	^ maxObjects
]

{ #category : #accessing }
GtWireEncoder >> maxObjects: anInteger [
	"Set the maximum number of objects encoded.
	This is intended as a guard against infinite recursion and is only approximate,
	as enforcing max or min depth is currently counted as an object."

	maxObjects := anInteger
]

{ #category : #accessing }
GtWireEncoder >> nextPut: anObject [

	self nextPut: anObject objectEncoder: nil
]

{ #category : #accessing }
GtWireEncoder >> nextPut: anObject objectEncoder: objectEncoder [
	| saveDepth |
	objectCount > maxObjects
		ifTrue: [ self error: 'Exceeded maximum object count' ].
	remainingDepth := remainingDepth - 1.
	saveDepth := remainingDepth.
	"remainingDepth includes the first (root) object, so compare to -1."
	remainingDepth < 0 ifTrue: 
		[ (self isMaxDepthLiteral: anObject)
			ifTrue: [ self privateNextPutMapEncoded: anObject objectEncoder: objectEncoder ]
			ifFalse: [ maxDepthEncoder encode: anObject with: self ] ]
		ifFalse:
			[ self privateNextPutMapEncoded: anObject objectEncoder: objectEncoder ].
	remainingDepth := remainingDepth + 1
]

{ #category : #accessing }
GtWireEncoder >> privateNextPutMapEncoded: anObject [
	(self map at: anObject class ifAbsent: [ defaultEncoder value: anObject ])
		encode: anObject
		with: self
]

{ #category : #accessing }
GtWireEncoder >> privateNextPutMapEncoded: anObject objectEncoder: objectEncoder [

	( objectEncoder ifNil:
		[ self map at: anObject class ifAbsent: [ defaultEncoder value: anObject ] ])
			encode: anObject
			with: self.
	objectCount := objectCount + 1.
]

{ #category : #'private - encoding' }
GtWireEncoder >> putByteArray: aByteArray [

	self putSize: aByteArray size.
	stream nextPutAll: aByteArray.
]

{ #category : #'private - encoding' }
GtWireEncoder >> putFloat64: aFloat [

	"GtWireEncodingFloat64Signal new
		float: aFloat;
		emit."
	stream float64: aFloat.
]

{ #category : #'private - encoding' }
GtWireEncoder >> putInt64: anInteger [

	"GtWireEncodingInt64Signal new
		integer: anInteger;
		emit."
	stream int64: anInteger.
]

{ #category : #accessing }
GtWireEncoder >> putNil [

	self putTypeIdentifier: GtWireNilEncoder typeIdentifier.
]

{ #category : #'private - encoding' }
GtWireEncoder >> putPackedInteger: aPositiveInteger [

	"GtWireEncodingPositiveIntegerSignal new
		integer: aPositiveInteger;
		emit."
	stream packedInteger: aPositiveInteger.
]

{ #category : #'private - encoding' }
GtWireEncoder >> putSize: anInteger [

	"GtWireEncodingSizeSignal new
		size: anInteger;
		emit."
	stream packedInteger: anInteger.
]

{ #category : #'private - encoding' }
GtWireEncoder >> putString: aString [
	| encoded |

	encoded := aString utf8Encoded.
	self putSize: encoded size.
	stream nextPutAll: encoded.
]

{ #category : #'private - encoding' }
GtWireEncoder >> putTypeIdentifier: anInteger [

	"GtWireEncodingTypeIdentifierSignal new
		typeIdentifier: anInteger;
		emit."
	stream packedInteger: anInteger.
]

{ #category : #accessing }
GtWireEncoder >> remainingDepth [
	^ remainingDepth
]

{ #category : #accessing }
GtWireEncoder >> remainingDepth: anInteger [
	"Set the remaining depth.
	The count includes the first (root) or current object."

	remainingDepth := anInteger
]

{ #category : #initialization }
GtWireEncoder >> reset [

	super reset.
	objectCount := 0.
]
