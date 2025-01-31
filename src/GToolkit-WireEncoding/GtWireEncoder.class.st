Class {
	#name : #GtWireEncoder,
	#superclass : #GtWireEncoderDecoder,
	#instVars : [
		'defaultEncoder',
		'maxObjects',
		'objectCount'
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

	^ self on: (ByteArray new: 100) writeStream
]

{ #category : #'as yet unclassified' }
GtWireEncoder >> decoderOn: aReadStream [
	| decoder |
	
	decoder := GtWireDecoder on: aReadStream.
	decoder 
		map: self map;
		reverseMap: self reverseMap.
	^ decoder
]

{ #category : #'as yet unclassified' }
GtWireEncoder >> initialize [

	super initialize.
	defaultEncoder := [ :anObject | GtWireObjectByNameEncoder new ].
	maxObjects := 500000.
	objectCount := 0.
]

{ #category : #accessing }
GtWireEncoder >> maxObjects [
	^ maxObjects
]

{ #category : #accessing }
GtWireEncoder >> maxObjects: anObject [
	maxObjects := anObject
]

{ #category : #accessing }
GtWireEncoder >> nextPut: anObject [

	objectCount > maxObjects ifTrue:
		[ self error: 'Exceeded maximum object count' ].
	(self map at: anObject class
		ifAbsent: [ defaultEncoder value: anObject ])
			encode: anObject with: self.
	objectCount := objectCount + 1.
]

{ #category : #'private - encoding' }
GtWireEncoder >> putByteArray: aByteArray [

	GtWireEncodingByteArraySignal new
		byteArray: aByteArray;
		emit.
	stream nextPutAll: aByteArray.
]

{ #category : #'private - encoding' }
GtWireEncoder >> putFloat64: aFloat [

	GtWireEncodingFloat64Signal new
		float: aFloat;
		emit.
	stream float64: aFloat.
]

{ #category : #'private - encoding' }
GtWireEncoder >> putInt64: anInteger [

	GtWireEncodingInt64Signal new
		integer: anInteger;
		emit.
	stream int64: anInteger.
]

{ #category : #'private - encoding' }
GtWireEncoder >> putPackedInteger: aPositiveInteger [

	GtWireEncodingPositiveIntegerSignal new
		integer: aPositiveInteger;
		emit.
	stream packedInteger: aPositiveInteger.
]

{ #category : #'private - encoding' }
GtWireEncoder >> putSize: anInteger [

	GtWireEncodingSizeSignal new
		size: anInteger;
		emit.
	stream packedInteger: anInteger.
]

{ #category : #'private - encoding' }
GtWireEncoder >> putTypeIdentifier: anInteger [

	GtWireEncodingTypeIdentifierSignal new
		typeIdentifier: anInteger;
		emit.
	stream packedInteger: anInteger.
]

{ #category : #initialization }
GtWireEncoder >> reset [

	super reset.
	objectCount := 0.
]
