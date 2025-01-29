Class {
	#name : #GtWireEncoder,
	#superclass : #GtWireEncoderDecoder,
	#instVars : [
		'defaultEncoder'
	],
	#category : #'GToolkit-WireEncoding'
}

{ #category : #'instance creation' }
GtWireEncoder class >> on: aByteArray [

	^ self basicNew stream:
		(GtWireWriteStream on: aByteArray)
]

{ #category : #'as yet unclassified' }
GtWireEncoder >> initialize [

	super initialize.
	stream := GtWireWriteStream on: 
		(ByteArray new: 64 * 1024).
	defaultEncoder := [ :anObject | GtWireUnsupportedObject signal: anObject ].
]

{ #category : #accessing }
GtWireEncoder >> nextPut: anObject [

	(self map at: anObject class
		ifAbsent: [ defaultEncoder value: anObject ])
			encode: anObject with: self
]
