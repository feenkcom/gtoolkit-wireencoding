Class {
	#name : #GtWireLeJsonV4Encoder,
	#superclass : #GtWireObjectEncoder,
	#instVars : [
		'leJsonV4Reader',
		'leJsonV4Writer'
	],
	#category : #'GToolkit-WireEncoding-GT'
}

{ #category : #accessing }
GtWireLeJsonV4Encoder class >> typeIdentifier [

	^ 18
]

{ #category : #'encoding - decoding' }
GtWireLeJsonV4Encoder >> decodeWith: aGtWireEncoderContext [
	| stream leJsonString|

	stream := aGtWireEncoderContext stream.
	leJsonString := (stream next: stream packedInteger) utf8Decoded.
	^ self leJsonV4Reader
		on: leJsonString readStream;
		next.
]

{ #category : #'encoding - decoding' }
GtWireLeJsonV4Encoder >> encode: anObject with: aGtWireEncoderContext [
	| leJsonEncoded |

	leJsonEncoded := (String streamContents: [ :stream |
		self leJsonV4Writer
			on: stream;
			prettyPrint: false;
			nextPut: anObject]) utf8Encoded.
	aGtWireEncoderContext
		putTypeIdentifier: self class typeIdentifier;
		putByteArray: leJsonEncoded.
]

{ #category : #accessing }
GtWireLeJsonV4Encoder >> leJsonV4Reader [
	^ leJsonV4Reader ifNil: [ LeJsonV4 default newReader ]
]

{ #category : #accessing }
GtWireLeJsonV4Encoder >> leJsonV4Reader: anObject [
	leJsonV4Reader := anObject
]

{ #category : #accessing }
GtWireLeJsonV4Encoder >> leJsonV4Writer [
	^ leJsonV4Writer  ifNil: [ LeJsonV4 default newWriter ]
]

{ #category : #accessing }
GtWireLeJsonV4Encoder >> leJsonV4Writer: anObject [
	leJsonV4Writer := anObject
]
