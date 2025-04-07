"
GtWireDummyProxyEncoder is used for testing as the real proxy encoders require the associated environment to be instantiated.
"
Class {
	#name : #GtWireDummyProxyEncoder,
	#superclass : #GtWireObjectEncoder,
	#instVars : [
		'object'
	],
	#category : #'GToolkit-WireEncoding'
}

{ #category : #access }
GtWireDummyProxyEncoder class >> typeIdentifier [

	^ 25
]

{ #category : #'encoding - decoding' }
GtWireDummyProxyEncoder >> decodeWith: aGtWireEncoderContext [
	"Decode the object on the supplied context"

	^ GtWireEncodingDummyProxy new description: aGtWireEncoderContext nextString.
]

{ #category : #'encoding - decoding' }
GtWireDummyProxyEncoder >> encode: anObject with: aGtWireEncoderContext [

	aGtWireEncoderContext
		putTypeIdentifier: self class typeIdentifier;
		putString: anObject printString.
]

{ #category : #accessing }
GtWireDummyProxyEncoder >> object [
	^ object
]

{ #category : #accessing }
GtWireDummyProxyEncoder >> object: anObject [
	object := anObject
]
