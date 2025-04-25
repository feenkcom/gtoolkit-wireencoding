"
GtWireDummyProxyEncoder is used for testing as the real proxy encoders require the associated environment to be instantiated.
"
Class {
	#name : #GtWireDummyProxyEncoder,
	#superclass : #GtWireObjectEncoder,
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

	anObject ifNil:
		[ ^ GtWireNilEncoder new encode: anObject with: aGtWireEncoderContext ].

	aGtWireEncoderContext
		putTypeIdentifier: self class typeIdentifier;
		putString: anObject storeString.
]

{ #category : #'as yet unclassified' }
GtWireDummyProxyEncoder >> isProxyObjectEncoder [
	"Answer a boolean indicating whether the receiver is a type of proxy encoder.
	Proxy encoding is platform dependent."

	^ true.
]
