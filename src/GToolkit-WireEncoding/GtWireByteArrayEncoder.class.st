Class {
	#name : #GtWireByteArrayEncoder,
	#superclass : #GtWireObjectEncoder,
	#category : #'GToolkit-WireEncoding'
}

{ #category : #accessing }
GtWireByteArrayEncoder class >> typeIdentifier [

	^ 4
]

{ #category : #'encoding - decoding' }
GtWireByteArrayEncoder >> decodeWith: aGtWireEncoderContext [
	| stream |

	stream := aGtWireEncoderContext stream.
	^ stream next: stream packedInteger.
]

{ #category : #'encoding - decoding' }
GtWireByteArrayEncoder >> encode: aByteArray with: aGtWireEncoderContext [

	aGtWireEncoderContext
		putTypeIdentifier: self typeIdentifier;
		putSize: aByteArray size;
		putByteArray: aByteArray
]
