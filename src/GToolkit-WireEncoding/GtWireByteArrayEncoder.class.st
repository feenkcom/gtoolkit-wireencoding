Class {
	#name : #GtWireByteArrayEncoder,
	#superclass : #GtWireObjectEncoder,
	#category : #'GToolkit-WireEncoding'
}

{ #category : #'encoding - decoding' }
GtWireByteArrayEncoder class >> decodeWith: aGtWireEncoderContext [
	| stream |

	stream := aGtWireEncoderContext stream.
	^ stream next: stream packedInteger.
]

{ #category : #'encoding - decoding' }
GtWireByteArrayEncoder class >> encode: aByteArray with: aGtWireEncoderContext [

	aGtWireEncoderContext stream
		packedInteger: self typeIdentifier;
		packedInteger: aByteArray size;
		nextPutAll: aByteArray
]

{ #category : #accessing }
GtWireByteArrayEncoder class >> typeIdentifier [

	^ 4
]
