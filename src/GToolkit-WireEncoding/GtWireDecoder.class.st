Class {
	#name : #GtWireDecoder,
	#superclass : #GtWireEncoderDecoder,
	#category : #'GToolkit-WireEncoding'
}

{ #category : #'instance creation' }
GtWireDecoder class >> on: aByteArray [

	^ self basicNew stream:
		(GtWireReadStream on: aByteArray)
]

{ #category : #accessing }
GtWireDecoder >> next [
	| type |

	type := stream packedInteger.
	^ (self reverseMap at: type) decodeWith: self.
]
