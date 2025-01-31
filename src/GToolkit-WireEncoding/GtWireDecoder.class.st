Class {
	#name : #GtWireDecoder,
	#superclass : #GtWireEncoderDecoder,
	#category : #'GToolkit-WireEncoding'
}

{ #category : #'instance creation' }
GtWireDecoder class >> on: aReadStream [

	^ self basicNew stream:
		(GtWireStream on: aReadStream)
]

{ #category : #accessing }
GtWireDecoder >> next [
	| type |

	type := stream packedInteger.
	^ (self reverseMap at: type) decodeWith: self.
]
