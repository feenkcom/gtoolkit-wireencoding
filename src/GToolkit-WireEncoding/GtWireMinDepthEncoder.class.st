Class {
	#name : #GtWireMinDepthEncoder,
	#superclass : #GtWireObjectEncoder,
	#instVars : [
		'depth',
		'encoder'
	],
	#category : #'GToolkit-WireEncoding'
}

{ #category : #'instance creation' }
GtWireMinDepthEncoder class >> depth: anInteger encoder: aGtWireEncoder [

	^ self new
		depth: anInteger;
		encoder: aGtWireEncoder
]

{ #category : #'encoding - decoding' }
GtWireMinDepthEncoder >> decodeWith: aGtWireEncoderContext [

	self error: 'Should not be decoded'
]

{ #category : #accessing }
GtWireMinDepthEncoder >> depth [
	^ depth
]

{ #category : #accessing }
GtWireMinDepthEncoder >> depth: anObject [
	depth := anObject
]

{ #category : #'encoding - decoding' }
GtWireMinDepthEncoder >> encode: anObject with: aGtWireEncoderContext [
	| oldDepth |

	oldDepth := aGtWireEncoderContext remainingDepth.
	aGtWireEncoderContext remainingDepth: (oldDepth max: depth).
	encoder
		ifNil: [ aGtWireEncoderContext nextPut: anObject ]
		ifNotNil: [ encoder encode: anObject with: aGtWireEncoderContext ].
	aGtWireEncoderContext remainingDepth: oldDepth.
]

{ #category : #accessing }
GtWireMinDepthEncoder >> encoder [
	^ encoder
]

{ #category : #accessing }
GtWireMinDepthEncoder >> encoder: anObject [
	encoder := anObject
]
