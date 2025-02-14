Class {
	#name : #GtWireMaxDepthEncoder,
	#superclass : #GtWireObjectEncoder,
	#instVars : [
		'depth',
		'encoder'
	],
	#category : #'GToolkit-WireEncoding'
}

{ #category : #'instance creation' }
GtWireMaxDepthEncoder class >> depth: anInteger encoder: aGtWireEncoder [

	^ self new
		depth: anInteger;
		encoder: aGtWireEncoder
]

{ #category : #'encoding - decoding' }
GtWireMaxDepthEncoder >> decodeWith: aGtWireEncoderContext [

	self error: 'Should not be decoded'
]

{ #category : #accessing }
GtWireMaxDepthEncoder >> depth [
	^ depth
]

{ #category : #accessing }
GtWireMaxDepthEncoder >> depth: anObject [
	depth := anObject
]

{ #category : #'encoding - decoding' }
GtWireMaxDepthEncoder >> encode: anInteger with: aGtWireEncoderContext [
	| oldDepth |

	oldDepth := aGtWireEncoderContext remainingDepth.
	aGtWireEncoderContext remainingDepth: (oldDepth min: depth).
	encoder
		ifNil: [ aGtWireEncoderContext nextPut: anInteger ]
		ifNotNil: [ encoder encode: anInteger with: aGtWireEncoderContext ].
	aGtWireEncoderContext remainingDepth: oldDepth.
]

{ #category : #accessing }
GtWireMaxDepthEncoder >> encoder [
	^ encoder
]

{ #category : #accessing }
GtWireMaxDepthEncoder >> encoder: anObject [
	encoder := anObject
]
