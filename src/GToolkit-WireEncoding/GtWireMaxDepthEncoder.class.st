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
GtWireMaxDepthEncoder >> encode: anObject with: aGtWireEncoderContext [
	| oldDepth |

	oldDepth := aGtWireEncoderContext remainingDepth.
	aGtWireEncoderContext setRemainingDepth: (oldDepth min: depth).
	encoder
		ifNil: [ aGtWireEncoderContext nextPut: anObject ]
		ifNotNil: [ encoder encode: anObject with: aGtWireEncoderContext ].
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

{ #category : #'private - helpers' }
GtWireMaxDepthEncoder >> replaceMappingsMatching: matchBlock with: replacementBlock [
	"Replace all the mappings matching matchBlock with the encoder returned by replacementBlock.
	Used by examples to avoid needing live servers."

	encoder ifNotNil:
		[ encoder replaceMappingsMatching: matchBlock with: replacementBlock ]
]
