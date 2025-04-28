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
	"Ensure the remaining depth is at least the receiver's depth - 1.
	Subtract one from the depth since we are inside anObject's nextPut:objectEncoder: and the remaining depth has already been decremented for anObject."
	| oldDepth |

	oldDepth := aGtWireEncoderContext remainingDepth.
	aGtWireEncoderContext remainingDepth: (oldDepth max: (depth - 1)).
	aGtWireEncoderContext privateNextPutMapEncoded: anObject objectEncoder: encoder.
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

{ #category : #'private - helpers' }
GtWireMinDepthEncoder >> replaceMappingsMatching: matchBlock with: replacementBlock [
	"Replace all the mappings matching matchBlock with the encoder returned by replacementBlock.
	Used by examples to avoid needing live servers."

	encoder replaceMappingsMatching: matchBlock with: replacementBlock
]
