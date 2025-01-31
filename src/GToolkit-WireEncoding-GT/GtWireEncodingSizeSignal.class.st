Class {
	#name : #GtWireEncodingSizeSignal,
	#superclass : #GtWireEncodingSignal,
	#instVars : [
		'size'
	],
	#category : #'GToolkit-WireEncoding-GT'
}

{ #category : #'as yet unclassified' }
GtWireEncodingSizeSignal >> printOneLineContentsOn: stream [

	stream
		<< 'size: ';
		print: size.
]

{ #category : #accessing }
GtWireEncodingSizeSignal >> size [
	^ size
]

{ #category : #accessing }
GtWireEncodingSizeSignal >> size: anObject [
	size := anObject
]
