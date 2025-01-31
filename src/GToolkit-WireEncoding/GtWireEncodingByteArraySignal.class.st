Class {
	#name : #GtWireEncodingByteArraySignal,
	#superclass : #GtWireEncodingSignal,
	#instVars : [
		'byteArray'
	],
	#category : #'GToolkit-WireEncoding'
}

{ #category : #accessing }
GtWireEncodingByteArraySignal >> byteArray [
	^ byteArray
]

{ #category : #accessing }
GtWireEncodingByteArraySignal >> byteArray: anObject [
	byteArray := anObject
]

{ #category : #printing }
GtWireEncodingByteArraySignal >> gtHexDumpFor: aView [
	<gtView>

	byteArray class == ByteArray ifFalse:
		[ ^ aView empty ].
	^ aView forward
		title: 'Hex dump';
		object: [ byteArray ];
		view: #gtHexDumpFor:
]

{ #category : #'as yet unclassified' }
GtWireEncodingByteArraySignal >> printOneLineContentsOn: stream [

	stream
		<< 'byte array: ';
		print: byteArray size.
]
