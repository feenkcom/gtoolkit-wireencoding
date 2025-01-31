Class {
	#name : #GtWireEncodingTypeIdentifierSignal,
	#superclass : #GtWireEncodingSignal,
	#instVars : [
		'typeIdentifier'
	],
	#category : #'GToolkit-WireEncoding'
}

{ #category : #printing }
GtWireEncodingTypeIdentifierSignal >> printOneLineContentsOn: stream [

	stream
		<< 'type identifier: ';
		print: typeIdentifier;
		<< ' (';
		<< (GtWireEncoderDecoder typeIdentifierClassName: typeIdentifier);
		<< ' )'.
]

{ #category : #accessing }
GtWireEncodingTypeIdentifierSignal >> typeIdentifier [
	^ typeIdentifier
]

{ #category : #accessing }
GtWireEncodingTypeIdentifierSignal >> typeIdentifier: anObject [
	typeIdentifier := anObject
]
