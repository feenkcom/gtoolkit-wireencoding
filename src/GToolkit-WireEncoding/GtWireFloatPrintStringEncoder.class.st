Class {
	#name : 'GtWireFloatPrintStringEncoder',
	#superclass : 'GtWireObjectEncoder',
	#category : 'GToolkit-WireEncoding'
}

{ #category : 'accessing' }
GtWireFloatPrintStringEncoder class >> typeIdentifier [

	^ 29
]

{ #category : 'encoding - decoding' }
GtWireFloatPrintStringEncoder >> decodeWith: aGtWireEncoderContext [

	^ aGtWireEncoderContext nextString asNumber
]

{ #category : 'encoding - decoding' }
GtWireFloatPrintStringEncoder >> encode: aFloat with: aGtWireEncoderContext [

	aGtWireEncoderContext 
		putPackedInteger: self typeIdentifier;
		putString: aFloat asString
]
