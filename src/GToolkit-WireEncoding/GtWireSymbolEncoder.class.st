Class {
	#name : #GtWireSymbolEncoder,
	#superclass : #GtWireCharacterArrayEncoder,
	#category : #'GToolkit-WireEncoding-ObjectEncoder'
}

{ #category : #'encoding - decoding' }
GtWireSymbolEncoder class >> decodeWith: aGtWireEncoderContext [
	
	^ (super decodeWith: aGtWireEncoderContext) asSymbol
]

{ #category : #accessing }
GtWireSymbolEncoder class >> typeIdentifier [

	^ 6
]
