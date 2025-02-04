Class {
	#name : #GtWireSymbolEncoder,
	#superclass : #GtWireCharacterArrayEncoder,
	#category : #'GToolkit-WireEncoding'
}

{ #category : #accessing }
GtWireSymbolEncoder class >> typeIdentifier [

	^ 6
]

{ #category : #'encoding - decoding' }
GtWireSymbolEncoder >> decodeWith: aGtWireEncoderContext [
	
	^ (super decodeWith: aGtWireEncoderContext) asSymbol
]
