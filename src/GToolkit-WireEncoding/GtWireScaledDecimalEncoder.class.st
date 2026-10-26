"
Encodes a ScaledDecimal as its numerator, denominator, and display scale, each written as an object in that order. Decoding reconstructs the exact numeric value and preserves the original display scale. This three-object payload is not compatible with the previous two-object payload that omitted the scale.
"
Class {
	#name : #GtWireScaledDecimalEncoder,
	#superclass : #GtWireObjectEncoder,
	#category : #'GToolkit-WireEncoding'
}

{ #category : #accessing }
GtWireScaledDecimalEncoder class >> typeIdentifier [
	^ 31
]

{ #category : #encoding }
GtWireScaledDecimalEncoder >> decodeWith: aGtWireEncoderContext [
	| numerator denominator scale |
	numerator := aGtWireEncoderContext next.
	denominator := aGtWireEncoderContext next.
	scale := aGtWireEncoderContext next.
	^ (numerator / denominator) asScaledDecimal: scale
]

{ #category : #encoding }
GtWireScaledDecimalEncoder >> encode: aScaledDecimal with: aGtWireEncoderContext [
	aGtWireEncoderContext putTypeIdentifier: self typeIdentifier.
	aGtWireEncoderContext
		nextPut: aScaledDecimal numerator;
		nextPut: aScaledDecimal denominator;
		nextPut: aScaledDecimal scale
]
