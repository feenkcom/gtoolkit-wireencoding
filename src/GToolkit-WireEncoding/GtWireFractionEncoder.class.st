Class {
	#name : #GtWireFractionEncoder,
	#superclass : #GtWireObjectEncoder,
	#category : #'GToolkit-WireEncoding'
}

{ #category : #accessing }
GtWireFractionEncoder class >> typeIdentifier [

	^ 30
]

{ #category : #'encoding - decoding' }
GtWireFractionEncoder >> decodeWith: aGtWireEncoderContext [
	| numerator denominator negative fraction |
	
	negative := aGtWireEncoderContext next.
	numerator := aGtWireEncoderContext nextPackedInteger.
	denominator := aGtWireEncoderContext nextPackedInteger.
	fraction := numerator / denominator.
	^ negative
		ifTrue: [ fraction negated ]
		ifFalse: [ fraction ].
]

{ #category : #'encoding - decoding' }
GtWireFractionEncoder >> encode: aFraction with: aGtWireEncoderContext [
	| absFraction |

	absFraction := aFraction abs.
	aGtWireEncoderContext 
		putPackedInteger: self typeIdentifier;
		nextPut: aFraction negative;
		putPackedInteger: absFraction numerator;
		putPackedInteger: absFraction denominator.
]
