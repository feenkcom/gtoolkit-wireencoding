Class {
	#name : #GtWireNilEncoder,
	#superclass : #GtWireObjectEncoder,
	#category : #'GToolkit-WireEncoding'
}

{ #category : #'encoding - decoding' }
GtWireNilEncoder class >> decodeWith: aGtWireEncoderContext [
	
	^ nil
]

{ #category : #accessing }
GtWireNilEncoder class >> typeIdentifier [

	^ 1
]
