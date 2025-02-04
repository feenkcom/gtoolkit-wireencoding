Class {
	#name : #GtWireNilEncoder,
	#superclass : #GtWireObjectEncoder,
	#category : #'GToolkit-WireEncoding'
}

{ #category : #accessing }
GtWireNilEncoder class >> typeIdentifier [

	^ 1
]

{ #category : #'encoding - decoding' }
GtWireNilEncoder >> decodeWith: aGtWireEncoderContext [
	
	^ nil
]
