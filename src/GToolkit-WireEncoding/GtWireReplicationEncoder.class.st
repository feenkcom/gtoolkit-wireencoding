"
GtWireReplicationEncoder uses the current mapping for the supplied object, unless it would return a type of proxy, in which case {{gtClass:GtWireObjectByNameEncoder}} is used.
"
Class {
	#name : #GtWireReplicationEncoder,
	#superclass : #GtWireObjectEncoder,
	#category : #'GToolkit-WireEncoding'
}

{ #category : #'encoding - decoding' }
GtWireReplicationEncoder >> decodeWith: aGtWireEncoderContext [
	
	^ self error: 'This can''t be serialised directly'
]

{ #category : #'as yet unclassified' }
GtWireReplicationEncoder >> encode: anObject with: aGtWireEncoderContext [
	"Encode the supplied object using its default encoding, unless it would return a type of proxy, in which case {{gtClass:GtWireObjectByNameEncoder}} is used."
	| encoder |

	encoder := aGtWireEncoderContext map at: anObject class.
	encoder isProxyObjectEncoder ifTrue:
		[ encoder := GtWireObjectByNameEncoder new ].
	encoder encode: anObject with: aGtWireEncoderContext.
]
