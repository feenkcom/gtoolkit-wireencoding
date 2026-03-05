Class {
	#name : #GtRemoteObjectWireEncoder,
	#superclass : #GtWireEncoder,
	#category : #'GToolkit-WireEncoding'
}

{ #category : #encoding }
GtRemoteObjectWireEncoder >> privateNextPutMapEncoded: anObject objectEncoder: objectEncoder [
	| encoder |
	
	encoder := objectEncoder ifNil:
		[ self map at: anObject class ifAbsent: [ self defaultEncoder value: anObject ] ].
	(anObject isNil or: [ encoder isProxyObjectEncoder ]) ifTrue:
		[ encoder encode: anObject with: self. ]
	ifFalse:
		[ GtWireGemStoneWithRsrEncoder new encode: anObject with: self objectEncoder: encoder ].
	objectCount := objectCount + 1.
]
