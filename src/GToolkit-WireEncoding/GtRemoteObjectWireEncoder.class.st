"
GtRemoteObjectWireEncoder is used when all objects up to the specified depth are to be returned as remote objects, i.e. they are returned by value with a proxy object registered with GtRsrProxyServiceClient.
"
Class {
	#name : #GtRemoteObjectWireEncoder,
	#superclass : #GtWireEncoder,
	#instVars : [
		'maxProxyDepth',
		'currentProxyDepth'
	],
	#category : #'GToolkit-WireEncoding'
}

{ #category : #accessing }
GtRemoteObjectWireEncoder >> currentProxyDepth [
	^ currentProxyDepth
]

{ #category : #accessing }
GtRemoteObjectWireEncoder >> currentProxyDepth: anObject [
	^ currentProxyDepth := anObject
]

{ #category : #initialization }
GtRemoteObjectWireEncoder >> initialize [
	super initialize.
	currentProxyDepth := -1
]

{ #category : #accessing }
GtRemoteObjectWireEncoder >> maxProxyDepth [
	"Answer the maximum depth to which proxy objects will be returned along with the object by value.
	nil = unlimited"

	^ maxProxyDepth
]

{ #category : #accessing }
GtRemoteObjectWireEncoder >> maxProxyDepth: anIntegerOrNil [

	maxProxyDepth := anIntegerOrNil
]

{ #category : #'private - encoding' }
GtRemoteObjectWireEncoder >> nextPut: anObject objectEncoder: objectEncoder [
	currentProxyDepth := currentProxyDepth + 1.
	^ [ super nextPut: anObject objectEncoder: objectEncoder ] 
		ensure: [ currentProxyDepth := currentProxyDepth - 1 ]
]

{ #category : #encoding }
GtRemoteObjectWireEncoder >> privateNextPutMapEncoded: anObject objectEncoder: objectEncoder [
	| encoder |
	
	encoder := objectEncoder ifNil:
		[ self map at: anObject class ifAbsent: [ self defaultEncoder value: anObject ] ].
	(anObject isNil or: 
	[ encoder isProxyObjectEncoder or:
	[ self shouldEncodeWithProxyAtCurrentDepth not ] ]) ifTrue:
		[ encoder encode: anObject with: self. ]
	ifFalse:
		[ GtWireGemStoneWithRsrEncoder new encode: anObject with: self objectEncoder: encoder ].
	objectCount := objectCount + 1.
]

{ #category : #testing }
GtRemoteObjectWireEncoder >> shouldEncodeWithProxyAtCurrentDepth [
	maxProxyDepth ifNil: [ ^ true ].
	^ currentProxyDepth <= maxProxyDepth
]
