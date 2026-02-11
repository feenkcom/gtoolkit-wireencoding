Class {
	#name : #GtWireGbsReplicationSpecEncodingExamples,
	#superclass : #Object,
	#category : #'GToolkit-WireEncoding-Examples'
}

{ #category : #examples }
GtWireGbsReplicationSpecEncodingExamples >> gbsAllInstVarsExample [
	"Check that all instance variables are encoded with `replicate` by default"

	<gtExample>
	<return: #ByteArray>
	| replicationSpec object encoder byteArray decoder next now |
	replicationSpec := {GtWireEncodingExampleInstVarObject -> #()} asDictionary.
	now := DateAndTime now.
	object := GtWireEncodingExampleInstVarObject new
			var1: 1;
			var2: '2';
			var3: now;
			var4: #(1 2 3).
	encoder := GtWireEncoder onByteArray.
	GtWireGbsReplicationSpecConverter new update: encoder from: replicationSpec.	"GemStone isn't available here, so replace all GtGemStoneRsrEncoders with dummies"
	encoder
		replaceMappingsMatching: [ :each | each isKindOf: GtWireGemStoneRsrEncoder ]
		with: [ GtWireDummyProxyEncoder new ].
	encoder nextPut: object.
	byteArray := encoder contents.
	decoder := GtWireDecoder on: byteArray readStream.
	next := decoder next.

	self assert: next class equals: GtWireEncodingExampleInstVarObject.
	self assert: next var1 equals: 1.
	self assert: next var2 equals: '2'.
	self assert: next var3 equals: now.
	self assert: next var4 equals: #(1 2 3).
	^ byteArray
]

{ #category : #examples }
GtWireGbsReplicationSpecEncodingExamples >> gbsMaxDepthToWireExample [
	"Demonstrate `max` and `min` keywords in a replication spec"

	<gtExample>
	<return: #GtWireGbsReplicationSpecEncodingExamples>
	| replicationSpec object encoder byteArray decoder next array currentArray |
	replicationSpec := {GtWireEncodingExampleInstVarObject -> #(#(var1 max 2))}
			asDictionary.
	array := Array new: 2.
	currentArray := array.
	1
		to: 10
		do: [ :i | 
			currentArray
				at: 1 put: i;
				at: 2 put: (Array new: 2).
			currentArray := currentArray second ].
	object := GtWireEncodingExampleInstVarObject new var1: array.
	encoder := GtWireEncoder onByteArray.
	GtWireGbsReplicationSpecConverter new update: encoder from: replicationSpec.	"GemStone isn't available here, so replace all GtGemStoneRsrEncoders with dummies"
	encoder
		replaceMappingsMatching: [ :each | each isKindOf: GtWireGemStoneRsrEncoder ]
		with: [ GtWireDummyProxyEncoder new ].
	encoder nextPut: object.
	byteArray := encoder contents.
	decoder := GtWireDecoder on: byteArray readStream.
	next := decoder next.

	self assert: next class equals: GtWireEncodingExampleInstVarObject.
	self assert: next var1 class equals: Array.
	self assert: next var1 first equals: 1.
	self assert: next var1 second equals: #(2 nil)
]

{ #category : #examples }
GtWireGbsReplicationSpecEncodingExamples >> gbsToWireExample1 [
	"Demonstrate `replicate`, `forwarder` and `stub` keywords in a replication spec"

	<gtExample>
	<return: #GtWireGbsReplicationSpecEncodingExamples>
	| replicationSpec object encoder byteArray decoder next |
	replicationSpec := {GtWireEncodingExampleInstVarObject
				-> #(#(var1 replicate) #(var2 forwarder) #(var3 stub))} asDictionary.	"var4 is default, replicate"
	object := GtWireEncodingExampleInstVarObject new
			var1: (GtWireEncodingExampleInstVarObject new var1: 'replicated');
			var2: (GtWireEncodingExampleInstVarObject new var1: 'forwarded (proxy)');
			var3: (GtWireEncodingExampleInstVarObject new var1: 'stub (proxy)');
			var4: (GtWireEncodingExampleInstVarObject new var1: 'default replication').
	encoder := GtWireEncoder onByteArray.
	GtWireGbsReplicationSpecConverter new update: encoder from: replicationSpec.	"GemStone isn't available here, so replace all GtGemStoneRsrEncoders with dummies"
	encoder
		replaceMappingsMatching: [ :each | each isKindOf: GtWireGemStoneRsrEncoder ]
		with: [ GtWireDummyProxyEncoder new ].
	encoder nextPut: object.
	byteArray := encoder contents.
	decoder := GtWireDecoder on: byteArray readStream.
	next := decoder next.

	self assert: next var1 class equals: GtWireEncodingExampleInstVarObject.
	self assert: next var1 var1 equals: 'replicated'.
	self assert: next var1 var2 isNil.
	self assert: next var1 var3 isNil.
	self assert: next var1 var4 isNil.
	self assert: next var2 class equals: GtWireEncodingDummyProxy.
	self
		assert: next var2 description
		equals: '(GtWireEncodingExampleInstVarObject basicNew instVarAt: 1 put: ''forwarded (proxy)''; instVarAt: 2 put: nil; instVarAt: 3 put: nil; instVarAt: 4 put: nil; yourself)'.
	self assert: next var3 class equals: GtWireEncodingDummyProxy.
	self
		assert: next var3 description
		equals: '(GtWireEncodingExampleInstVarObject basicNew instVarAt: 1 put: ''stub (proxy)''; instVarAt: 2 put: nil; instVarAt: 3 put: nil; instVarAt: 4 put: nil; yourself)'.
	self assert: next var4 var1 equals: 'default replication'
]
