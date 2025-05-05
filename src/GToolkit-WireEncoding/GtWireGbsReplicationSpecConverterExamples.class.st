Class {
	#name : #GtWireGbsReplicationSpecConverterExamples,
	#superclass : #Object,
	#category : #'GToolkit-WireEncoding'
}

{ #category : #examples }
GtWireGbsReplicationSpecConverterExamples >> flattenSpec [
	"Check that the spec is correctly flattened."

	<gtExample>
	<return: #GtWireGbsReplicationSpecConverterExamples>
	| spec actual expected |
	spec := #(#(a 1) #(b 1) #(a 2)).
	actual := GtWireGbsReplicationSpecConverter new flattenSpec: spec.
	expected := #(#(a 2) #(b 1)).
	self assert: actual equals: expected
]
