Class {
	#name : #GtWireEncodingInspectionObject,
	#superclass : #Object,
	#instVars : [
		'startIndex',
		'object',
		'decoder',
		'parent',
		'children',
		'components',
		'endIndex'
	],
	#category : #'GToolkit-WireEncoding'
}

{ #category : #accessing }
GtWireEncodingInspectionObject >> addComponent: anObject [

	components add: anObject
]

{ #category : #accessing }
GtWireEncodingInspectionObject >> byteArray [

	^ decoder byteArray
]

{ #category : #accessing }
GtWireEncodingInspectionObject >> decoder [
	^ decoder
]

{ #category : #accessing }
GtWireEncodingInspectionObject >> decoder: anObject [
	decoder := anObject
]

{ #category : #accessing }
GtWireEncodingInspectionObject >> endIndex [
	^ endIndex
]

{ #category : #accessing }
GtWireEncodingInspectionObject >> endIndex: anObject [
	endIndex := anObject
]

{ #category : #ui }
GtWireEncodingInspectionObject >> gtChildrenFor: aView [
	<gtView>

	^ aView columnedList
		  title: 'Components';
		  priority: 15;
		  items: [ components ];
		  column: 'Field' text: [ :item | item key ];
		  column: 'Value' text: [ :item | item value ];
		  send: [ :item | item value ];
		  actionUpdateButton
]

{ #category : #ui }
GtWireEncodingInspectionObject >> gtHexDumpFor: aView [
	<gtView>

	^ aView forward
		title: 'Buffer';
		priority: 20;
		object: [ self objectByteArray ];
		view: #gtHexDumpFor:
]

{ #category : #ui }
GtWireEncodingInspectionObject >> gtSummaryFor: aView [
	<gtView>

	^ aView columnedList
		  title: 'Summary';
		  priority: 10;
		  items: [ self summaryAttributes ];
		  column: #Attribute text: [ :item | item first ];
		  column: #Value text: [ :item | item second ];
		  send: [ :item | item last ];
		  actionUpdateButton
]

{ #category : #initialization }
GtWireEncodingInspectionObject >> initialize [

	super initialize.
	components := OrderedCollection new.
]

{ #category : #accessing }
GtWireEncodingInspectionObject >> object [
	^ object
]

{ #category : #accessing }
GtWireEncodingInspectionObject >> object: anObject [
	object := anObject
]

{ #category : #'as yet unclassified' }
GtWireEncodingInspectionObject >> objectByteArray [

	^ self byteArray copyFrom: startIndex to: endIndex
]

{ #category : #accessing }
GtWireEncodingInspectionObject >> parent [
	^ parent
]

{ #category : #accessing }
GtWireEncodingInspectionObject >> parent: anObject [
	parent := anObject
]

{ #category : #'as yet unclassified' }
GtWireEncodingInspectionObject >> printOn: aStream [

	aStream
		<< self type name;
		<< '(';
		print: object;
		<< ')'.
]

{ #category : #accessing }
GtWireEncodingInspectionObject >> startIndex [
	^ startIndex
]

{ #category : #accessing }
GtWireEncodingInspectionObject >> startIndex: anObject [
	startIndex := anObject
]

{ #category : #accessing }
GtWireEncodingInspectionObject >> summaryAttributes [

	^ {
		{ 'Start Index'. startIndex. }.
		{ 'Type Indicator'. self typeIndicator. }.
		{ 'Type'. self type. }.
		{ 'Object'. object. }.
	}
]

{ #category : #accessing }
GtWireEncodingInspectionObject >> type [

	^ decoder reverseMap at: self typeIndicator
]

{ #category : #accessing }
GtWireEncodingInspectionObject >> typeIndicator [
	| stream |

	stream := ReadStream on: self byteArray.
	stream position: startIndex - 1.
	^ (GtWireDecoder on: stream) stream packedInteger.
]
