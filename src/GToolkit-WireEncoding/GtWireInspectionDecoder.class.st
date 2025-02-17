Class {
	#name : #GtWireInspectionDecoder,
	#superclass : #GtWireDecoder,
	#instVars : [
		'stack',
		'root',
		'byteArray'
	],
	#category : #'GToolkit-WireEncoding'
}

{ #category : #'instance creation' }
GtWireInspectionDecoder class >> byteArray: aByteArray [

	^ (self on: aByteArray readStream)
		byteArray: aByteArray;
		next;
		root
]

{ #category : #accessing }
GtWireInspectionDecoder >> byteArray [
	^ byteArray
]

{ #category : #accessing }
GtWireInspectionDecoder >> byteArray: anObject [
	byteArray := anObject
]

{ #category : #initialization }
GtWireInspectionDecoder >> initialize [

	super initialize.
	"Use an OrderedCollection for the stack since GemStone doesn't have a Stack"
	stack := OrderedCollection new.
]

{ #category : #accessing }
GtWireInspectionDecoder >> next [
	| inspectionObject object parent |

	inspectionObject := GtWireEncodingInspectionObject new.
	root ifNil: [ root := inspectionObject ].
	parent := stack
			ifEmpty: [ nil ]
			ifNotEmpty: [ stack last ].
	inspectionObject 
		parent: parent;
		startIndex: stream position + 1;
		decoder: self.
	stack addLast: inspectionObject.
	object := super next.
	inspectionObject 
		object: object;
		endIndex: stream position.
	parent ifNotNil: [ parent addComponent: #object ->inspectionObject ].
	stack removeLast.
	^ object
]

{ #category : #accessing }
GtWireInspectionDecoder >> nextByteArray [
	| ba |
	ba := super nextByteArray.
	stack last addComponent: #byteArray -> ba.
	^ ba.
]

{ #category : #accessing }
GtWireInspectionDecoder >> nextFloat64 [
	| float64 |

	float64 := super nextFloat64.
	stack last addComponent: #float64 -> float64.
	^ float64.
]

{ #category : #accessing }
GtWireInspectionDecoder >> nextInt64 [
	| int64 |

	int64 := super nextInt64.
	stack last addComponent: #int64 -> int64.
	^ int64.
]

{ #category : #accessing }
GtWireInspectionDecoder >> nextPackedInteger [
	| packedInteger |

	packedInteger := super nextPackedInteger.
	stack last addComponent: #packedInteger -> packedInteger.
	^ packedInteger.
]

{ #category : #accessing }
GtWireInspectionDecoder >> nextSize [
	| size |

	size := super nextSize.
	stack last addComponent: #size -> size.
	^ size.
]

{ #category : #accessing }
GtWireInspectionDecoder >> nextString [
	| string |

	string := super nextString.
	stack last addComponent: #string -> string.
	^ string.
]

{ #category : #accessing }
GtWireInspectionDecoder >> nextTypeIdentifier [
	| typeIdentifier |

	typeIdentifier := super nextTypeIdentifier.
	stack last addComponent: #typeIdentifier -> typeIdentifier.
	^ typeIdentifier.
]

{ #category : #accessing }
GtWireInspectionDecoder >> root [
	^ root
]
