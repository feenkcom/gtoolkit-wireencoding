"
GtWireClassEncoder passes class references by name, assuming that the local and remote class definitions are equivalent.
"
Class {
	#name : 'GtWireClassEncoder',
	#superclass : 'GtWireObjectEncoder',
	#category : 'GToolkit-WireEncoding'
}

{ #category : 'accessing' }
GtWireClassEncoder class >> typeIdentifier [

	^ 28
]

{ #category : 'encoding - decoding' }
GtWireClassEncoder >> decodeWith: aGtWireEncoderContext [
	| className isMeta instanceClass |

	className := aGtWireEncoderContext nextString.
	isMeta := aGtWireEncoderContext next.
	instanceClass := self
		gtDo: [ self class environment classOrTraitNamed: className ]
		gemstoneDo: [ (System myUserProfile resolveSymbol: className asSymbol) value ].
	^ isMeta
		ifTrue: [ instanceClass class ]
		ifFalse: [ instanceClass ].
]

{ #category : 'encoding - decoding' }
GtWireClassEncoder >> encode: aClass with: aGtWireEncoderContext [

	aGtWireEncoderContext 
		putTypeIdentifier: self typeIdentifier;
		putString: (self
			gtDo: [ aClass instanceSide name ]
			gemstoneDo: [ aClass thisClass name ]);
		nextPut: aClass isMeta.
]
