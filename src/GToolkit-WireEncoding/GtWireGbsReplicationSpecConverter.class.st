"
GtWireGbsReplicationSpecConverter takes a dictionary of Gbs replicationSpecs and modifies the supplied {{gtClass:GtWireEncoder}} to match the dictionary.
"
Class {
	#name : #GtWireGbsReplicationSpecConverter,
	#superclass : #Object,
	#category : #'GToolkit-WireEncoding'
}

{ #category : #actions }
GtWireGbsReplicationSpecConverter >> flattenSpec: anArray [
	"Remove entries that are later overridden.
	This answers the array in reverse order, which doesn't affect the final outcome."
	| seen |

	seen := Set new.
	^ Array streamContents: [ :stream |
		anArray size to: 1 by: -1 do: [ :i | | iVarEntry |
			iVarEntry := anArray at: i.
			(seen includes: iVarEntry first) ifFalse:
				[ stream nextPut: iVarEntry.
				seen add: iVarEntry first. ] ] ].
]

{ #category : #private }
GtWireGbsReplicationSpecConverter >> forwarderEncodingFor: aGtWireEncoder class: aClass objectEncoder: aGtWireInstVarEncoder instVarMap: instVarMap replicationSpec: replicationSpecArray [

	instVarMap
		at: replicationSpecArray first
		put: GtWireGemStoneRsrEncoder new.
]

{ #category : #private }
GtWireGbsReplicationSpecConverter >> indexablePartEncodingFor: aGtWireEncoder class: aClass objectEncoder: aGtWireInstVarEncoder instVarMap: instVarMap replicationSpec: replicationSpecArray [
	"Indexable objects aren't supported at the moment"

	self notYetImplemented
]

{ #category : #private }
GtWireGbsReplicationSpecConverter >> maxEncodingFor: aGtWireEncoder class: aClass objectEncoder: aGtWireInstVarEncoder instVarMap: instVarMap replicationSpec: replicationSpecArray [

	instVarMap
		at: replicationSpecArray first
		put: (GtWireMaxDepthEncoder new depth: replicationSpecArray third).
]

{ #category : #private }
GtWireGbsReplicationSpecConverter >> minEncodingFor: aGtWireEncoder class: aClass objectEncoder: aGtWireInstVarEncoder instVarMap: instVarMap replicationSpec: replicationSpecArray [

	instVarMap
		at: replicationSpecArray first
		put: (GtWireMinDepthEncoder new depth: replicationSpecArray third).
]

{ #category : #private }
GtWireGbsReplicationSpecConverter >> replicateEncodingFor: aGtWireEncoder class: aClass objectEncoder: aGtWireInstVarEncoder instVarMap: instVarMap replicationSpec: replicationSpecArray [

	instVarMap
		at: replicationSpecArray first
		put: GtWireReplicationEncoder new.
]

{ #category : #actions }
GtWireGbsReplicationSpecConverter >> replicationSpecKeywordToWireObjectEncoderMap [
	"Answer the map from replicationSpec keyword to Wire encoder.
	#replicate and #indexable_part are special cases."

	^ {
		#stub -> #stubEncodingFor:class:objectEncoder:instVarMap:replicationSpec:.
		#forwarder -> #forwarderEncodingFor:class:objectEncoder:instVarMap:replicationSpec:.
		#min -> #minEncodingFor:class:objectEncoder:instVarMap:replicationSpec:.
		#max -> #maxEncodingFor:class:objectEncoder:instVarMap:replicationSpec:.
		#replicate -> #replicateEncodingFor:class:objectEncoder:instVarMap:replicationSpec:.
		#indexable_part -> #indexablePartEncodingFor:class:objectEncoder:instVarMap:replicationSpec:.
	} asDictionary.
]

{ #category : #private }
GtWireGbsReplicationSpecConverter >> stubEncodingFor: aGtWireEncoder class: aClass objectEncoder: aGtWireInstVarEncoder instVarMap: instVarMap replicationSpec: replicationSpecArray [
	"Stubs aren't supported at the moment, return a proxy (forwarder)"

	self forwarderEncodingFor: aGtWireEncoder class: aClass objectEncoder: aGtWireInstVarEncoder instVarMap: instVarMap replicationSpec: replicationSpecArray
]

{ #category : #actions }
GtWireGbsReplicationSpecConverter >> update: aGtWireEncoder class: aClass spec: aSpecArray [
	| flattenedSpec objectEncoder rsWireMap instVarMap |

	flattenedSpec := self flattenSpec: aSpecArray.
	rsWireMap := self replicationSpecKeywordToWireObjectEncoderMap.
	objectEncoder := GtWireInstVarEncoder new.
	instVarMap := Dictionary new.
	aSpecArray do: [ :anArray |
		self perform: (rsWireMap at: anArray second)
			withArguments: { aGtWireEncoder. aClass. objectEncoder. instVarMap. anArray. }
		].
	objectEncoder instVarMap: instVarMap.
	aGtWireEncoder map at: aClass put: objectEncoder.
]

{ #category : #actions }
GtWireGbsReplicationSpecConverter >> update: aGtWireEncoder from: aGbsReplicationSpecDictionary [

	aGbsReplicationSpecDictionary associationsDo: [ :assoc |
		self update: aGtWireEncoder class: assoc key spec: assoc value ].
]
