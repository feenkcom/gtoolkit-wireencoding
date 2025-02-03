Class {
	#name : #BaselineOfGToolkitWireEncoding,
	#superclass : #BaselineOf,
	#category : #BaselineOfGToolkitWireEncoding
}

{ #category : #loading }
BaselineOfGToolkitWireEncoding class >> loadLepiter [
	IceRepository registry  
		detect: [ :aRepository |
			aRepository name = 'gtoolkit-wireencoding' ]
		ifFound: [ :aRepository | 
			| defaultDatabase currentProperties |
			defaultDatabase := LeDatabasesRegistry defaultLogicalDatabase.
			currentProperties := defaultDatabase properties.
			currentProperties addRegisteredDirectory: aRepository repositoryDirectory / 'lepiter'.
			defaultDatabase reload ]
		ifNone: [
			self inform: 'Repository not found.' ]
]

{ #category : #baseline }
BaselineOfGToolkitWireEncoding >> baseline: spec [
	<baseline>
		^ spec for: #common do: [
			spec 
				package: 'GToolkit-WireEncoding';
				package: 'GToolkit-WireEncoding-GT'
		]
]
