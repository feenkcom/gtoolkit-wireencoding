Class {
	#name : #GtWireEncodingPerformance,
	#superclass : #Object,
	#category : #'GToolkit-WireEncoding'
}

{ #category : #'instance creation' }
GtWireEncodingPerformance class >> profileWire [

	^ AndreasSystemProfiler new spyOn: 
		[ self new testWireEncoder: {
			nil.
			true.
			false.
			42.
			4.2.
			{ 1. 2. 3. }.
			{ 'one' -> 1.
				'two' -> 2. } asDictionary. }
		count: 10000 ].
]

{ #category : #tests }
GtWireEncodingPerformance class >> testMixed [

	^ self new testObject: {
		nil.
		true.
		false.
		42.
		4.2.
		{ 1. 2. 3. }.
		{ 'one' -> 1.
			'two' -> 2. } asDictionary. }
	count: 10000.
]

{ #category : #support }
GtWireEncodingPerformance >> testLeJsonV4: anObject count: anInteger [ 

	^ [ anInteger timesRepeat:
		[ | string |
		string := LeJsonV4 uniqueInstance serialize: anObject.
		LeJsonV4 uniqueInstance deserialize: string readStream ]
	] timeToRun
]

{ #category : #support }
GtWireEncodingPerformance >> testObject: anObject count: anInteger [

	^ {
		#wireEncoder -> (self testWireEncoder: anObject count: anInteger).
		#leJsonV4 -> (self testLeJsonV4: anObject count: anInteger).
		#ston -> (self testSton: anObject count: anInteger).
	} asDictionary.
]

{ #category : #support }
GtWireEncodingPerformance >> testSton: anObject count: anInteger [ 

	^ [ anInteger timesRepeat:
		[ | string |
		string := STON toString: anObject.
		STON fromString: string ]
	] timeToRun
]

{ #category : #support }
GtWireEncodingPerformance >> testWireEncoder: anObject count: anInteger [

	^ [ anInteger timesRepeat:
		[ | encoded |
		encoded := (GtWireEncoder onByteArray nextPut: anObject) contents.
		(GtWireDecoder on: encoded readStream) next ]
	] timeToRun
]
