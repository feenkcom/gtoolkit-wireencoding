Extension { #name : #DateAndTimeANSI }

{ #category : #'*GToolkit-WireEncoding-GemStone' }
DateAndTimeANSI >> asUnixTime [

	^ self asPosixSeconds
]

{ #category : #'*GToolkit-WireEncoding-GemStone' }
DateAndTimeANSI >> nanoSecond [

	^ (self second fractionPart * (10 raisedTo: 9)) rounded
]

{ #category : #'*GToolkit-WireEncoding-GemStone' }
DateAndTimeANSI >> setNanoSeconds: nanoSeconds [
	"Set the fractional seconds of the receiver"

	^ DateAndTime posixSeconds: self asPosixSeconds truncated + (nanoSeconds / (10 raisedTo: 9))
		offset: self offset
]
