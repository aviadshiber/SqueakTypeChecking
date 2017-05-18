Class subclass: #MyObject
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'OOP3'!
  
  

!MyObject methodsFor: 'analzying' stamp: 'initalize 5/15/2017 22:27'!
extractArgs: aString
	"the method will extract the arguments to a orderedCollection of a method which will be returned."
	| parser argsAsOrderedCollection |
	parser:=Parser new.
	"using the parser to get the aguments names"
	argsAsOrderedCollection := parser parseParameterNames: aString.
	^argsAsOrderedCollection.! !

!MyObject methodsFor: 'analzying' stamp: 'initalize 5/16/2017 12:35'!
extractReturn:aString
	"the method will extract the returned type from aString. if not found Nil will be returned"
	| regexObj returnedValue  |
	regexObj := '\^([^.]*)\.' asRegex.
	regexObj search: aString.
	returnedValue := (regexObj  subexpression:2). 
	^returnedValue.
	! !

!MyObject methodsFor: 'analzying' stamp: 'initalize 5/16/2017 13:07'!
extractTypes: aString annotatedBy:annotation
	"the method will extract the types of ArgTypes from a String"
	|regexObj typesInString orderedTypes |
	"the ordeder of the types is imprtant so we save it in an orderedCollection"
	orderedTypes := OrderedCollection new.
	
	"pulling all the types in argTypes via regex, including spaces"
	regexObj :=  '"@',annotation,':\s*(.*\s*\w+)\."' asRegex.
	typesInString := ((regexObj search: aString) subexpression: 2).
	"adding the types to the collection which match with a word"
	typesInString regex: '\w+' matchesCollect:[:each | orderedTypes add: each].
	
	^orderedTypes.
	
	
	! !

!MyObject methodsFor: 'analzying' stamp: 'initalize 5/16/2017 19:07'!
validate: orderedArgs withTypes: orderedTypes
	"the method validates the ordered collection to be with the same kind of type,
	 on compile time and runtime. the method will return the runtime check to inject"
	|argCheckText  i |
	(orderedTypes isNil) ifTrue:[self throwWrongType].
	((orderedArgs size>0) and:[ orderedArgs size ~= orderedTypes size ] ) ifTrue:[ self throwWrongType.].
	"arguments and the types are on the same size"
	"inorder to check their types we will return a block check that will be activated on runtime"
	i :=0.
	argCheckText := '('.
	[i<orderedArgs size] whileTrue:[ | arg type |
	arg := orderedArgs at:i.
	type := orderedTypes at:i.
	argCheckText:=argCheckText,arg,' isKindOf:',type,' & '.
	].
	argCheckText:=argCheckText,'true)'. "to end concanting right, and to handle it to be default true"
	argCheckText:=argCheckText,'ifFalse[self throwWrongType].'.
	^argCheckText.! !


!MyObject methodsFor: 'injecting' stamp: 'initalize 5/18/2017 11:38'!
char:ch existOnLine:line in:aString
	"the method return a boolean if  ch exist on line in aString"
	|lineNumber hadBeenCommented |
	lineNumber:=1.
	(ReadStream on: aString) do: [ :currentChar | 
	(lineNumber=line and:[currentChar=ch ]) ifTrue:[ ^(hadBeenCommented:=true) ].
	(lineNumber>line) ifTrue:[^false].
	(currentChar=Character cr) ifTrue:[lineNumber :=  lineNumber+1].
	]."end of do block"
	^false.! !

!MyObject methodsFor: 'injecting' stamp: 'initalize 5/18/2017 11:19'!
commentExistOnSecondLine:aString
	"the method return a boolean if comment exist on second line of a method (aString)"
	|lineNumber hadBeenCommented targetChar |
	targetChar:=$".
	lineNumber:=1.
	(ReadStream on: aString) do: [ :currentChar | 
	
	
	(lineNumber=2 and:[currentChar=targetChar ]) ifTrue:[ ^(hadBeenCommented:=true) ].
	(lineNumber>2) ifTrue:[^false].
	(currentChar=Character cr) ifTrue:[lineNumber :=  lineNumber+1].
	]."end of do block"
	^false.! !

!MyObject methodsFor: 'injecting' stamp: 'initalize 5/18/2017 12:28'!
inject:code toMethod: aString
	"the method return the method string with the injected code"
	| lineNumber wasAdded result targetLine commentChar pipeLineChar isCommentExist isPipeLineExist|
pipeLineChar:=$|.
commentChar:=$".
targetLine:=2.
isCommentExist := (self char: commentChar existOnLine: targetLine in:aString).
isPipeLineExist := [ self char: pipeLineChar existOnLine: targetLine in:aString ].
"if not comment exist on second line and there is a pipeline than there are local vars there. so we increase the targetLine by one"
(     (isCommentExist not)   and:isPipeLineExist         ) 		ifTrue: [ targetLine:=targetLine+1 ].
"if comment exist on second line than there is no local vars according to FAQ so we are fine so far... but just in case that comment exist we still want to increase the target line"
(isCommentExist)  ifTrue:[ 
						targetLine:=targetLine+1.  
						"and after this comment there is a local vars we need to increase it again"
						(isPipeLineExist value) ifTrue:[targetLine:=targetLine+1].
						].
"now we have a good target, and we are need to look for the target line and inject it"
lineNumber:=1.
wasAdded:=false.
result:=''.
(ReadStream on: aString) do: [ :currentChar | 
	(lineNumber=targetLine and:[wasAdded=false]) ifTrue:[
												result:=result,code. 
												wasAdded:=true.
												     ].
	result:=result,currentChar.
	(currentChar=Character cr) ifTrue:[lineNumber :=  lineNumber+1].
	]."end of do block"
^result.
! !


!MyObject methodsFor: 'throws' stamp: 'initalize 5/16/2017 17:39'!
throwAbstractMethodInvoked
	self error:'Abstract method called!!'.! !

!MyObject methodsFor: 'throws' stamp: 'initalize 5/16/2017 17:31'!
throwBadTags
	"the method thow bad tag tags error"
	self error:'Bad Tags!!'.! !

!MyObject methodsFor: 'throws' stamp: 'initalize 5/16/2017 17:38'!
throwCanNotAccessMethod
	self error:'Cannot access method!!'.! !

!MyObject methodsFor: 'throws' stamp: 'initalize 5/16/2017 17:34'!
throwCanNotHideAbstractMethod
	self error:'Cannot hide Abstract method!!'.! !

!MyObject methodsFor: 'throws' stamp: 'initalize 5/16/2017 17:38'!
throwCanNotInitAbstract
	self error:'Cannot instantiate abstract class!!'.! !

!MyObject methodsFor: 'throws' stamp: 'initalize 5/16/2017 17:35'!
throwCanNotOverridePrivate
	self error:'Cannot override Private method!!'.! !

!MyObject methodsFor: 'throws' stamp: 'initalize 5/16/2017 17:37'!
throwCanNotOvverrideFinal
	self error:'Cannot override Final method!!'.! !

!MyObject methodsFor: 'throws' stamp: 'initalize 5/16/2017 17:32'!
throwMethodAlreadyExist
	self error:'Method already exists!!'.! !

!MyObject methodsFor: 'throws' stamp: 'initalize 5/16/2017 17:34'!
throwMethodDoesNotExist
	self error:'Method does not exist!!'.! !

!MyObject methodsFor: 'throws' stamp: 'initalize 5/16/2017 17:41'!
throwWrongType
	self error: 'Wrong Type!!'.! !


!MyObject methodsFor: 'compiling' stamp: 'initalize 5/18/2017 12:30'!
compile: text classified: category notifying: requestor
	"the following method should:"
	"check for errors"
	"get the tags"
	"change code"
	|checkArgumentsEX checkReturnEx  tagSet orderedArgs orderedTypes returnValue returnTypes returnValueCollection argAnnotation returnAnnotation methodResult |
	checkArgumentsEX:='('.
	checkReturnEx:='('.
	argAnnotation :='ArgsTypes'.
	returnAnnotation := 'returnAnnotation'.
	returnValueCollection := OrderedCollection new.
	tagSet:=MyObject getTags:text.
	orderedArgs := self extractArgs:text.
	orderedTypes:= self extractTypes:text annotatedBy: argAnnotation.
	returnTypes:= self extractTypes:text annotatedBy: returnAnnotation.
	returnValue := self extractReturn:text.
	(returnValue isNil) ifTrue:['self'].
	
	"need to check for type errors"
	tagSet isEmpty ifTrue:[
	"Transcript show: 'no tags were found skipping to normal compilation'."
	super compile: text classified: category notifying: requestor.
	]ifFalse:[
	"Transcript show:'tags were found in the method, starting to validate types';cr."
	"Transcript show:'starting to check types'."
	"validate that there they are on the same size on compile time"
	
	(tagSet includes:argAnnotation) ifTrue:[checkArgumentsEX:=self validate:orderedArgs withTypes:orderedTypes].
	checkArgumentsEX :=checkArgumentsEX,')'.
	"we add the return value to the collection to use the same validation method"
	returnValueCollection add:returnValue.
	(tagSet includes:returnAnnotation) ifTrue:[checkReturnEx:= self validate:returnValueCollection withTypes:returnTypes].
	checkReturnEx := checkReturnEx , ')'.
	"inject the return check to the method,as next line after delceration"
	methodResult:= (self inject: checkReturnEx toMethod: text).
	methodResult:= (self inject: checkArgumentsEX toMethod: methodResult).
	super compile: methodResult classified: category notifying: requestor.
	]."tagSet is not Empty End" ! !

"-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- "!

MyObject class
	instanceVariableNames: ''!

!MyObject class methodsFor: 'AbstractHandling' stamp: 'initalize 5/18/2017 12:39'!
isMethod: aSymbol abstractIn: aClass
	"The method return true if the symbol which represent a method is an abstract method, and false otherwise"
	
	"TODO: we should look for abstract annotation in the class of the method or one of it's parent. if no override to that method been done than it is abastract method"
^false.! !


!MyObject class methodsFor: 'TagsHelper' stamp: 'initalize 5/13/2017 21:53'!
extractTagContent: aTag
	"the method get aTag for example @SomeTag and return SomeTag"
	|tagSize tagContent|
	tagSize:= (aTag size).
	tagContent:= aTag copyFrom: 1 to: tagSize.
	^tagContent.! !

!MyObject class methodsFor: 'TagsHelper' stamp: 'initalize 5/13/2017 22:00'!
getTags:aString
	"a class method that get a String and returns a set of all the tags in that String without any special symbols, if no tags 	where found an empty Set is returned"
	"example of such set will be:   {'New','Final','ArgsType'}"
|regexString tagSet|
tagSet:=Set new.
"defining all the tags to extract in Regex"
regexString :='"@(Private|Override|New|Final|Abstract|ArgTypes:|RetType:)"'.
"iterate over the string matches and add it to the set"
aString regex: regexString matchesCollect: [:tag | tagSet add: (self extractTagContent:tag). ].
^tagSet.

! !
