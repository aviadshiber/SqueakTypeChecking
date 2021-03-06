Class subclass: #MyObject
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'OOP3'!


!MyObject methodsFor: 'private' stamp: 'initalize 5/23/2017 20:25'!
instRespondsTo: aSymbol
^ ((self class) classCanUnderstand:aSymbol).! !

"-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- "!

MyObject class
	instanceVariableNames: ''!

!MyObject class methodsFor: 'AbstractHandling' stamp: 'initalize 5/24/2017 20:27'!
changeToAbstract:original
|result|
result := ''.
(ReadStream on: original) do: [ :currentChar | 
	result:=result,currentChar.
	(currentChar=Character cr) ifTrue:[
		result:=result,'MyObject throwAbstractMethodInvoked.'.
		(self getTags:original) do:[:tag | result:=result,'"@',tag,'"'].
		^result]
	].! !

!MyObject class methodsFor: 'AbstractHandling' stamp: 'initalize 5/25/2017 08:10'!
isMethod: aSymbol abstractIn: aClass
	"The method return true if the symbol which represent a method is an abstract method, and false otherwise"
	| aSuperclass tagSet|
	 aSuperclass := aClass.
	((aSuperclass lookupSelector:aSymbol) = nil) ifTrue:[^false].
	  [self isEndOfHierachy:aSuperclass]
		whileFalse: 
			[(aSuperclass includesSelector:aSymbol) ifTrue:[
			tagSet := self getTags:((aSuperclass sourceMethodAt:aSymbol) asString).
			(tagSet includes:'Abstract') ifTrue:[^true].
			(tagSet includes:'Override') ifTrue:[^false]].
			aSuperclass := aSuperclass superclass].
	^false.

! !

!MyObject class methodsFor: 'AbstractHandling' stamp: 'initalize 5/25/2017 08:27'!
isMethod: aSymbol finalIn: aClass
	^ self isMethod: aSymbol in: aClass collidWith: 'Final' and: 'New'.
! !

!MyObject class methodsFor: 'AbstractHandling' stamp: 'initalize 5/25/2017 08:28'!
isMethod: aSymbol in: aClass collidWith: firstAnnotation and: secondAnnotation
	"The method return true if the symbol which represent a method is a private method, and false otherwise"
	| aSuperclass tagSet|
	 aSuperclass := aClass.
	((aSuperclass lookupSelector:aSymbol) = nil) ifTrue:[^false].
	  [self isEndOfHierachy:aSuperclass]
		whileFalse: 
			[(aSuperclass includesSelector:aSymbol) ifTrue:[
			tagSet := self getTags:((aSuperclass sourceMethodAt:aSymbol) asString).
			(tagSet includes: firstAnnotation) ifTrue:[^true].
			(tagSet includes: secondAnnotation) ifTrue:[^false]].
			aSuperclass := aSuperclass superclass].
	^false.
! !

!MyObject class methodsFor: 'AbstractHandling' stamp: 'initalize 5/25/2017 08:26'!
isMethod: aSymbol privateIn: aClass
	"The method return true if the symbol which represent a method is a private method, and false otherwise"
	^ self isMethod: aSymbol in: aClass collidWith: 'Private' and: 'New'.
! !

!MyObject class methodsFor: 'AbstractHandling' stamp: 'initalize 5/23/2017 12:25'!
new
	(MyObject isAbstractClass:(self)) ifTrue:[self throwCanNotInitAbstract].
	^(super new).! !


!MyObject class methodsFor: 'TagsHelper' stamp: 'rj 5/25/2017 18:04'!
char:ch existOnLine:line in:aString
	"the method return a boolean if  ch exist on line in aString"
	|lineNumber stringArray|
	lineNumber:=1.
	stringArray := aString lines.
	stringArray do:[:row| 
		((((row asString) findString:(ch asString)) >0 ) and:[lineNumber==line]) ifTrue:[^true]. 
		lineNumber:=lineNumber+1.].
	^false.! !

!MyObject class methodsFor: 'TagsHelper' stamp: 'rj 5/19/2017 16:53'!
extractTagContent: aTag
	"the method get aTag for example @SomeTag and return SomeTag"
	|tagSize tagContent|
	tagSize:= (aTag size).
	tagContent:= aTag copyFrom: 3 to: tagSize-1.
	^tagContent.! !

!MyObject class methodsFor: 'TagsHelper' stamp: 'rj 5/25/2017 18:10'!
findChar:ch in:aString
	"the method return a the line number which ch first appears in aString"
	|lineNumber stringArray|
	lineNumber:=1.
	stringArray := aString lines.
	stringArray do:[:line| 
		(((line asString) findString:(ch asString)) >0 ) ifTrue:[^lineNumber]. 
		lineNumber:=lineNumber+1.].
	^-1.! !

!MyObject class methodsFor: 'TagsHelper' stamp: 'initalize 5/24/2017 23:19'!
getTags:aString
"a class method that get a String and returns a set of all the tags in that String without any special symbols, if no tags 	where found an empty Set is returned"
"example of such set will be:   {'New','Final','ArgsType'}"
|SetToReturn realString|
SetToReturn := Set new.
realString:=(aString asString).
((realString findString: '"@New"') > 0) ifTrue: [SetToReturn add:'New'].
((realString findString: '"@Final"') > 0) ifTrue: [SetToReturn add:'Final'].
((realString findString: '"@Override"') > 0) ifTrue: [SetToReturn add:'Override'].
((realString findString: '"@Private"') > 0) ifTrue: [SetToReturn add:'Private'].
((realString findString: '"@Abstract"') > 0) ifTrue: [SetToReturn add:'Abstract'].
((realString findString: '"@RetType:') > 0) ifTrue: [SetToReturn add:'RetType'].
((realString findString: '"@ArgsTypes:') > 0) ifTrue: [SetToReturn add:'ArgsTypes'].
^SetToReturn.! !


!MyObject class methodsFor: 'Private' stamp: 'initalize 5/24/2017 20:27'!
addPrivateCheck:methodText
"the method inject privacy check to methodText"
| toInject result parser methodName|

	parser:= Parser new.
	methodName:=parser parseSelector:methodText.
	
	toInject:='	
		(
			(	(thisContext client) class	)  includesLocalSelector: (#',methodName,')
		)
		 ifFalse:[
			MyObject throwCanNotAccessMethod.
			].'.
	
		result:= self inject:toInject toMethod:methodText.
		^result.! !

!MyObject class methodsFor: 'Private' stamp: 'initalize 5/25/2017 08:12'!
classCanUnderstand: aSymbol
	| aSuperclass tagSet|
	 aSuperclass := self.
	((aSuperclass lookupSelector:aSymbol) = nil) ifTrue:[^false]. "RULE #1"
	(MyObject isMethod:aSymbol abstractIn:self) ifTrue:[^false]. "RULE #2"
	(aSuperclass includesSelector:aSymbol) ifTrue:[tagSet := self getTags:((aSuperclass sourceMethodAt:aSymbol) asString).
	(tagSet includes:'Private') ifTrue:[^true]].  "part 2 of RULE #3"
	 [self isEndOfHierachy:aSuperclass]
		whileFalse: "Part 1+3 of RULE #3"
			[(aSuperclass includesSelector:aSymbol) ifTrue:[
			tagSet := self getTags:((aSuperclass sourceMethodAt:aSymbol) asString).
			(tagSet includes:'Private') ifTrue:[^false].
			(tagSet includes:'New') ifTrue:[^true]].
			aSuperclass := aSuperclass superclass.
		]. 
	
	^true.! !


!MyObject class methodsFor: 'as yet unclassified' stamp: 'initalize 5/25/2017 08:15'!
isAbstractClass: aClass
	| aSuperclass |
	 aSuperclass := aClass.
	
	[self isEndOfHierachy:aSuperclass]
		whileFalse: [
			aSuperclass selectorsDo:[:sel | (self isMethod:sel abstractIn:aClass) ifTrue:[^true]].
			aSuperclass := aSuperclass superclass.
			].
	^false.


"(aClass allSelectors) do:[:sel | (MyObject isMethod:sel abstractIn:aClass) ifTrue:[^true]].
^false."! !


!MyObject class methodsFor: 'injection' stamp: 'rj 5/19/2017 18:27'!
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

!MyObject class methodsFor: 'injection' stamp: 'rj 5/19/2017 18:27'!
extractArgs: aString
	"the method will extract the arguments to a orderedCollection of a method which will be returned."
	| parser argsAsOrderedCollection |
	parser:=Parser new.
	"using the parser to get the aguments names"
	argsAsOrderedCollection := parser parseParameterNames: aString.
	^argsAsOrderedCollection.! !

!MyObject class methodsFor: 'injection' stamp: 'initalize 5/24/2017 21:14'!
extractReturn:aString
	"the method will extract the returned type from aString. if not found Nil will be returned"
	| regexObj returnedValue  |
	regexObj := '\^([^.]*)\s*' asRegex.
	regexObj search: (aString asString).
	returnedValue := (regexObj  subexpression:2). 
	^returnedValue.
	! !

!MyObject class methodsFor: 'injection' stamp: 'initalize 5/24/2017 19:11'!
extractTypes: aString annotatedBy: annotation
	"the method will extract the types of ArgTypes from a String"
	|regexObj typesInString orderedTypes regexString|
	"the ordeder of the types is imprtant so we save it in an orderedCollection"
	orderedTypes := OrderedCollection new.
	"@New"
	"pulling all the types in argTypes via regex, including spaces"
	
	regexString:='"@',annotation,'\:([^\.]+)\."'.
	regexObj :=  regexString asRegex.
	regexObj search: aString.
	typesInString := (regexObj subexpression: 2) asString.
	"adding the types to the collection which match with a word"
	typesInString regex: '\w+' matchesCollect:[:each | orderedTypes add: each].
	
	^orderedTypes.
	
	
	! !

!MyObject class methodsFor: 'injection' stamp: 'initalize 5/24/2017 19:47'!
inject:code toMethod: aString
	"the method return the method string with the injected code"
	| result targetLine pipeExist commentExist blockExist |
targetLine :=2.
pipeExist :=self char:($|) existOnLine:targetLine in:aString.
commentExist :=self char:($") existOnLine:targetLine in:aString.
blockExist :=self char:($[) existOnLine:targetLine in:aString.

(pipeExist and:[commentExist not] and:[blockExist not]) ifTrue:[targetLine:=targetLine+1].

"now we have a good target, and we are need to look for the target line and inject it"
result:=self inject:code toMethod:aString inLine:targetLine.


^result.


! !

!MyObject class methodsFor: 'injection' stamp: 'rj 5/25/2017 18:09'!
inject:code toMethod: aString inLine: targetLine
	"the method return the method string with the injected code"
	|lineNumber wasAdded result stringArray|
lineNumber:=1.
wasAdded:=false.
result:=''.
	stringArray := aString lines.
	stringArray do:[:line|  (lineNumber == targetLine) ifTrue:[result:=result,code,(Character cr)] 
		.lineNumber:=lineNumber+1.result:=result,line,(Character cr)].
(lineNumber <= targetLine) ifTrue:[result:=result,code,(Character cr)].
^result.


! !


!MyObject class methodsFor: 'compiling' stamp: 'initalize 5/24/2017 16:45'!
addNewToSubclasses:selector in:aClass
|methodCode tagSet|
aClass allSubclassesDo:[:sub |
	 (sub includesSelector:selector) ifTrue:[ "method exist on sub"
										methodCode :=  ((sub sourceMethodAt:selector) asString). "get method code"
										tagSet := self getTags:methodCode. 
										((tagSet includes:'New') or:[tagSet includes:'Override']) ifFalse:[ "if any subclass does not have @Override or @New we add @New"
										methodCode := (MyObject inject:'"@New"' toMethod:methodCode).
										sub compile:methodCode] "end of trueBlock"
				]."end of outer trueBlock"

		]. "end of do block"
! !

!MyObject class methodsFor: 'compiling' stamp: 'initalize 5/24/2017 22:31'!
compile: text classified: category notifying: requestor
	"the following method should:"
	"check for errors"
	"get the tags"
	"change code"
	|checkArgumentsEX checkReturnEx  tagSet orderedArgs orderedTypes returnValue returnTypes returnValueCollection argAnnotation returnAnnotation methodResult returnChar returnLine  methodName|
	
	methodName:=self getMethodName:text.
	
	tagSet:=MyObject getTags:text.
	
	self validateBadTags:tagSet."validation of Bad Tags (ERROR 1)"
	
	self validateMethodExistence:methodName WithTags:tagSet. "validate tags in heirarchy (ERROR 2,3)"

	self validateInvalidHidingWithinMethod:methodName withTags:tagSet. "ERROR 4"
	
	self validateOverrdingPrivateWithinMethod:methodName withTags:tagSet. "ERROR 5"
	
	self validateOverrdingFinalWithinMethod:methodName withTags:tagSet. "ERROR 6"
	
	
	
	"need to check for type errors"
	tagSet isEmpty ifTrue:[
							super compile: text classified: category notifying: requestor.
							self addNewToSubclasses:methodName in:self. "last part in the hw"
	]ifFalse:["tag set is not empty we should check and inject current method"
							checkArgumentsEX:=''.
							checkReturnEx:=''.
							argAnnotation :='ArgsTypes'.
							returnAnnotation := 'RetType'.
							returnValueCollection := OrderedCollection new.
							orderedArgs := self extractArgs:text.
							orderedTypes:= self extractTypes:text annotatedBy: argAnnotation.
							returnTypes:= self extractTypes:text annotatedBy: returnAnnotation.
							returnValue := self extractReturn:text.
							(returnValue isNil) ifTrue:[returnValue:='self'].
							
							
							(tagSet includes:argAnnotation) ifTrue:[checkArgumentsEX:=self validate:orderedArgs withTypes:orderedTypes].
							checkArgumentsEX :=checkArgumentsEX,'.'.
							"we add the return value to the collection to use the same validation method"
							returnValueCollection add:returnValue.
	
							(tagSet includes:returnAnnotation) ifTrue:[checkReturnEx:= self validate:returnValueCollection withTypes:returnTypes].
								checkReturnEx := checkReturnEx,'.'.
								"inject the return check to the method,as next line after delceration"
								 returnChar:=$^.
								"we need to find the line to inject the return value"
								returnLine := self findChar:returnChar in:text.
								methodResult:=text.
								(returnLine>0) ifTrue:[
									methodResult := (self inject: checkReturnEx toMethod: methodResult inLine:returnLine).
								].

							methodResult := (self inject: checkArgumentsEX toMethod: methodResult).
							(tagSet includes:'Abstract') ifTrue:[methodResult:=self changeToAbstract: text].
							(tagSet includes:'Private') ifTrue:[methodResult:=self addPrivateCheck: methodResult].
							super compile: methodResult classified: category notifying: requestor.
	]."tagSet is not Empty End" ! !

!MyObject class methodsFor: 'compiling' stamp: 'initalize 5/24/2017 09:57'!
getMethodName:methodText
	"the method returns the method name of methodText"
	| parser methodName|
	parser:= Parser new.
	methodName :=parser parseSelector:methodText.
	^methodName.! !

!MyObject class methodsFor: 'compiling' stamp: 'initalize 5/25/2017 08:41'!
isEndOfHierachy:aClass
	^aClass==nil or:[aClass == (MyObject superclass)].! !

!MyObject class methodsFor: 'compiling' stamp: 'initalize 5/24/2017 13:08'!
isNewAfterAbstract:selector in:aClass
	"Look down the given selector in my methodDictionary.
	Return the corresponding method if found.
	Otherwise chase the subclass chain and try again.
	Return nil if no method is found."
	| sub  methodCode|
	sub := aClass subclasses. "array of all subclasses"
	(sub = #() ) ifTrue:[^false].
	sub do:[:subClazz | 
		(subClazz includesSelector:selector) ifTrue:[
			methodCode :=  ((subClazz sourceMethodAt:selector) asString).
			((MyObject getTags:methodCode) includes:'New') ifTrue:[^true] ifFalse:[^false].
			
			] ifFalse:[
			(self isNewAfterAbstract:selector in: subClazz) ifTrue:[^true]
			].
		].
	^false.! !

!MyObject class methodsFor: 'compiling' stamp: 'initalize 5/24/2017 22:22'!
isOverrideNext:selector in:aClass
	| sub  methodCode|
	sub := aClass subclasses. "array of all subclasses"
	(sub = #() ) ifTrue:[^false].
	sub do:[:subClazz | 
		(subClazz includesSelector:selector) ifTrue:[ |tagSet|
			methodCode :=  ((subClazz sourceMethodAt:selector) asString).
			tagSet:=MyObject getTags:methodCode.
			(tagSet includes:'Override') ifTrue:[^true] ifFalse:[(tagSet includes:'New') ifTrue:[^false]].
			] ifFalse:[
			(self isOverrideNext:selector in: subClazz) ifTrue:[^true]
			].
		].
	^false.! !

!MyObject class methodsFor: 'compiling' stamp: 'initalize 5/24/2017 20:27'!
validate: orderedArgs withTypes: orderedTypes
	"the method validates the ordered collection to be with the same kind of type,
	 on compile time and runtime. the method will return the runtime check to inject"
	|argCheckText  i |
	(orderedTypes isNil) ifTrue:[self throwWrongType].
	((orderedArgs size>0) and:[ orderedArgs size ~= orderedTypes size ] ) ifTrue:[ self throwWrongType.].
	"arguments and the types are on the same size"
	"inorder to check their types we will return a block check that will be activated on runtime"
	i :=1.
	argCheckText := '('.
	[i<=orderedArgs size] whileTrue:[ | arg type |
	arg := orderedArgs at:i.
	type := orderedTypes at:i.
	argCheckText:=argCheckText,'(',arg,' isKindOf:',type,') & '.
	i:=i+1.
	].
	argCheckText:=argCheckText,'true)'. "to end concanting right, and to handle it to be default true"
	argCheckText:=argCheckText,'ifFalse:[MyObject throwWrongType].'.
	^argCheckText.! !

!MyObject class methodsFor: 'compiling' stamp: 'initalize 5/24/2017 09:44'!
validateBadTags:tagSet
	((tagSet includes: 'Abstract') & (tagSet includes: 'Final'))  ifTrue: [self throwBadTags]. 
	((tagSet includes: 'Override') & (((tagSet includes: 'New') | (tagSet includes: 'Private'))))  ifTrue: [self throwBadTags]. "ERROR 1"! !

!MyObject class methodsFor: 'compiling' stamp: 'initalize 5/24/2017 13:50'!
validateInvalidHidingWithinMethod:methodName withTags:tagSet
	"ERROR 4"
	((self isMethod:methodName abstractIn:(self superclass)) and:[tagSet includes:'New']) ifTrue:[self throwCanNotHideAbstractMethod]. 	"Err 4 Pt2"
	((tagSet includes: 'Abstract') and:[(self isNewAfterAbstract:methodName in:self)]) ifTrue:[self throwCanNotHideAbstractMethod]. ! !

!MyObject class methodsFor: 'compiling' stamp: 'initalize 5/24/2017 13:57'!
validateMethodExistence:methodName WithTags:tagSet
	(((self superclass) lookupSelector:methodName) = nil) ifFalse:[((tagSet includes:'New')| (tagSet includes:'Override'))
		 ifFalse:[self throwMethodAlreadyExist]]  "ERROR 2"
	ifTrue:[((tagSet includes:'New')| (tagSet includes:'Override')) ifTrue:[self throwMethodDoesNotExist]].   "ERROR 3"! !

!MyObject class methodsFor: 'compiling' stamp: 'initalize 5/24/2017 16:26'!
validateOverrdingFinalWithinMethod:methodName withTags:tagSet
	"Error 6"
	(self isMethod:methodName finalIn:(self superclass)) ifTrue:[(tagSet includes:'Override') ifTrue:[self throwCanNotOverrideFinal]].
	"Err 6 Pt2"
	(tagSet includes: 'Final') ifTrue:[(self isOverrideNext:methodName in:self) ifTrue:[self throwCanNotOverrideFinal]].! !

!MyObject class methodsFor: 'compiling' stamp: 'initalize 5/24/2017 16:24'!
validateOverrdingPrivateWithinMethod:methodName withTags:tagSet
	"ERROR 5"
	(self isMethod:methodName privateIn:(self superclass)) ifTrue:[(tagSet includes:'Override') ifTrue:[self throwCanNotOverridePrivate]].
	"Err 5 Pt2"
	(tagSet includes: 'Private') ifTrue:[(self isOverrideNext:methodName in:self) ifTrue:[self throwCanNotOverridePrivate]].! !


!MyObject class methodsFor: 'errors' stamp: 'initalize 5/24/2017 20:25'!
throwAbstractMethodInvoked
	AssertionFailure signal:'Abstract method called!!'.! !

!MyObject class methodsFor: 'errors' stamp: 'initalize 5/24/2017 16:50'!
throwBadTags
	"the method thow bad tag tags error"
	AssertionFailure signal: 'Bad Tags!!'.! !

!MyObject class methodsFor: 'errors' stamp: 'initalize 5/24/2017 16:51'!
throwCanNotAccessMethod
	AssertionFailure signal: 'Cannot access method!!'.! !

!MyObject class methodsFor: 'errors' stamp: 'initalize 5/24/2017 16:51'!
throwCanNotHideAbstractMethod
	AssertionFailure signal: 'Cannot hide Abstract method!!'.! !

!MyObject class methodsFor: 'errors' stamp: 'initalize 5/24/2017 16:51'!
throwCanNotInitAbstract
	AssertionFailure signal: 'Cannot instantiate abstract class!!'.! !

!MyObject class methodsFor: 'errors' stamp: 'initalize 5/24/2017 16:51'!
throwCanNotOverrideFinal
	AssertionFailure signal:'Cannot override Final method!!'.! !

!MyObject class methodsFor: 'errors' stamp: 'initalize 5/24/2017 16:51'!
throwCanNotOverridePrivate
	AssertionFailure signal: 'Cannot override Private method!!'.! !

!MyObject class methodsFor: 'errors' stamp: 'initalize 5/24/2017 16:51'!
throwCanNotOvverrideFinal
	AssertionFailure signal: 'Cannot override Final method!!'.! !

!MyObject class methodsFor: 'errors' stamp: 'initalize 5/24/2017 16:52'!
throwMethodAlreadyExist
	AssertionFailure signal:'Method already exists!!'.! !

!MyObject class methodsFor: 'errors' stamp: 'initalize 5/24/2017 16:52'!
throwMethodDoesNotExist
	AssertionFailure signal: 'Method does not exist!!'.! !

!MyObject class methodsFor: 'errors' stamp: 'initalize 5/24/2017 16:52'!
throwWrongType
	AssertionFailure signal: 'Wrong Type!!'.! !
