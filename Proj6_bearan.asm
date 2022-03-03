TITLE Project 6: String Primitives and Macros     (Proj6_bearan.asm)

; Author: Andrew Bear
; Last Modified: 27Feb2022
; OSU email address: bearan@oregonstate.edu
; Course number/section:   CS271 Section 406
; Project Number: 6                Due Date: 13Mar2022
; Description:	This program centers around the creation of two procedures - ReadVal and WriteVal 
; that mimic the Irvine Library ReadInt and WriteInt, using macros and string primitives to achieve 
; this functionality without using any int- or dec-writing irvine prodecures. 

INCLUDE Irvine32.inc

; (insert macro definitions here)

;---------------------------------------------------------------------------------------------
; Name: mGetString
;
; Outputs prompt, and calls Irvine ReadString to read response into memory
;
; Receives: prompt -- OFFSET of prompt to display
;			targetLoc -- output location 
;			maxLength -- Length of user input (required for Irvine ReadString)
; Returns: screen output of string
;----------------------------------------------------------------------------------------------
mGetString MACRO prompt, targetLoc, maxLength
	PUSH	EAX
	PUSH	ECX						; preserve registers
	PUSH	EDX

; display prompt
	mDisplayString prompt

; set up and call ReadString
	MOV		EDX, targetLoc			; set up and call ReadString
	MOV		ECX, maxLength
	CALL	ReadString


	POP		EDX						; restore registers
	POP		ECX
	POP		EAX
ENDM

;---------------------------------------------------------------------------------------------
; Name: mDisplayString
;
; Calls WriteString on the supplied address
;
; Receives: OFFSET of string to display
; Returns: screen output of string
;----------------------------------------------------------------------------------------------
mDisplayString MACRO stringOffset		; param - offset of string to print
	PUSH	EDX							; uses EDX so we protect it

	MOV		EDX, stringOffset			; standard writeString call
	CALL	WriteString

	POP		EDX
ENDM

; (insert constant definitions here)

ARRAYLENGTH		=	10					; numer of ints to get
MAXCHARS		=	11					; a SDWORD int can only be 11 chars long, -2,147,483,648, plus padding 0
USERSTRLENGTH	=	16					; the user gets this much space to enter characters. More than they need.


.data
; greeting/program/exit prompts
greeting	BYTE	"Project 6: String Primitives and Macros.",13,10
			BYTE	"Programmed by A. Bear",13,10,13,10
			BYTE	"**EC: Numbers the entry lines and displays a running total.",13,10
			BYTE	"**EC: HAHAHAHAAH MAYBE IN ANOTHER LIFE.",13,10,13,10
			BYTE	"This program will ask you for 10 signed decimal integers.",13,10
			BYTE	"Each integer must fit into a 32-bit register (-2,147,483,648 to 2,147,483,647).",13,10
			BYTE	"Once you're done entering integers, the program will display a list",13,10
			BYTE	"of the integers, the sum of the integers, and the average of the values.",13,10
			BYTE	"Salmon on the bank / curing in the sun for days / eat them up yum yum ~A bear",13,10,13,10,0
numEntry	BYTE	": Please enter a signed integer: ",0
invalidNum	BYTE	32,32,32,"Something's wrong with your entry.",13,10,0
tryAgain	BYTE	32,32,32,"Please try again: ",0
numsEntered	BYTE	13,10,"Here are your numbers, all in a nice row:",13,10,0
numSum		BYTE	13,10,"The total sum of your numbers is: ",0
numAverage	BYTE	13,10,"The truncated average of your numbers is: ",0
byePrompt	BYTE	13,10,13,10,"Goodbye, and thanks for all the fish! ROAR!",13,10,0

subTotMsg	BYTE	32,32,32,"Number Accepted! Your subtotal so far is: ",0

oBracket	BYTE	"[",0
cBracket	BYTE	"]",0
comma		BYTE	", ",0
linebreak	BYTE	13,10,0

; variables
numArray	SDWORD	ARRAYLENGTH DUP(?)			; array for holding the user's entered values

; variables for readVal
specialNum	BYTE	"-2147483648",0,0,0,0,0,0   ; This damn number. Plus padding zeroes
validVal	SDWORD	1							; used as flag to check for valid readVal return
userVal		SDWORD	0							; user's converted value will be here
userString	BYTE	USERSTRLENGTH DUP(?)		; string to hold the string user enters.
padding		BYTE	5 DUP(0)					; don't want overrun by some fluke

; variables for writeVal
tempArray	BYTE	(MAXCHARS + 2) DUP(0)		; array used as scratch by writeVal -- 12 characters, plus padding 0

; EC variables
runTotal	SDWORD	?							; running total sum

; final calcs
sumValues	SDWORD	?
avgValues	SDWORD	?


.code
main PROC
; display greeting prompt
	mDisplayString	OFFSET greeting

; ===============================================================
; Here is the loop to fill the array
; ===============================================================
	MOV		EDI, OFFSET numArray			; dest is start of array
	MOV		ECX, 1

; loop re-entrance point
_fillArray:
; number the output line and display prompt
	MOV		EAX, ECX						; current line

	PUSH	OFFSET tempArray				; scratch array for working in
	PUSH	EAX								; push value to print out
	CALL	writeVal						; actual CALL

; make readVal call to get user input -- stored in userVal if successful
	PUSH	OFFSET numEntry					; EBP+24 -- prompt to print for entry
	PUSH	OFFSET specialNum				; EBP+20 -- the special case number I hate.
	PUSH	OFFSET validVal					; EBP+16 -- BOOLEAN FOR SUCCESS/FAILURE
	PUSH	OFFSET userVal					; EBP+12
	PUSH	OFFSET userString				; ebp+8
	CALL	readVal

; check result boolean for valid value
	CMP		validVal, 1
	JE		_validValue

_invalidValue:
; invalid value found, ask again:
	mDisplayString OFFSET invalidNum

	; make readVal call to get user input -- stored in userVal if successful
	PUSH	OFFSET tryAgain					; EBP+24 -- prompt to print for entry
	PUSH	OFFSET specialNum				; EBP+20 -- the special case number I hate.
	PUSH	OFFSET validVal					; EBP+16 -- BOOLEAN FOR SUCCESS/FAILURE
	PUSH	OFFSET userVal					; EBP+12
	PUSH	OFFSET userString				; ebp+8
	CALL	readVal

	CMP		validVal, 0
	JE		_invalidValue


_validValue:
	MOV		EAX, userVal
	
	MOV		[EDI], EAX

	ADD		runTotal, EAX
	
	INC		ECX								; next loop value
	ADD		EDI, TYPE numArray

	CMP		ECX, ARRAYLENGTH
	JG		_doneFill


; Display running subtotal
	mDisplayString	OFFSET subTotMsg
	MOV		EAX, runTotal

	PUSH	OFFSET tempArray				; scratch array for working in
	PUSH	EAX								; push value to print out
	CALL	writeVal						; actual CALL

	mDisplayString	OFFSET linebreak		; I BUILT THIS MACRO I'M GONNA USE IT
	mDisplayString	OFFSET linebreak		; CALL CRLF IS FOR CHUMPS

	JMP		_fillArray

_doneFill:

; ===============================================================
; Here we display the resulting array, and the total and average
; ===============================================================
	mDisplayString	OFFSET numsEntered		; array display prompt
	
	mDisplayString	OFFSET oBracket			; opening bracket

	MOV		ESI, OFFSET numArray			; offset of first element
	MOV		ECX, ARRAYLENGTH				; count of loops to run

; loop to display elements
_displayLoop:
	LODSD									; load the element to EAX
	ADD		sumValues, EAX					; add it to our sum

; display the current element
	PUSH	OFFSET tempArray				; scratch array for working in
	PUSH	EAX								; push value to print out
	CALL	writeVal						; actual CALL

; test if last loop, 
	CMP		ECX, 1
	JE		_endDisplay

; if not, write comma and loop again
	mDisplayString	OFFSET comma
	LOOP	_displayLoop

; terminating the array, print the closing bracket 
_endDisplay:
	mDisplayString	OFFSET cBracket			; print closing bracket

; here we display the sum
	mDisplayString 	OFFSET numSum			; sum prompt

	MOV		EAX,	sumValues				; sum to EAX

	PUSH	OFFSET tempArray				; print sum of values
	PUSH	EAX
	CALL	writeVal

; here we display the average
	mDisplayString	OFFSET numAverage		; avg prompt

	CDQ										; div by ARRAYLENGTH to find average
	MOV		EBX,	ARRAYLENGTH
	IDIV	EBX

	PUSH	OFFSET tempArray				; scratch array for working in
	PUSH	EAX								; we don't care about fractions, just print quotient
	CALL	writeVal						; actual CALL
	
	mDisplayString	OFFSET byePrompt

	Invoke	ExitProcess,0	; exit to operating system
main ENDP


; (insert additional procedures here)
;---------------------------------------------------------------------------------------------
; Name: readVal
;
; Prompts user for value. Reads value as string, using mGetString macro.
;
; Preconditions: 
; Postconditions: 
; Receives:			[EBP + 24] -- prompt to display
;					[EBP + 20] -- our special boah -2147483648 in string form
;					[EBP + 16] -- validVal boolean
;					[EBP + 12] -- userVal variable; will be filled with SDWORD conversion
;								  (iff successful!)
;					[EBP + 8] -- userString; OFFSET of array to store the user's string 
; Returns:			
;----------------------------------------------------------------------------------------------
readVal PROC
; prep stack, save registers
	LOCAL	isValid:DWORD, localValue:DWORD, posOrNeg:DWORD, stringLength:DWORD, specialBoah:BYTE
	PUSHAD

; mov 0 to isValid, and store it. Makes sure we have a failure by default
	MOV		isValid, 0
	MOV		EAX, isValid
	MOV		EBX, [EBP + 16]
	MOV		[EBX], EAX

	MOV		localValue, 0
	MOV		posOrNeg, 0								; -1 if first char is -, 1 if first char is +, 0 if first char is 0-9

; get the string from the user woth mGetString
	mGetString	[EBP+24], [EBP + 8], USERSTRLENGTH
	

; ===============================================================
; Here is the special case for -2147483648 string
; ===============================================================
	CLD		
	MOV		EDI, [EBP + 8]				; user string
	MOV		ESI, [EBP + 20]				; bastard -2147483648 string

	MOV		ECX, 12
_scanForSpecial:
	CMPSB
	JNE		_checkSign
	LOOP	_scanForSpecial

; rut roh, we found the thing!
	MOV		localValue, -2147483648
	MOV		isValid, 1
	JMP		_storeNum

_checkSign:
; ===============================================================
; Here we test and set our personal sign flag
; ===============================================================
	MOV		ESI, [EBP + 8]				; user string
	MOV		EAX, 0
	LODSB

; test the first character for validity, and is it a + or - sign?
	CMP		EAX, 43						; is it an ASCII + symbol?
	JE		_setPlus
	CMP		EAX, 45						; is it an ASCII - symbol?
	JE		_setMinus
	CMP		EAX, 48						; does the caracter come before ASCII 0?
	JB		_exitProcedure				; if so, terminaate with valid = 0
	CMP		EAX, 57						; does the caracter come after ASCII 9?
	JA		_exitProcedure				; if so, terminaate with valid = 0

	JMP		_countString

; code to set the sign flag
_setPlus:
	MOV		posOrNeg, 1					; set flag 1 and start parsing
	JMP		_countString
_setMinus:	
	MOV		posOrNeg, -1				; set flag -1 and start parsing


_countString:
; ===============================================================
; Here we count the length of the string
; ===============================================================



; find string length:
	MOV		ECX, 0						; start at 0
	MOV		ESI, [EBP + 8]				; user string
_startCOunt:
	MOV		EAX, 0						; null EAX
	LODSB								
	CMP		EAX, 0						; if we find a null byte, terminate counting
	JE		_doneCount

; +1 to count and re-loop
	INC		ECX							
	JMP		_startCOunt

_doneCount:
; done counting so store the value in stringLength
	CMP		ECX, 0						; HERE IS THE SPECIAL CASE EXIT FOR NULL STRING
	JE		_exitProcedure
	MOV		stringLength, ECX


; ===============================================================
; Here is the loop to scan characters and add the values
; ===============================================================

	MOV		ECX, 0
	MOV		ESI, [EBP + 8]
	ADD		ESI, stringLength					; Set ESI with  string length
	DEC		ESI									; minus 1!
	STD											; descending order

; if the lead char is + or -, decrement stringlenth -- one less loop

	CMP		posOrNeg, 0							; flag = 0? forst char is numeric
	JE		_mathLoop					

	DEC		stringLength						; flag is 1/-1 first char is +/-


; meat of the loop
_mathLoop:
	
	CMP		ECX, 0								; first loop -- set multiplier to 1
	JE		_onesPlace
	
	JMP		_notOnesPlace						; not first? then we need to multiply by powers of 10

_onesPlace:
	MOV		EBX, 1
	JMP		_multiplierSet

_notOnesPlace:
	MOV		EAX, 1
	PUSH	ECX

	_mulLoop:
		MOV		EBX, 10
		MUL		EBX						; mul EAX by 10 == gets called ECX times - 10. 100. 1000. 10000. etc
		LOOP	_mulLoop

	MOV		EBX, EAX

	POP		ECX

_multiplierSet:							; 10s multiplier is in EBX
	
	MOV		EAX, 0						; clear EAX

; get the character
	LODSB
	
	CMP		EAX, 48						; does the caracter come before ASCII 0?
	JB		_exitProcedure				; if so, terminaate with valid = 0

	CMP		EAX, 57						; does the caracter come after ASCII 9?
	JA		_exitProcedure				; if so, terminaate with valid = 0

	SUB		EAX, 48						; convert from ASCII to decimal!

	MUL		EBX							; power of 10 is in EBX
	JO		_exitProcedure				; overflow


; rack up the value
	ADD		localValue, EAX

; test for carry
	JC		_exitProcedure
	
	INC		ECX							; add 1 to ECX
	CMP		ECX, stringLength			; have looped fully

	JE		_doneMath

	JMP		_mathLoop					; back to top


_doneMath:

	CMP		localValue, 2147483647
	JA		_exitProcedure

; see if our value is negative
	MOV		isValid, 1

	CMP		posOrNeg, -1
	JNE		_storeNum					; not neg, can just store

	NEG		localValue					; negative, we negate value

	MOV		isValid, 1

; this is how we SAVE THE VALUE
_storeNum:
	MOV		EAX, localValue
	MOV		EBX, [EBP + 12]
	MOV		[EBX], EAX 

_exitProcedure:
; store final result of isValid
	MOV		EAX, isValid
	MOV		EBX, [EBP + 16]
	MOV		[EBX], EAX



; byebye
	POPAD
	RET		20				; 5 dwords on stack

readVal ENDP


;---------------------------------------------------------------------------------------------
; Name: writeVal
;
; Writes SDWORD value using mDisplayString macro. The algorithm is a bit odd. Uses a temp
; array to store values, working backwards doing division by 10, storing the remainder as ASCII.
; Once fully calculated, it prints the array (forwards of course!)
;
; Preconditions:	The value to print is a SDWORD. Array exists, to hold output string in process.
; Postconditions:	
; Receives:			[EBP + 8] -- SDWORD value to be printed	
;					[EBP + 12] -- address of temp array for scratch work
; Returns:			Outputs value as a string, using mDisplayString macro
;----------------------------------------------------------------------------------------------
writeVal PROC
; prep stack, save registers
; use some local variables for fun
	LOCAL	localVal:SDWORD, arrayEnd:DWORD, negFlag:DWORD, specialFlag:DWORD
	PUSH	EAX
	PUSH	EBX
	PUSH	EDX
	PUSH	EDI

; get offset value to last usable idex of array -- remember it's MAXCHARS + 1 long,
; but we reserve the last ,0 to terminate the output
	MOV		EAX, MAXCHARS
	MOV		arrayEnd, EAX				
	
; retrieve value to print, and set negFlag = 0
	MOV		EAX, [EBP + 8]				; Rretrieve value from stack
	MOV		negFlag, 0					; set flag for non-negative
	CMP		EAX, 0						; test for negativity
	JGE		_notNegative

; initialize the special case flag to 0, and test for presence of -2,147,483,648
	MOV		specialFlag, 0
	CMP		EAX, -2147483648			; test for -2,147,483,648 -- this bastard thing
	JNE		_notSpeeshul				; not our boah

; rut-roh, we found it...
	INC		EAX							; bump it up to -2,147,483,647 for the next steps
	MOV		specialFlag, 1

; negate number nad set neg flag for later use
_notSpeeshul:
	NEG		EAX							; negate
	MOV		negFlag, 1					; set flag

; wasn't negative, so no special cases or anything
_notNegative:
	MOV		localVal, EAX				; store the value

; get address of array, and find target location
	MOV		EDI, [EBP + 12]
	ADD		EDI, arrayEnd				; increment to target location in array

	STD									; decrementing

; start of the calculating/storing loop
_startLoop:
; sequence of dividing by 10, and storing the remainder in the array as element to print
	MOV		EAX, localVal				; set up for divison
	CDQ		
	MOV		EBX, 10
	IDIV	EBX

; store the quotient for the next round
	MOV		localVal, EAX				; divided number to localval

; test for the "special case" flag
	CMP		specialFlag, 1
	JNE		_asciiAdd					; not special

; bump the value up (the LSB 7 becomes 8)
	INC		EDX
	MOV		specialFlag, 0				; clear flag so it never happens again

; convert to ASCII by adding 48, and store in the array
_asciiAdd:
	ADD		EDX, 48						; this converts it into ASCII equivalent!
	MOV		EAX, EDX
	STOSB

; if the quotient is 0, we've done enough math
	CMP		localVal, 0					; remainder 0, we're done dividing
	JE		_doneDiv					; exit loop

	JMP		_startLoop					; restart loop

; done dividing, do some output
_doneDiv:	 
; test to see if it's a negative number
	CMP		negFlag, 1
	JNE		_printArray					; not neg, just print

; number was negative? write the ASCII for a negative sign into the array!
	MOV		EAX, 45						; negative sign
	STOSB

; Print the finalized array!
_printArray:
	MOV		EAX, EDI
	ADD		EAX, 1						; readjust to account for last STOSB
	mDisplayString	EAX					; mDisplayString just works on an offset :D

; cleanup and exit
	POP		EDI
	POP		EDX
	POP		EBX
	POP		EAX
	RET		8

writeVal ENDP


END main
