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
ARRAYLENGTH		=	10					; numer of ints to get - math is correct if tweaked!
MAXCHARS		=	11					; a SDWORD int can only be 11 chars long, -2,147,483,648
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

; -------------------------------------------------------------------------------------
; Here is the loop to fill the array.
; Loops ARRAYLENGTH times. 
; Each loop does this: Gets a value with readVal, makes sure it's valid. Re-prompts
; until valid number is received from the user. Then calculates and displays the
; running subtitle.
; -------------------------------------------------------------------------------------
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
	PUSH	OFFSET specialNum				; EBP+20 -- the special case number I hate, string form
	PUSH	OFFSET validVal					; EBP+16 -- BOOLEAN FOR SUCCESS/FAILURE
	PUSH	OFFSET userVal					; EBP+12 -- holds the SDWORD value of the user's string
	PUSH	OFFSET userString				; EBP+8	 -- Scratch, holds string user enters
	CALL	readVal

; check result boolean for valid value
	CMP		validVal, 1
	JE		_validValue						; got it!

_invalidValue:
; we didn;t find the 1, sooooo ... invalid value found, ask again:
	mDisplayString OFFSET invalidNum

	; make readVal call to get user input -- stored in userVal if successful
	PUSH	OFFSET tryAgain					; EBP+24 -- prompt to print for entry
	PUSH	OFFSET specialNum				; EBP+20 -- the special case number I hate, string form
	PUSH	OFFSET validVal					; EBP+16 -- BOOLEAN FOR SUCCESS/FAILURE
	PUSH	OFFSET userVal					; EBP+12 -- holds the SDWORD value of the user's string
	PUSH	OFFSET userString				; EBP+8	 -- Scratch, holds string user enters
	CALL	readVal

; we want to repeat the INVALID prompt, not the normal prompt
	CMP		validVal, 0						
	JE		_invalidValue

; we just got a valid value, so we jump here:
_validValue:
	MOV		EAX, userVal					; get the result
	
	MOV		[EDI], EAX						; store it in array

	ADD		runTotal, EAX					; add it to the running total
	
	INC		ECX								; next loop value
	ADD		EDI, TYPE numArray				; move EDI to next array location

	CMP		ECX, ARRAYLENGTH				; have we gotten enough numbers?
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

; ------------------------------------------------------------------------------------------
; Done filling the array, so here we display the resulting array, and the total and average.
; Displays a prompt, and then an opening bracket. Cycles through array printing values
; separated by commas, until array is exhausted, then closes the brackets.  Displays the sum
; of the numbers, and then calculates and displays the truncated integer average. Then...
; the proram ends! Hopefully with less smoke and fire than some code I've written.
; ------------------------------------------------------------------------------------------
_doneFill:
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

; so long, and thanks for all the bits
	mDisplayString	OFFSET byePrompt

	Invoke	ExitProcess,0	; exit to operating system
main ENDP


; (insert additional procedures here)
;---------------------------------------------------------------------------------------------
; Name: readVal
;
; Prompts user for value. Reads value as string, using mGetString macro. Checks the string for
; invalid characters. Parses string by iteratively adding the digits*powers of ten  
; as appropriaate. WHen conclusion is reached, sets the validVal boolean as appropriate,
; and if a number was sucessfully entered, the number will be stored in the userVal variable.
;
; Preconditions:	Prompt to print exists. "special dude" character string exists.
; Postconditions: 
; Receives:			[EBP + 24] -- Offset of prompt to display
;					[EBP + 20] -- our special boah -2147483648 in string form
;					[EBP + 16] -- validVal boolean
;					[EBP + 12] -- userVal variable; will be filled with SDWORD conversion
;								  (iff successful!)
;					[EBP + 8] -- userString; OFFSET of array to store the user's string 
; Returns:			userString holds the string the user entered. Not used, but it's there.
;					Value of user's string entry converted to SDWORD and stored in userVal.
;					validVal holds 0 (invalid) or 1 (valid) depending the user's entry
;----------------------------------------------------------------------------------------------
readVal PROC
; declare LOCAL variables, for my sanity instead of needing offsets for EVERYTHING.
	LOCAL	isValid:DWORD, localValue:DWORD, posOrNeg:DWORD, stringLength:DWORD
	PUSHAD											; we use so many... I hope the blunt instrument is allowed

; mov 0 to isValid, and store it. Makes sure we have a failure by default
	MOV		isValid, 0
	MOV		EAX, isValid
	MOV		EBX, [EBP + 16]
	MOV		[EBX], EAX

	MOV		localValue, 0				; sum starts at 0
	MOV		posOrNeg, 0					; -1 if first char is -, 1 if first char is +, 0 if first char is 0-9

; get the string from the user woth mGetString
	mGetString	[EBP+24], [EBP + 8], USERSTRLENGTH
	
; -------------------------------------------------------------------------------------
; Here is the special case for "-2147483648" string, This bugger is annoying, because it 
; is its own complement and thus can't be negated, or created by negation, as my 
; algorithm does with other vlaues. So we test the user's string against this special 
; string, and short-circuit the logic to write it into the results.
; -------------------------------------------------------------------------------------
	CLD		
	MOV		EDI, [EBP + 8]				; user string
	MOV		ESI, [EBP + 20]				; bastard -2147483648 string

	MOV		ECX, 12
_scanForSpecial:
	CMPSB
	JNE		_checkSign
	LOOP	_scanForSpecial

; rut roh, we found the thing!
	MOV		localValue, -2147483648		; store, and skip to "valid" code
	MOV		isValid, 1
	JMP		_storeNum

; -------------------------------------------------------------------------------------
; Here we test and set our personal sign flag. THe sign flag will either be -1 if 
; the user enters a - as the first character, 1 if the first character is a +, or 0 
; if the user enters a digit. Also tests to see if the user is being clever and typing
; a word, it rejects any ASII values that represent anything other than digits.
; -------------------------------------------------------------------------------------
_checkSign:
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

; -------------------------------------------------------------------------------------
; Here we count the length of the string. We iterate over the string looking for the 
; first ",0" terminator we see. If the string is 0 digits long (no entry), it gets
; rejected. Strings over 11 digits get tossed too, because the max possible length is
; "-2147483648" -- 11 characters.
; -------------------------------------------------------------------------------------
_countString:
;prep loop
	MOV		ECX, 0						; start at 0
	MOV		ESI, [EBP + 8]				; user string

; start loop
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

; a user-supplied string of over 11 digits is invalid by default!
	CMP		stringLength, 11
	JA		_exitProcedure

; -------------------------------------------------------------------------------------
; Here is the loop to scan characters and add the values. We start with the digit in 
; the 1's place, convert it to a digit, and add it to localValue. Then we loop again
; to the 10's place, multiply by the value, and add the value to localValue, 100s place,
; 1000s pace, etc until the string is exhausted. Also filters out attempts with  
; sneaky letters and non-digit characters in the string.
; -------------------------------------------------------------------------------------
	MOV		ECX, 0								; reset ECX
	MOV		ESI, [EBP + 8]
	ADD		ESI, stringLength					; Set ESI, add string length
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

; this happens on the first loop -- we set the multiplier to 1 for the ones place
_onesPlace:
	MOV		EBX, 1								; EBX is multiplier, 1 in this case
	JMP		_multiplierSet

; successive loops, we multiply by 10^ECX
_notOnesPlace:
	MOV		EAX, 1
	PUSH	ECX							; save ECX for inner loop

	; this loop raises 10^ECXth power
	_mulLoop:
		MOV		EBX, 10
		MUL		EBX						; mul EAX by 10 == gets called ECX times - 10. 100. 1000. 10000. etc
		LOOP	_mulLoop

	MOV		EBX, EAX					; multiplier to EBX

	POP		ECX							; restore ECX

; the multiplier is set, and in EBX, time to start doing WORK
_multiplierSet:							
	MOV		EAX, 0						; clear EAX
	LODSB								; get the character
	
	CMP		EAX, 48						; does the caracter come before ASCII 0?
	JB		_exitProcedure				; if so, terminate with valid = 0

	CMP		EAX, 57						; does the caracter come after ASCII 9?
	JA		_exitProcedure				; if so, terminaate with valid = 0

	SUB		EAX, 48						; convert from ASCII to decimal!

	MUL		EBX							; power of 10 is in EBX
	JO		_exitProcedure				; overflow, early exit potential

; rack up the value
	ADD		localValue, EAX
	JC		_exitProcedure				; carry? maybe this happens

; next value, and check for ending	
	INC		ECX							; add 1 to ECX
	CMP		ECX, stringLength			; have looped fully
	JE		_doneMath					; bye, math loop!

	JMP		_mathLoop					; back to top

; -----------------------------------------------------------------------------------------
; localvalue now holds the sum of all the digits. Up until this point it was UNSIGNED, 
; which means it could potentially hold numbers that are too large to fit in a SIGNED INT.
; So, we test it against the largest possible value, and reject ones that are too big. 
; once rejection happens, we use the value in our sign flag to give it its desired sign.
; If it's -1 we NEG the value, giving us the final SDWORD value. Then we store the value
; in the appropriate variable, store the value of the "isValid" boolean, and kablammo!
; -----------------------------------------------------------------------------------------
_doneMath:
	CMP		localValue, 2147483647		; compare with the max value we should allow  
	JA		_exitProcedure				; larger? bye bye!

; if we have made it this far, we have won. Valid number!
	MOV		isValid, 1

; test our sign flag
	CMP		posOrNeg, -1
	JNE		_storeNum					; not neg, can just store

	NEG		localValue					; negative, we negate value

	MOV		isValid, 1

; this is how we SAVE THE VALUE
_storeNum:
	MOV		EAX, localValue				
	MOV		EBX, [EBP + 12]				; offset of user val, on stack
	MOV		[EBX], EAX					; save value

; This is where we land from all the "invalid" cases above ... or if we made it here the hard way
_exitProcedure:
; store final result of isValid
	MOV		EAX, isValid				; save the boolean 0/1
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

; ----------------------------------------------------------------------------------------------
; Here's the prepping we do before we calculate and create the string. We set up the initial
; values of out local variables. We set our "negative number" status flag. IF we found the 
; "speciaal number surprise", we increment by 1, and set a flag so we remember to decrement it
; later. The last step is to store out value in the localVal variable, and we're ready to
; go to town on this ********
; ---------------------------------------------------------------------------------------------
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

; negate number nad set neg flag for later use. POSITIVE numbers give good results. Negative
; ones give us the sadz when we do math.
_notSpeeshul:
	NEG		EAX							; negate
	MOV		negFlag, 1					; set flag

; wasn't negative, so no special cases or anything
_notNegative:
	MOV		localVal, EAX				; store the value

;--------------------------------------------------------------------------------------------
; Here we do the fun stuff, the math to generate the string body. Starting with the least 
; significant digits place, we divide by 10, convert the remainder to ASCII, and write it 
; into the array. Repeat until we're out of digits, and the string is built! (Backwards, natch.)
; -------------------------------------------------------------------------------------------
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

; -------------------------------------------------------------------------------------------
; Time to print our string. if our negative flag has not been set, we just print it.
; If our negative flag HAS been set, we write a negative sign charater at the beginning of
; the string, and THEN print it.
; -------------------------------------------------------------------------------------------
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
