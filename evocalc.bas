' ----------------------------------------------------------
' EVOCALC.BAS - Basic calculator for addition, subtraction,
' multiplication, and division
'
' Author: Tim's Tech Retrospect
'
' Version 1.1
' ----------------------------------------------------------

CONST APPNAME = "EvoCalc"
CONST APPVERSION = "1.1"

CONST FALSE = 0
CONST TRUE = NOT FALSE

CONST ECERRNONE = 0
CONST ECERROVERFLOW = 1
CONST ECERRDIVZERO = 2

CONST ECOPNONE = 0
CONST ECOPADD = 1
CONST ECOPSUBTRACT = 2
CONST ECOPMULTIPLY = 3
CONST ECOPDIVIDE = 4

TYPE CurrentPosition
    row AS INTEGER
    col AS INTEGER
END TYPE

TYPE EnteredDigit
    literal AS STRING * 1
    ascii AS INTEGER
END TYPE

DIM SHARED numbers(0 TO 1) AS STRING
DIM SHARED activeNumber AS INTEGER
DIM SHARED operator AS INTEGER
DIM SHARED answer AS INTEGER
DIM SHARED errorState AS INTEGER
DIM SHARED hasAnswer AS INTEGER
DIM SHARED lastNumbers(0 TO 1) AS STRING
DIM SHARED lastOperator AS INTEGER


DECLARE SUB MAIN ()
DECLARE SUB TERMINATE ()

DECLARE SUB RedrawInfoBar ()
DECLARE SUB GetCurrentPosition (cpos AS CurrentPosition)
DECLARE SUB ReadDigit (digit AS EnteredDigit)
DECLARE SUB ClearInfoBar ()
DECLARE SUB ResetCalculator (clearAnswer AS INTEGER)
DECLARE SUB Calculate ()
DECLARE SUB SetOperator (op AS STRING)
DECLARE SUB AppendNumber (digit AS EnteredDigit)
DECLARE SUB ClearState ()
DECLARE SUB Backspace ()

DECLARE FUNCTION PadLeft$ (str AS STRING, length AS INTEGER)
DECLARE FUNCTION PadRight$ (str AS STRING, length AS INTEGER)

ON ERROR GOTO ErrorHandler

CALL MAIN: END

ErrorHandler:
    SELECT CASE ERR
        CASE 6
            errorState = ECERROVERFLOW
            PRINT
            PRINT "ERR: Overflow"
        CASE 11
            errorState = ECERRDIVZERO
            PRINT
            PRINT "ERR: Divide by zero"
        CASE ELSE
            IF activeNumber > 0 OR numbers(0) <> "" THEN PRINT
            PRINT "An unexpected error occurred: CODE " + LTRIM$(STR$(ERR))
            END
    END SELECT
    RESUME NEXT


SUB AppendNumber (digit AS EnteredDigit)
    DIM number AS STRING

    number = LTRIM$(STR$(digit.ascii - 48))
    PRINT number;
    numbers(activeNumber) = numbers(activeNumber) + number
END SUB

SUB Backspace
    IF LEN(numbers(activeNumber)) = 0 THEN EXIT SUB
    DIM cpos AS CurrentPosition
    CALL GetCurrentPosition(cpos)
    LOCATE cpos.row, cpos.col - 1
    PRINT " ";
    LOCATE cpos.row, cpos.col - 1
    numbers(activeNumber) = LEFT$(numbers(activeNumber), LEN(numbers(activeNumber)) - 1)
END SUB

SUB Calculate
    IF numbers(0) = "" THEN
        IF hasAnswer = FALSE THEN EXIT SUB
        numbers(0) = LTRIM$(STR$(answer))
        numbers(1) = lastNumbers(1)
        operator = lastOperator
        PRINT numbers(0);
        SELECT CASE operator
            CASE ECOPADD: PRINT "+";
            CASE ECOPSUBTRACT: PRINT "-";
            CASE ECOPMULTIPLY: PRINT "*";
            CASE ECOPDIVIDE: PRINT "/";
        END SELECT
        PRINT numbers(1);
    ELSEIF numbers(1) = "" THEN
        EXIT SUB
    END IF
    errorState = ECERRNONE
    SELECT CASE operator
        CASE ECOPADD
            answer = VAL(numbers(0)) + VAL(numbers(1))
        CASE ECOPSUBTRACT
            answer = VAL(numbers(0)) - VAL(numbers(1))
        CASE ECOPMULTIPLY
            answer = VAL(numbers(0)) * VAL(numbers(1))
        CASE ECOPDIVIDE
            answer = VAL(numbers(0)) / VAL(numbers(1))
    END SELECT
    hasAnswer = TRUE
    IF errorState = ECERRNONE THEN
        PRINT "=";
        PRINT LTRIM$(STR$(answer))
        lastOperator = operator
        lastNumbers(0) = numbers(0)
        lastNumbers(1) = numbers(1)
    END IF
    CALL ResetCalculator(FALSE)

    

END SUB

SUB ClearInfoBar
    DIM cpos AS CurrentPosition
    CALL GetCurrentPosition(cpos)
    LOCATE 25, 1
    PRINT
    COLOR 7, 0
    LOCATE cpos.row, cpos.col
END SUB

SUB ClearState
    IF activeNumber = 0 AND numbers(0) = "" THEN
        ' Check anything has been entered. If not, then
        ' reset the last answer
        IF hasAnswer = TRUE THEN
            CALL ResetCalculator(TRUE)
            PRINT "ANSWER CLEARED!"
        END IF
    ELSE
        ' Otherwise, just reset the current state
        CALL ResetCalculator(FALSE)
        PRINT
        PRINT "CLEARED.";
        IF hasAnswer = TRUE THEN
            PRINT " PRESS [ESC] AGAIN TO CLEAR THE LAST ANSWER"
        ELSE
            PRINT
        END IF
    END IF
END SUB

SUB GetCurrentPosition (cpos AS CurrentPosition)
    DIM row AS INTEGER: row = CSRLIN
    DIM col AS INTEGER: col = POS(0)

    cpos.row = row
    cpos.col = col
END SUB

SUB MAIN
    DIM digit AS EnteredDigit

    CLS : RedrawInfoBar
    CALL ResetCalculator(TRUE)
    
    DO
        CALL ReadDigit(digit)
        SELECT CASE digit.ascii
           
            CASE 48 TO 57 ' Digits 0 through 9
                CALL AppendNumber(digit)

            CASE 43, 45, 42, 47 ' Operators
                CALL SetOperator(digit.literal)

            CASE 24 ' Ctrl+X
                CALL TERMINATE

            CASE 27 ' ESC
                CALL ClearState

            CASE 61, 13 ' Equals / Enter / Return
                CALL Calculate

            CASE 8
                CALL Backspace

        END SELECT
        RedrawInfoBar
    LOOP
END SUB

FUNCTION PadLeft$ (str AS STRING, length AS INTEGER)
    DIM padLength%
    padLength% = length% - LEN(str)
    IF padLength% > 0 THEN
        PadLeft$ = SPACE$(padLength%) + str
    ELSE
        PadLeft$ = str
    END IF
END FUNCTION

FUNCTION PadRight$ (str AS STRING, length AS INTEGER)
    DIM padLength%
    padLength% = length% - LEN(str)
    IF padLength% > 0 THEN
        PadRight$ = str + SPACE$(padLength%)
    ELSE
        PadRight$ = str
    END IF

END FUNCTION

SUB ReadDigit (digit AS EnteredDigit)
    DIM digitLiteral AS STRING
    DIM digitAscii AS INTEGER
    DO
        digitLiteral = INKEY$
        IF digitLiteral <> "" THEN ' We have a key press
            digit.literal = digitLiteral
            digit.ascii = ASC(digit.literal)
            EXIT SUB
        END IF
    LOOP
END SUB

SUB RedrawInfoBar
    DIM cpos AS CurrentPosition
    CALL GetCurrentPosition(cpos)
    IF cpos.row = 1 THEN cpos.row = 2 ' So we don't overwrite the banner
    LOCATE 1, 1
    COLOR 15, 1
    DIM banner AS STRING, controls AS STRING
    controls = "Equals: [ENTER] | Clear: [ESC] | Exit: [Ctrl+X] "
    banner = " " + APPNAME + " v" + APPVERSION
    PRINT PadRight$(banner, 80 - LEN(controls)); controls;
    LOCATE cpos.row, cpos.col
    COLOR 7, 0
END SUB

SUB ResetCalculator (clearAnswer AS INTEGER)
    operator = ECOPNONE
    activeNumber = 0
    numbers(0) = ""
    numbers(1) = ""

    IF clearAnswer = TRUE THEN
        hasAnswer = FALSE
        answer = 0
    END IF
END SUB

SUB SetOperator (op AS STRING)
    IF operator > ECOPNONE THEN EXIT SUB ' Skip if already set

    ' Determine if we are using the last answer
    IF activeNumber = 0 AND numbers(0) = "" THEN
        IF hasAnswer = TRUE THEN
            numbers(0) = LTRIM$(STR$(answer))
            PRINT numbers(0);
        ELSE
            EXIT SUB ' No values entered yet
        END IF
    END IF

    SELECT CASE op
        CASE "+"
            operator = ECOPADD
        CASE "-"
            operator = ECOPSUBTRACT
        CASE "*"
            operator = ECOPMULTIPLY
        CASE "/"
            operator = ECOPDIVIDE
    END SELECT
    PRINT op;
    activeNumber = 1
END SUB

SUB TERMINATE
    IF NOT (activeNumber = 0 AND numbers(0) = "") THEN
        PRINT
    END IF
    
    ClearInfoBar
    PRINT "Goodbye!"
    PRINT
    END
END SUB
