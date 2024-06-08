' ----------------------------------------------------------
' EVOCALC.BAS - Basic calculator for addition, subtraction,
' multiplication, and division.
'
' Author: Tim's Tech Retrospect
'
' Version: 1.0
' ----------------------------------------------------------

DIM digit AS STRING                     ' The entered key
DIM digitValue AS INTEGER               ' The ascii value for the digit
DIM number AS STRING                    ' This is the sanitized number entered
DIM activeNumber AS INTEGER             ' If the active number is first or second
DIM numbers(0 TO 1) AS STRING           ' The two numbers on the two sides of the operator
DIM operator AS STRING                  ' The operator
DIM answer AS INTEGER                   ' The calculated answer
DIM lastAnswer AS STRING                ' The last answer, sanitized
DIM row AS INTEGER, col AS INTEGER      ' Current position of the cursor

ON ERROR GOTO ErrorHandler

activeNumber = 0

DO
    digit = INKEY$
    '48 = 0, 57 = 9 :: ASC(digit) - 48
    ' + = 43, - = 45, * = 42, / = 47, ENTER = 13 OR 10 (probably 13)
    IF digit <> "" THEN ' We have a key press
        digitValue = ASC(digit)
            SELECT CASE digitValue
                CASE 27 ' Escape key - clear and reset
                    PRINT
                    GOSUB ResetCalculator
                    lastAnswer = ""
                    PRINT "Cleared!"
                CASE 24 ' Ctrl+X
                    PRINT
                    PRINT "Goodbye!"
                    END
                CASE 8 ' Backspace key
                    ' Only performed if the active number is not empty
                    IF LEN(numbers(activeNumber)) > 0 THEN
                        row = CSRLIN
                        col = POS(0)
                        LOCATE row, (col - 1)
                        PRINT " ";
                        LOCATE row, (col - 1)
                        numbers(activeNumber) = LEFT$(numbers(activeNumber), LEN(numbers(activeNumber)) - 1)
                    END IF
                CASE 48 TO 57 ' Digits 0 through 9
                    number = RTRIM$(LTRIM$(STR$(digitValue - 48)))
                    PRINT number;
                    numbers(activeNumber) = numbers(activeNumber) + number
                CASE 43, 45, 42, 47 ' Operators
                    IF operator = "" THEN ' Only set if operator has not been set yet
                        IF numbers(0) = "" AND lastAnswer <> "" THEN
                            numbers(0) = lastAnswer
                            PRINT lastAnswer;
                        END IF
                        IF numbers(0) <> "" THEN
                            SELECT CASE digitValue
                                CASE 43 ' +
                                    operator = "+"
                                CASE 45 ' -
                                    operator = "-"
                                CASE 42 ' *
                                    operator = "*"
                                CASE 47 ' /
                                    operator = "/"
                            END SELECT
                            PRINT operator;
                            activeNumber = 1
                        END IF
                    END IF
                CASE 61, 13 ' enter/equals
                    IF activeNumber = 1 AND numbers(1) <> "" THEN
                        PRINT "=";
                        SELECT CASE operator
                            CASE "+"
                                answer = VAL(numbers(0)) + VAL(numbers(1))
                            CASE "-"
                                answer = VAL(numbers(0)) - VAL(numbers(1))
                            CASE "*"
                                answer = VAL(numbers(0)) * VAL(numbers(1))
                                
                            CASE "/"
                                IF numbers(1) = "0" THEN ' Division by zero
                                    answer = 0
                                ELSE
                                    answer = VAL(numbers(0)) / VAL(numbers(1))
                                END IF
                        END SELECT
                        lastAnswer = LTRIM$(RTRIM$(STR$(answer)))
                        PRINT lastAnswer
                        GOSUB ResetCalculator
                    END IF
            END SELECT
    END IF
LOOP

END

ResetCalculator:
    ' Reset
    operator = "": activeNumber = 0
    numbers(0) = "": numbers(1) = ""
    RETURN

ErrorHandler:
    SELECT CASE ERR
        CASE 6 ' Overflow Error
            answer = -1
        CASE ELSE
            PRINT
            PRINT "An unexpected error occured: Code " + LTRIM$(STR$(ERR))
            END
    END SELECT
    RESUME NEXT
