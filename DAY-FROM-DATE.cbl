       IDENTIFICATION DIVISION.
       FUNCTION-ID. DAY-FROM-DATE.
                                                                                                                                                                                                                                                                                *>****************************************************************
       *> This GNU COBOL user-defined function converts a Gregorian or **
       *> Julian date into a numeric day of the week. **
       *>****************************************************************
       *> Arguments: **
       *> **
        *> Calendar-Date A PIC 9 data item or numeric literal which **
        *> will be treated as a calendar date as fol- **
        *> lows: **
        *> **
        *> 7-digit value: Interpreted as a Julian date **
        *> in the form yyyyddd **
        *> 8-digit value: Interpreted as a Gregorian **
        *> date in the form yyyymmdd **
        *> **
        *> The result returned will be one of the following: **
        *> **
        *> 0: The supplied date is invalid **
        *> 1: The supplied date is a Sunday **
        *> 2: The supplied date is a Monday **
        *> . **
        *> . **
        *> . **
        *> 7: The supplied date is a Saturday **
        *>****************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       REPOSITORY.
       FUNCTION ALL INTRINSIC.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-Input-Date-DT.
           05 WS-ID-YYYY-NUM PIC 9(4).
           05 WS-ID-MM-NUM PIC 9(2).
           05 WS-ID-DD-NUM PIC 9(2).
       01 WS-Y-NUM BINARY-LONG.
       01 WS-M-NUM BINARY-LONG.
       01 WS-Temp-NUM BINARY-LONG.
       LINKAGE SECTION.
       01 L-Input-Date-DT PIC X ANY LENGTH.
       01 L-Output-Day-NUM USAGE BINARY-LONG SIGNED.
       PROCEDURE DIVISION USING L-Input-Date-DT
           RETURNING L-Output-Day-NUM.
       000-Main SECTION.
           CALL "C$PARAMSIZE" USING 1
           EVALUATE RETURN-CODE
           WHEN 7
           IF TEST-DAY-YYYYDDD(L-Input-Date-DT) > 0
               MOVE 0 TO L-Output-Day-NUM
               GOBACK
           END-IF
           MOVE DATE-OF-INTEGER(INTEGER-OF-DAY(L-Input-Date-DT))
           TO WS-Input-Date-DT
           WHEN 8
           IF TEST-DATE-YYYYMMDD(L-Input-Date-DT) > 0
               MOVE 0 TO L-Output-Day-NUM
               GOBACK
           END-IF
           MOVE L-Input-Date-DT TO WS-Input-Date-DT
           WHEN OTHER
           MOVE 0 TO L-Output-Day-NUM
           GOBACK
           END-EVALUATE
       *> IF january OR february
       *> y = year - 1
       *> m = month + 10
       *> ELSE
       *> y = year
       *> m = month - 2
       *> END-IF
       *> For Gregorian calendar:
       *> result = (day + y + y/4 - y/100 + y/400 + (31*m)/12) mod 7
       *> (All divisions are integer divisions, discarding any remainder)
           IF WS-ID-MM-NUM = 1 OR 2
               SUBTRACT 1 FROM WS-ID-YYYY-NUM GIVING WS-Y-NUM
               ADD WS-ID-MM-NUM, 10 GIVING WS-M-NUM
           ELSE
               MOVE WS-ID-YYYY-NUM TO WS-Y-NUM
               SUBTRACT 2 FROM WS-ID-MM-NUM GIVING WS-M-NUM
           END-IF
           COMPUTE L-Output-Day-NUM = WS-ID-DD-NUM
               + WS-Y-NUM
               + INTEGER(WS-Y-NUM/4)
               - INTEGER(WS-Y-NUM/100)
               + INTEGER(WS-Y-NUM/400)
               + INTEGER((31*WS-M-NUM)/12)
           DIVIDE L-Output-Day-NUM BY 7
           GIVING WS-Temp-NUM
           REMAINDER L-Output-Day-NUM.
           ADD 1 TO L-Output-Day-NUM.
           GOBACK.
