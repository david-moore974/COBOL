       IDENTIFICATION DIVISION.
       PROGRAM-ID. CBLDSM04.
       AUTHOR.     David Moore. 
       DATE-WRITTEN. 01/20/18.

      ******************************************************************
      *THIS WILL TOTAL OUR POP SALES AND WRITE INVALID RECORDS TO ERR FL
      ******************************************************************






       ENVIRONMENT DIVISION.

           INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT POP-MASTER
               ASSIGN TO 'C:\COBOL\CP#4.dat'
                   ORGANIZATION IS LINE SEQUENTIAL.
           SELECT PRTOUT
               ASSIGN TO 'C:\COBOL\CBLPOPSL.PRT'
               ORGANIZATION IS RECORD SEQUENTIAL.
           SELECT PRTOUTERR
               ASSIGN TO 'C:\COBOL\CBLPOPER.PRT'
               ORGANIZATION IS RECORD SEQUENTIAL.





       CONFIGURATION SECTION.

       DATA DIVISION.
           FILE SECTION.

           FD  POP-MASTER
           LABEL RECORD IS STANDARD
           DATA RECORD IS I-REC
           RECORD CONTAINS 71 CHARACTERS.
       01  I-REC.
           05 P-LNAME          PIC X(15).
           05 P-FNAME          PIC X(15).
           05 P-ADDRESS        PIC X(15).
           05 P-CITY           PIC X(10).
           05 P-STATE          PIC XX.
           05 P-ZIP            PIC 9(9).
           05 P-POP-TYPE       PIC 99.
           05 P-CASES          PIC 99.
           05 P-TEAM           PIC X.

       FD  PRTOUT
           LABEL RECORD IS OMMITED
           RECORD CONTAINS 132 CHARACTERS
           DATA RECORD IS PRINTLINE
           LINAGE IS 60 WITH FOOTING AT 56.
           01 PRINTLINE             PIC X(132).


       FD  PRTOUTERR
           LABEL RECORD IS OMMITED
           RECORD CONTAINS 132 CHARACTERS
           DATA RECORD IS PRINTLINE
           LINAGE IS 60 WITH FOOTING AT 56.
           01 PRINTLINE             PIC X(132).



       WORKING-STORAGE SECTION.
       01 MISIC.
           05 EOF                    PIC X         VALUE 'F'.
           05 CURRENT-DATE-AND-TIME.
               10  CURRENT-YEAR      PIC X(4).
               10  CURRENT-MONTH     PIC XX.
               10  CURRENT-DAY       PIC XX.
               10  CURRENT-TIME      PIC X(11).
           05  C-PCTR                PIC 99         VALUE ZERO.
           05  C-PCTR-2              PIC 99         VALUE ZERO.
           
           05 WHOLD                    PIC X.

               
           05 ERROR-SW             PIC X.
               88 IS-ERROR VALUE 'Y'.
               88 NOT-ERROR VALUE 'N'.

           05 C-POP-TYPE               PIC 99.
               88 VALID-POP-TYPES      VALUE 1 THRU 12.

           05 C-STATE                  PIC XX.
               88 VALID-STATES      VALUE 'IA','IL','MO','NE','WI','MI'.


           05 C-CASES                  PIC 99.
               88 NOT-VALID-CASES      VALUE 0.


           05 C-TEAMS                  PIC X.
               88 VALID-TEAMS          VALUE 'A','B','C','D','E'.


           05 P-ZIP-CODE.
               10 O-ZIP-1              PIC 9(5).
               10 O-ZIP-2              PIC 9(4).



       01 CALCS.
           05 COKE                     PIC 9(6) VALUE ZERO.
           05 DIET-COKE                PIC 9(6) VALUE ZERO.
           05 MELLO-YELLO              PIC 9(6) VALUE ZERO.
           05 CHERRY-COKE              PIC 9(6) VALUE ZERO.
           05 DIET-CHERRY-COKE         PIC 9(6) VALUE ZERO.
           05 SPRITE                   PIC 9(6) VALUE ZERO.
           05 DIET-SPRITE              PIC 9(6) VALUE ZERO.
           05 DASANI                   PIC 9(6) VALUE ZERO.
           05 C2                       PIC 9(6) VALUE ZERO.
           05 MR-PIBB                  PIC 9(6) VALUE ZERO.
           05 DIET-LEMON-COKE          PIC 9(6) VALUE ZERO.
           05 VANILLA-COKE             PIC 9(6) VALUE ZERO.

           05 TEAM-A                   PIC 9(10)V99 VALUE ZERO.
           05 TEAM-B                   PIC 9(10)V99 VALUE ZERO.
           05 TEAM-C                   PIC 9(10)V99 VALUE ZERO.
           05 TEAM-D                   PIC 9(10)V99 VALUE ZERO.
           05 TEAM-E                   PIC 9(10)V99 VALUE ZERO.
           05 HOLD                     PIC 9(10)V99 VALUE ZERO.

           05 POP-DEPOSIT              PIC 9V99.
           05 POP-DEPO-AMT             PIC 9999V99.

           05 TOTAL                    PIC 9(5)V99.

           05 ERR-CRT                  PIC 9999 VALUE ZERO.



       01 HEADING1.
           05 FILLER                 PIC X(6)       VALUE 'DATE: '.
           05  H1-DATE.
               10 H1-MONTH           PIC 99.
               10 FILLER             PIC X      VALUE '/'.
               10  H1-DAY            PIC 99.
               10 FILLER             PIC X      VALUE '/'.
               10 H1-YEAR            PIC 9999.
           05  FILLER                PIC X(36)  VALUE SPACES.
           05 FILLER                 PIC X(29)
                           VALUE 'ALBIA SOCCER CLUB FUNDRAISER'.
           05 FILLER                 PIC X(43) VALUE SPACES.
           05  FILLER                PIC X(6)   VALUE 'PAGE: '.
           05  H1-PAGE               PIC Z9.



       01 HEADING1-2.
           05 FILLER                 PIC X(6)       VALUE 'DATE: '.
           05  H1-DATE.
               10 H1-MONTH           PIC 99.
               10 FILLER             PIC X      VALUE '/'.
               10  H1-DAY            PIC 99.
               10 FILLER             PIC X      VALUE '/'.
               10 H1-YEAR            PIC 9999.
           05  FILLER                PIC X(36)  VALUE SPACES.
           05 FILLER                 PIC X(29)
                           VALUE 'ALBIA SOCCER CLUB FUNDRAISER'.
           05 FILLER                 PIC X(43) VALUE SPACES.
           05  FILLER                PIC X(6)   VALUE 'PAGE: '.
           05  H1-PAGE-2             PIC Z9.




       01 HEADING2.
           05 FILLER                   PIC X(56) VALUE SPACES.
           05 O-NAME                   PIC X(10) VALUE 'MOORE'.
           05 FILLER                   PIC X VALUE SPACE.
           05 FILLER                   PIC X(8) VALUE 'DIVISION'.
           05 FILLER                   PIC X(57) VALUE SPACES.


       01 HEADING3.
           05 FILLER                   PIC X(60) VALUE SPACES.
           05 FILLER                   PIC X(12) VALUE 'SALES REPORT'.
           05 FILLER                   PIC X(58) VALUE SPACES.

       01 HEADING3-2.
           05 FILLER                   PIC X(60) VALUE SPACES.
           05 FILLER                   PIC X(12) VALUE 'ERROR REPORT'.
           05 FILLER                   PIC X(58) VALUE SPACES.

       01 HEADING4-2.
           05 FILLER                   PIC X(12) VALUE 'ERROR RECORD'.
           05 FILLER                   PIC X(60) VALUE SPACES.
           05 FILLER                   PIC X(17)
                               VALUE 'ERROR DESCRIPTION'.
           05 FILLER                   PIC X(43) VALUE SPACES.


       01 COL-HEADING1.
           05 FILLER                   PIC X(3) VALUE SPACES.
           05 FILLER                   PIC X(9) VALUE 'LAST NAME'.
           05 FILLER                   PIC X(8) VALUE SPACES.
           05 FILLER                   PIC X(10) VALUE 'FIRST NAME'.
           05 FILLER                   PIC X(7) VALUE SPACES.
           05 FILLER                   PIC X(4) VALUE 'CITY'.
           05 FILLER                   PIC X(8) VALUE SPACES.
           05 FILLER                   PIC X(5) VALUE 'STATE'.
           05 FILLER                   PIC X    VALUE SPACE.
           05 FILLER                   PIC X(8) VALUE 'ZIP CODE'.
           05 FILLER                   PIC X(4) VALUE SPACES.
           05 FILLER                   PIC X(8) VALUE 'POP TYPE'.
           05 FILLER                   PIC X(13) VALUE SPACES.
           05 FILLER                   PIC X(8) VALUE 'QUANTITY'.
           05 FILLER                   PIC X(6) VALUE SPACES.
           05 FILLER                   PIC X(11) VALUE 'DEPOSIT AMT'.
           05 FILLER                   PIC X(6) VALUE SPACES.
           05 FILLER                   PIC X(11) VALUE 'TOTAL SALES'.
           05 FILLER                   PIC X(3) VALUE SPACES.

       01 DETAIL-LINE.
           05 FLAG-AST                 PIC XXX.
           05 P-LNAME                  PIC X(15).
           05 FILLER                   PIC XX VALUE SPACES.
           05 P-FNAME                  PIC X(15).
           05 FILLER                   PIC XX VALUE SPACES.
           05 P-CITY                   PIC X(10).
           05 FILLER                   PIC XXX VALUE SPACES.
           05 P-STATE                  PIC XX.
           05 FILLER                   PIC XXX VALUE SPACES.
           05 P-ZIP-CODE.
               10 O-ZIP-1              PIC 9(5).
               10 FILLER               PIC X VALUE '-'.
               10 O-ZIP-2              PIC 9(4).

           05 FILLER                   PIC XXX VALUE SPACES.
           05 O-POP-LIT                PIC X(16).
           05 FILLER                   PIC X(8).
           05 O-QTY                    PIC Z9.
           05 FILLER                   PIC X(11) VALUE SPACES.
           05 O-DEPO-AMT               PIC $$$$.99.
           05 FILLER                   PIC X(9) VALUE SPACES.
           05 O-TOTAL-SALES            PIC $$,$$$.99.
           05 FILLER                   PIC XXX VALUE SPACES.


       




       01 GRAND-TOTALS-LIT.
           05 FILLER                   PIC X(14) VALUE 'GRAND TOTALS: '.
           05 FILLER                   PIC X(118) VALUE SPACES.


       01 GRAND-TOTALS-LINE.
           05 FILLER                   PIC X(3) VALUE SPACES.
           05 O-GT-POP-LIT-1           PIC X(16).
           05 FILLER                   PIC X VALUE SPACE.
           05 O-GT-POP-NUM-1           PIC ZZZ,ZZ9.
           05 FILLER                   PIC X(3) VALUE SPACES.

           05 O-GT-POP-LIT-2           PIC X(16).
           05 FILLER                   PIC X VALUE SPACE.
           05 O-GT-POP-NUM-2           PIC ZZZ,ZZ9.
           05 FILLER                   PIC X(3) VALUE SPACES.

           05 O-GT-POP-LIT-3           PIC X(16).
           05 FILLER                   PIC X VALUE SPACE.
           05 O-GT-POP-NUM-3           PIC ZZZ,ZZ9.
           05 FILLER                   PIC X(3) VALUE SPACES.

           05 O-GT-POP-LIT-4           PIC X(16).
           05 FILLER                   PIC X VALUE SPACE.
           05 O-GT-POP-NUM-4           PIC ZZZ,ZZ9.
           05 FILLER                   PIC X(24) VALUE SPACES.




       01 TEAM-TOTALS-LIT.
           05 FILLER                   PIC X(12) VALUE 'TEAM TOTALS:'.
           05 FILLER                   PIC X(120) VALUE SPACES.



      
       01 TEAM-TOTALS.
           05 FILLER                   PIC XXX VALUE SPACES.
           05 O-TEAM-X                 PIC X.
           05 FILLER                   PIC X VALUE SPACE.
           05 O-GT-TEAM-SALES         PIC $$$$,$$$,$$$.99.

       01 TEAM-WINNER.
           05 FILLER                   PIC XXX VALUE '***'.
           05 FILLER                   PIC X(5) VALUE 'TEAM '.
           05 O-WINNER                 PIC X.
           05 FILLER                   PIC X VALUE SPACE.
           05 FILLER                   PIC X(16)
                            VALUE 'IS THE WINNER!!!'.



       01 OE-LINE.
           05 OE-REC                   PIC X(71).
           05 FILLER                   PIC X VALUE SPACE.
           05 OE-MESG                  PIC X(60).


       01 TOTAL-ERR-LIT.
           05 FILLER                   PIC X(13) VALUE 'TOTAL ERRORS '.
           05 O-ERR-CRT                PIC Z,ZZ9.


       PROCEDURE DIVISION.

       L1-MAIN.
           PERFORM L2-INIT.
           PERFORM L2-MAINLINE
                   UNTIL EOF = 'T'.
           PERFORM L2-CLOSINGS.
           STOP RUN.

       L2-INIT.
           MOVE FUNCTION current-date        TO CURRENT-DATE-AND-TIME.
           MOVE CURRENT-DAY                  TO H1-DAY OF HEADING1.
           MOVE CURRENT-MONTH                TO H1-MONTH OF HEADING1.
           MOVE CURRENT-YEAR                 TO H1-YEAR OF HEADING1.
           MOVE CURRENT-DAY                  TO H1-DAY OF HEADING1-2.
           MOVE CURRENT-MONTH                TO H1-MONTH OF HEADING1-2.
           MOVE CURRENT-YEAR                 TO H1-YEAR OF HEADING1-2.
           OPEN INPUT POP-MASTER.
           OPEN OUTPUT PRTOUT.
           OPEN OUTPUT PRTOUTERR.

           PERFORM L3-HEADINGS-PRT.
           PERFORM L3-HEADINGS-ERR.
           PERFORM L3-READ.






       L2-MAINLINE.
           PERFORM L3-VALIDATION
                       THRU L3-VALIDATION-EXIT.
           IF NOT-ERROR
               PERFORM L3-CALCS
               PERFORM L3-OUTPUT
           ELSE 
               PERFORM L3-ERROR

           END-IF

           PERFORM L3-READ.



       L2-CLOSINGS.

           WRITE PRINTLINE of PRTOUT FROM GRAND-TOTALS-LIT
                               AFTER ADVANCING 3 LINES.
           MOVE COKE TO O-GT-POP-NUM-1.
           MOVE DIET-COKE TO O-GT-POP-NUM-2.
           MOVE MELLO-YELLO TO O-GT-POP-NUM-3.
           MOVE CHERRY-COKE TO O-GT-POP-NUM-4.

           MOVE 'COKE' TO O-GT-POP-LIT-1.
           MOVE 'DIET-COKE' TO O-GT-POP-LIT-2.
           MOVE 'MELLO-YELLO' TO O-GT-POP-LIT-3.
           MOVE 'CHERRY-COKE' TO O-GT-POP-LIT-4.

           WRITE PRINTLINE of PRTOUT FROM GRAND-TOTALS-LINE.

           MOVE DIET-CHERRY-COKE TO O-GT-POP-NUM-1.
           MOVE SPRITE TO O-GT-POP-NUM-2.
           MOVE DIET-SPRITE TO O-GT-POP-NUM-3.
           MOVE DASANI TO O-GT-POP-NUM-4.

           MOVE 'DIET-CHERRY-COKE' TO O-GT-POP-LIT-1.
           MOVE 'SPRITE' TO O-GT-POP-LIT-2.
           MOVE 'DIET-SPRITE' TO O-GT-POP-LIT-3.
           MOVE 'DASANI' TO O-GT-POP-LIT-4.

           WRITE PRINTLINE of PRTOUT FROM GRAND-TOTALS-LINE.

           MOVE C2 TO O-GT-POP-NUM-1.
           MOVE MR-PIBB TO O-GT-POP-NUM-2.
           MOVE DIET-LEMON-COKE TO O-GT-POP-NUM-3.
           MOVE VANILLA-COKE TO O-GT-POP-NUM-4.

           MOVE 'C2' TO O-GT-POP-LIT-1.
           MOVE 'MR-PIBB' TO O-GT-POP-LIT-2.
           MOVE 'DIET-LEMON-COKE' TO O-GT-POP-LIT-3.
           MOVE 'VANILLA-COKE' TO O-GT-POP-LIT-4.

           WRITE PRINTLINE of PRTOUT FROM GRAND-TOTALS-LINE.

           WRITE PRINTLINE OF PRTOUT FROM TEAM-TOTALS-LIT
                           AFTER ADVANCING 3 lines.
           MOVE 'A' TO O-TEAM-X.
           MOVE TEAM-A TO O-GT-TEAM-SALES.
           MOVE TEAM-A TO HOLD.

           WRITE PRINTLINE OF PRTOUT FROM TEAM-TOTALS.

           MOVE 'B' TO O-TEAM-X.
           MOVE TEAM-B TO O-GT-TEAM-SALES.

           IF TEAM-B > HOLD
               MOVE 'B' TO WHOLD
               MOVE TEAM-B TO HOLD
           END-IF.

           WRITE PRINTLINE OF PRTOUT FROM TEAM-TOTALS.

           MOVE 'C' TO O-TEAM-X.
           MOVE TEAM-C TO O-GT-TEAM-SALES.

           IF TEAM-C > HOLD
               MOVE TEAM-C TO HOLD
               MOVE 'C' TO WHOLD
           END-IF.

           WRITE PRINTLINE OF PRTOUT FROM TEAM-TOTALS.

           MOVE 'D' TO O-TEAM-X.
           MOVE TEAM-D TO O-GT-TEAM-SALES.

           IF TEAM-D > HOLD
               MOVE TEAM-D TO HOLD
               MOVE 'D' TO WHOLD
           END-IF.

           WRITE PRINTLINE OF PRTOUT FROM TEAM-TOTALS.

           MOVE 'E' TO O-TEAM-X.
           MOVE TEAM-E TO O-GT-TEAM-SALES.

           IF TEAM-E > HOLD
               MOVE TEAM-E TO HOLD
               MOVE 'E' TO WHOLD
           END-IF.

           WRITE PRINTLINE OF PRTOUT FROM TEAM-TOTALS.


           MOVE WHOLD TO O-WINNER.

           WRITE PRINTLINE OF PRTOUT FROM TEAM-WINNER.

           MOVE ERR-CRT TO O-ERR-CRT.

           WRITE PRINTLINE of PRTOUTERR FROM TOTAL-ERR-LIT
                           AFTER ADVANCING 2 LINES.

       L3-READ.
           READ POP-MASTER
               AT end  
                   MOVE 'T' TO EOF.



       L3-OUTPUT.
           
           MOVE P-LNAME OF I-REC TO P-LNAME of DETAIL-LINE.
           MOVE P-FNAME OF I-REC TO P-FNAME of DETAIL-LINE.
           MOVE P-CITY of I-REC TO P-CITY of DETAIL-LINE.
           MOVE P-STATE of I-REC TO P-STATE of DETAIL-LINE.

           MOVE P-ZIP TO P-ZIP-CODE OF MISIC.
           
           MOVE O-ZIP-1 of MISIC TO O-ZIP-1 of DETAIL-LINE.
           MOVE O-ZIP-2 OF MISIC TO O-ZIP-2 OF DETAIL-LINE. 

           MOVE P-CASES TO O-QTY.
           MOVE POP-DEPO-AMT TO O-DEPO-AMT.
           MOVE TOTAL TO O-TOTAL-SALES.

           WRITE PRINTLINE of PRTOUT FROM DETAIL-LINE
                           AFTER ADVANCING 2 lines.


       L3-HEADINGS-PRT.
           ADD 1 TO C-PCTR.
           MOVE C-PCTR TO H1-PAGE.
           WRITE PRINTLINE OF PRTOUT FROM HEADING1
                       AFTER ADVANCING PAGE.

           WRITE PRINTLINE OF PRTOUT FROM HEADING2
                       AFTER ADVANCING 1 LINE.

           WRITE PRINTLINE OF PRTOUT FROM HEADING3
                       AFTER ADVANCING 1 LINES.
           
           WRITE PRINTLINE OF PRTOUT FROM COL-HEADING1
                   AFTER ADVANCING 2 LINES.
           

      ******NEED TO PIC FOR ERROR  AND MAKE HEADING CORRECT******
       L3-HEADINGS-ERR.

           ADD 1 TO C-PCTR-2.
           MOVE C-PCTR-2 TO H1-PAGE-2.
           WRITE PRINTLINE OF PRTOUTERR FROM HEADING1-2
                       AFTER ADVANCING PAGE.

           WRITE PRINTLINE OF PRTOUTERR FROM HEADING2
                       AFTER ADVANCING 1 LINE.

           WRITE PRINTLINE OF PRTOUTERR FROM HEADING3-2
                       AFTER ADVANCING 1 LINE.
           
           WRITE PRINTLINE OF PRTOUTERR FROM HEADING4-2
                   AFTER ADVANCING 2 LINES.

      ******THIS MAY NOT WORK RIGHT**********************************
       L3-VALIDATION.
           MOVE 'N' TO ERROR-SW.

           IF P-LNAME OF I-REC EQUAL spaces
               MOVE 'LAST NAME REQUIRED' TO OE-MESG
               MOVE 'Y' TO ERROR-SW
               GO TO L3-VALIDATION-EXIT.

           IF P-FNAME OF I-REC EQUAL spaces
               MOVE 'FIRST NAME REQUIRED' TO OE-MESG
               MOVE 'Y' TO ERROR-SW
               GO TO L3-VALIDATION-EXIT.

           IF P-ADDRESS EQUAL SPACES
               MOVE 'ADDRESS REQUIRED' TO OE-MESG
               MOVE 'Y' TO ERROR-SW
               GO TO L3-VALIDATION-EXIT.

           IF P-CITY OF I-REC EQUAL spaces
               MOVE 'CITY REQUIRED' TO OE-MESG
               MOVE 'Y' TO ERROR-SW
               GO TO L3-VALIDATION-EXIT.

           MOVE P-STATE of I-REC TO C-STATE.

           IF NOT VALID-STATES 
                
                   MOVE 'INVALID STATE' TO OE-MESG
                   MOVE 'Y' TO ERROR-SW.

           
           
           IF P-ZIP NOT NUMERIC
               MOVE 'ZIP CODE NOT NUMERIC' TO OE-MESG
               MOVE 'Y' TO ERROR-SW
               GO TO L3-VALIDATION-EXIT.

           IF P-POP-TYPE NOT NUMERIC
               MOVE 'POP TYPE NOT NUMERIC' TO OE-MESG
               MOVE 'Y' TO ERROR-SW
               GO TO L3-VALIDATION-EXIT.


           MOVE P-POP-TYPE TO C-POP-TYPE.
           IF NOT VALID-POP-TYPES
               MOVE 'INVALID POP TYPE' TO OE-MESG
               MOVE 'Y' TO ERROR-SW
               GO TO L3-VALIDATION-EXIT.
           
           
               
           IF P-CASES NOT NUMERIC 
               MOVE 'NUMBER OF CASES NOT NUMERIC' TO OE-MESG
               MOVE 'Y' TO ERROR-SW
               GO TO L3-VALIDATION-EXIT.


           MOVE P-CASES TO C-CASES.

           IF NOT-VALID-CASES
           
               MOVE 'NUMBER OF CASES MUST BE >= 1' TO OE-MESG
               MOVE 'Y' TO ERROR-SW
               GO TO L3-VALIDATION-EXIT.

           MOVE P-TEAM TO C-TEAMS.

           IF NOT VALID-TEAMS

                 MOVE 'INVALID TEAM' TO OE-MESG
                 MOVE 'Y' TO ERROR-SW
                 GO TO L3-VALIDATION-EXIT.
           
           



       L3-VALIDATION-EXIT.
           exit.



       L3-ERROR.
           ADD 1 TO ERR-CRT.
           MOVE I-REC TO OE-REC.
           WRITE PRINTLINE of PRTOUTERR FROM OE-LINE
                       AFTER ADVANCING 1 LINE
           AT eop
               PERFORM L3-HEADINGS-ERR.




       L3-CALCS.
           EVALUATE P-STATE OF I-REC
             WHEN 'IA'
               MOVE 0.05 TO POP-DEPOSIT
             WHEN 'NE'
               MOVE 0.05 TO POP-DEPOSIT
             WHEN 'WI'
               MOVE 0.05 TO POP-DEPOSIT
             WHEN 'MI'
               MOVE 0.10 TO POP-DEPOSIT
             WHEN 'IL'
               MOVE 0.00 TO POP-DEPOSIT
             WHEN 'MO'
               MOVE 0.00 TO POP-DEPOSIT
           END-EVALUATE.





           


           EVALUATE P-POP-TYPE
               WHEN 1
                   COMPUTE COKE = (COKE + P-CASES)
                   MOVE 'COKE' TO O-POP-LIT
               WHEN 2
                   COMPUTE DIET-COKE = DIET-COKE + P-CASES
                   MOVE 'DIET-COKE' TO O-POP-LIT
               WHEN 3
                   COMPUTE MELLO-YELLO = MELLO-YELLO + P-CASES
                   MOVE 'MELLO-YELLO' TO O-POP-LIT
               WHEN 4
                   COMPUTE CHERRY-COKE = CHERRY-COKE + P-CASES
                   MOVE 'CHERRY-COKE' TO O-POP-LIT
               WHEN 5
                   COMPUTE DIET-CHERRY-COKE = DIET-CHERRY-COKE + P-CASES
                   MOVE 'DIET-CHERRY-COKE' TO O-POP-LIT
               WHEN 6
                   COMPUTE SPRITE = SPRITE + P-CASES
                   MOVE 'SPRITE' TO O-POP-LIT
               WHEN 7
                   COMPUTE DIET-SPRITE = DIET-SPRITE + P-CASES
                   MOVE 'DIET-SPRITE' TO O-POP-LIT
               WHEN 8
                   COMPUTE DASANI = DASANI + P-CASES
                   MOVE 'DASANI' TO O-POP-LIT
               WHEN 9
                   COMPUTE C2 = C2 + P-CASES
                   MOVE 'C2' TO O-POP-LIT
               WHEN 10
                   COMPUTE MR-PIBB = MR-PIBB + P-CASES 
                   MOVE 'MR-PIBB' TO O-POP-LIT
               WHEN 11
                   COMPUTE DIET-LEMON-COKE = DIET-LEMON-COKE + P-CASES
                   MOVE 'DIET-LEMON-COKE' TO O-POP-LIT
               WHEN 12
                   COMPUTE VANILLA-COKE = VANILLA-COKE + P-CASES
                   MOVE 'VANILLA-COKE' TO O-POP-LIT
           END-EVALUATE.

           COMPUTE POP-DEPO-AMT = POP-DEPOSIT * 24.
           COMPUTE TOTAL = ((18.71 * P-CASES) + POP-DEPO-AMT).

           EVALUATE P-TEAM
               WHEN 'A'
                   COMPUTE TEAM-A = TEAM-A + TOTAL
               WHEN 'B'
                   COMPUTE TEAM-B = TEAM-B + TOTAL
               WHEN 'C'
                   COMPUTE TEAM-C = TEAM-C + TOTAL
               WHEN 'D'
                   COMPUTE TEAM-D = TEAM-D + TOTAL
               WHEN 'E'
                   COMPUTE TEAM-E = TEAM-E + TOTAL
           END-EVALUATE



           IF TOTAL > 250.00
               MOVE '***' TO FLAG-AST
           ELSE
               MOVE '   ' TO FLAG-AST
           END-IF.


       










           
           
       END PROGRAM CBLDSM04.