      ******************************************************************
      * Author: Cho Zin Nwe
      * Date: 17.6.2026
      * Purpose: Phone Insurance Project Screen 4
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. Screen4.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT question-file ASSIGN TO "screen4_questions.txt"
           ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.
       FD question-file.
       01 question-record.
           05 q-text PIC X(50).

       WORKING-STORAGE SECTION.
       01 EOF PIC X VALUE "N".
       01 ws-response PIC X.
       01 ws-total-questions PIC 99 VALUE 0.
       01 ws-yes-count PIC 99 VALUE 0.
       01 ws-answers.
           05 ans1 PIC X.
           05 ans2 PIC X.
           05 ans3 PIC X.
           05 ans4 PIC X.

       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
            DISPLAY "========================================="
            DISPLAY "       Device Insurance Underwriting     "
            DISPLAY "========================================="

            OPEN INPUT question-file
            READ question-file
            NEXT RECORD
            AT END MOVE "Y" TO EOF
            END-READ

            PERFORM UNTIL EOF = 'Y'
            ADD 1 TO ws-total-questions
            READ question-file
            AT END MOVE "Y" TO EOF
            END-READ

            DISPLAY ws-total-questions". " FUNCTION TRIM(q-text)
            DISPLAY "Your Answer(Y/N) : "
            ACCEPT ws-response
            MOVE FUNCTION UPPER-CASE(ws-response) TO ws-response

            PERFORM UNTIL ws-response = 'Y' OR ws-response = 'N'
            DISPLAY "Invalid Answer. Type Y or N."
            ACCEPT ws-response
            MOVE FUNCTION UPPER-CASE(ws-response) TO ws-response
            END-PERFORM

            EVALUATE ws-total-questions
            WHEN 1
            MOVE ws-response TO ans1
            WHEN 2
            MOVE ws-response TO ans2
            WHEN 3
            MOVE ws-response TO ans3
            WHEN 4
            MOVE ws-response TO ans3
            END-EVALUATE

            IF ws-response = 'Y'
               ADD 1 TO ws-yes-count
            END-IF

            END-PERFORM
            CLOSE question-file.

            IF ws-yes-count = ws-total-questions AND
                ws-total-questions > 0
                DISPLAY "Result : Rejected!"
            ELSE
                DISPLAY "Result : Processed!"
            END-IF.
            DISPLAY "=================================================="


            STOP RUN.
       END PROGRAM Screen4.
