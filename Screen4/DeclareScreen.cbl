      ******************************************************************
      * Author: Cho Zin Nwe
      * Date: 23.6.2026
      * Purpose: Phone Insurance Project Screen 4
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. Screen4.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT question-file 
           ASSIGN TO "Screen4/screen4_questions.txt"
           ORGANIZATION IS LINE SEQUENTIAL.

           SELECT OPTIONAL output-file 
           ASSIGN TO "Screen4/screen4_output.csv"
           ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.
       FD question-file.
       01 question-record.
           05 q-text PIC X(100).

       FD output-file.
       01 output-record.
           05 output-data PIC X(500).

       WORKING-STORAGE SECTION.
       01 EOF PIC X VALUE "N".
       01 ws-response PIC X.
       01 ws-total-questions PIC 99 VALUE 0.
       01 ws-total-score PIC 999 VALUE 0.
       01 ws-score-display PIC ZZ9.
       01 ws-status-result PIC X(11) VALUE SPACES.

       01 WS-CURRENT-DATE-DATA.
           05 WS-CURRENT-YEAR PIC 9(4).
           05 WS-CURRENT-MONTH PIC 9(2).
           05 WS-CURRENT-DAY PIC 9(2).
           05 FILLER PIC X(13).

       01 ws-answers.
           05 damage_flg PIC X.
           05 screen_flg PIC X.
           05 water_flg PIC X.
           05 old_device_flg PIC X.
           05 spare_flg PIC X.

       01 ws-csv-line PIC X(500).

       LINKAGE SECTION.
       01  LNK-IMEI           PIC X(15).

       PROCEDURE DIVISION USING LNK-IMEI.
       MAIN-PROCEDURE.
            DISPLAY "========================================="
            DISPLAY "       Device Insurance Underwriting     "
            DISPLAY "========================================="

      *>       Read questions from file and collect answers from user
            OPEN INPUT question-file
            READ question-file
            AT END MOVE "Y" TO EOF
            END-READ

            PERFORM UNTIL EOF = 'Y'
            ADD 1 TO ws-total-questions

            DISPLAY ws-total-questions". " FUNCTION TRIM(q-text)
            DISPLAY "Your Answer(Y/N) : "
            ACCEPT ws-response
            MOVE FUNCTION UPPER-CASE(ws-response) TO ws-response

      *>       Validate Input : Only Y or N
            PERFORM UNTIL ws-response = 'Y' OR ws-response = 'N'
            DISPLAY "Invalid Answer. Type Y or N."
            ACCEPT ws-response
            MOVE FUNCTION UPPER-CASE(ws-response) TO ws-response
            END-PERFORM

      *>       Risk Scoring Logic
            EVALUATE ws-total-questions
            WHEN 1
            MOVE ws-response TO damage_flg
            IF damage_flg = 'Y'
                ADD 50 TO ws-total-score
            END-IF
            WHEN 2
            MOVE ws-response TO screen_flg
            IF screen_flg = 'Y'
                ADD 30 TO ws-total-score
            END-IF
            WHEN 3
            MOVE ws-response TO water_flg
            IF water_flg = 'Y'
                ADD 40 TO ws-total-score
            END-IF
            WHEN 4
            MOVE ws-response TO old_device_flg
            IF old_device_flg = 'Y'
                ADD 20 TO ws-total-score
            END-IF
            WHEN 5
            MOVE ws-response TO spare_flg
            IF spare_flg = 'Y'
                ADD 20 TO ws-total-score
            END-IF
            END-EVALUATE

            READ question-file
            AT END MOVE "Y" TO EOF
            END-READ

            END-PERFORM
            CLOSE question-file.

      *>       Decision Threshold "Approved" or "Rejected"
            EVALUATE TRUE
            WHEN ws-total-score <= 30
            MOVE "APPROVED" TO ws-status-result

            WHEN ws-total-score >= 31 AND ws-total-score <= 70
            MOVE "PENDING" TO ws-status-result
            IF damage_flg = 'Y' OR water_flg = 'Y' THEN
                MOVE "REJECTED" TO ws-status-result
            ELSE
                MOVE "APPROVED" TO ws-status-result
            END-IF

            WHEN ws-total-score >= 71
            MOVE "REJECTED" TO ws-status-result
            END-EVALUATE

            DISPLAY "================== RESULT ========================"
            MOVE ws-total-score TO ws-score-display
           DISPLAY "Total Risk Score : " FUNCTION TRIM(ws-score-display)
           DISPLAY "Status Result    : " FUNCTION TRIM(ws-status-result)
            DISPLAY "=================================================="

      * Insert current date
           MOVE FUNCTION CURRENT-DATE TO WS-CURRENT-DATE-DATA

      *>       Writing Output CSV File
            STRING
            FUNCTION TRIM(LNK-IMEI) " , "
            WS-CURRENT-YEAR "/" WS-CURRENT-MONTH "/" WS-CURRENT-DAY ","
            "Device Damaged? : " FUNCTION TRIM(damage_flg) " , "
            "Screen Cracked? : " FUNCTION TRIM(screen_flg) " , "
            "Water Damaged? : " FUNCTION TRIM(water_flg) " , "
            "Very Old Device? : " FUNCTION TRIM(old_device_flg) " , "
            "Repaired Before? : " FUNCTION TRIM(spare_flg) " , "
            "Total score : " FUNCTION TRIM(ws-score-display) " , "
            "Status : " FUNCTION TRIM(ws-status-result)
            DELIMITED BY SIZE
            INTO ws-csv-line

            OPEN EXTEND output-file
            MOVE ws-csv-line TO output-record
            WRITE output-record
            CLOSE output-file.

      *>       Display message based on result status
            IF ws-status-result = "APPROVED" THEN
                DISPLAY "SUCCESS : Insurance approved. "
                "Processing batch tonight."
            ELSE
                DISPLAY "FAILED : Insurance cannot be provided "
                "due to high risk."
            END-IF.

            STOP RUN.
       END PROGRAM Screen4.
