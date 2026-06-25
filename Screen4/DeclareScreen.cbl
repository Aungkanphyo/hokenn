******************************************************************
      * Author: Cho Zin Nwe
      * Date: 23.6.2026
      * Purpose: Phone Insurance Project Screen 4 (Dynamic Score from File)
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. Screen4.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT question-file 
           ASSIGN TO "Data/screen4_questions.txt"
           ORGANIZATION IS LINE SEQUENTIAL.

           SELECT OPTIONAL output-file 
           ASSIGN TO "Data/screen4_output.csv"
           ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.
       FD question-file.
       01 question-record          PIC X(150).

       FD output-file.
       01 output-record.
           05 output-data          PIC X(1000).

       WORKING-STORAGE SECTION.
       01 EOF                      PIC X VALUE "N".
       01 ws-response              PIC X.
       01 ws-total-questions       PIC 99 VALUE 0.
       01 ws-total-score           PIC 999 VALUE 0.
       01 ws-status-result         PIC X(11) VALUE SPACES.

       *> .txt ထဲက ခွဲထုတ်ဖတ်မည့် Variable များ
       01 ws-current-q-text        PIC X(100).
       01 ws-file-score            PIC X(3).
       
       *> ဖိုင်ထဲကရမှတ်ကို ဂဏန်းအဖြစ်ပြောင်းရန်နှင့် လက်ရှိမှတ်သိမ်းရန်
       01 ws-numeric-score         PIC 99 VALUE 0.
       01 ws-current-score         PIC 99 VALUE 0.
       01 ws-current-score-disp    PIC Z9.

       01 WS-CURRENT-DATE-DATA.
           05 WS-CURRENT-YEAR      PIC 9(4).
           05 WS-CURRENT-MONTH     PIC 9(2).
           05 WS-CURRENT-DAY       PIC 9(2).
           05 FILLER               PIC X(13).

       01 ws-answers.
           05 damage_flg           PIC X VALUE "N".
           05 screen_flg           PIC X VALUE "N".
           05 water_flg            PIC X VALUE "N".
           05 old_device_flg       PIC X VALUE "N".
           05 spare_flg            PIC X VALUE "N".

       01 ws-csv-line              PIC X(1000).

       LINKAGE SECTION.
       01  LNK-IMEI                PIC X(15).

       PROCEDURE DIVISION USING LNK-IMEI.
       MAIN-PROCEDURE.
            DISPLAY "========================================="
            DISPLAY "       Device Insurance Underwriting     "
            DISPLAY "========================================="

            MOVE FUNCTION CURRENT-DATE TO WS-CURRENT-DATE-DATA

            *> CSV အစပိုင်း တည်ဆောက်ခြင်း (IMEI , ရက်စွဲ)
            INITIALIZE ws-csv-line
            STRING
                FUNCTION TRIM(LNK-IMEI) " , "
                WS-CURRENT-YEAR "/" WS-CURRENT-MONTH "/" WS-CURRENT-DAY
                DELIMITED BY SIZE
                INTO ws-csv-line

            OPEN INPUT question-file
            READ question-file
                AT END MOVE "Y" TO EOF
            END-READ

            PERFORM UNTIL EOF = 'Y'
                ADD 1 TO ws-total-questions

                INITIALIZE ws-current-q-text ws-file-score
                UNSTRING question-record DELIMITED BY ","
                    INTO ws-current-q-text, ws-file-score
                END-UNSTRING

               
                MOVE ws-file-score TO ws-numeric-score

                DISPLAY ws-total-questions ". " 
                        FUNCTION TRIM(ws-current-q-text)
                DISPLAY "Your Answer(Y/N) : " WITH NO ADVANCING
                ACCEPT ws-response
                MOVE FUNCTION UPPER-CASE(ws-response) TO ws-response

                PERFORM UNTIL ws-response = 'Y' OR ws-response = 'N'
                    DISPLAY "Invalid Answer. Type Y or N."
                    DISPLAY "Your Answer(Y/N) : " WITH NO ADVANCING
                    ACCEPT ws-response
                    MOVE FUNCTION UPPER-CASE(ws-response) TO ws-response
                END-PERFORM

              
                EVALUATE ws-total-questions
                    WHEN 1 MOVE ws-response TO damage_flg
                    WHEN 2 MOVE ws-response TO screen_flg
                    WHEN 3 MOVE ws-response TO water_flg
                    WHEN 4 MOVE ws-response TO old_device_flg
                    WHEN 5 MOVE ws-response TO spare_flg
                END-EVALUATE

               
                IF ws-response = 'Y' THEN
                    ADD ws-numeric-score TO ws-total-score
                    MOVE ws-numeric-score TO ws-current-score
                ELSE
                    MOVE 0 TO ws-current-score
                END-IF

              
                MOVE ws-current-score TO ws-current-score-disp
                STRING
                    FUNCTION TRIM(ws-csv-line) " , "
                    FUNCTION TRIM(ws-current-q-text) " : " 
                    FUNCTION TRIM(ws-current-score-disp)
                    DELIMITED BY SIZE
                    INTO ws-csv-line

                READ question-file
                    AT END MOVE "Y" TO EOF
                END-READ
            END-PERFORM
            
            CLOSE question-file.

           
            EVALUATE TRUE
                WHEN ws-total-score >= 71
                    MOVE "REJECTED" TO ws-status-result
                WHEN ws-total-score >= 31 AND ws-total-score <= 70
                    IF damage_flg = 'Y' OR water_flg = 'Y' THEN
                        MOVE "REJECTED" TO ws-status-result
                    ELSE
                        MOVE "PENDING" TO ws-status-result
                    END-IF
                WHEN OTHER
                    MOVE "PENDING" TO ws-status-result
            END-EVALUATE.

            
            STRING
                FUNCTION TRIM(ws-csv-line) " , "
                "Status : " FUNCTION TRIM(ws-status-result)
                DELIMITED BY SIZE
                INTO ws-csv-line

            OPEN EXTEND output-file
            MOVE ws-csv-line TO output-record
            WRITE output-record
            CLOSE output-file.

            IF ws-status-result = "PENDING" THEN
                CALL 'screen5' USING LNK-IMEI
            ELSE
                DISPLAY "-----------------------------------------"
                DISPLAY "Total Risk Score : " ws-total-score
                DISPLAY "Status Result    : " ws-status-result
                DISPLAY "-----------------------------------------"
                DISPLAY "FAILED : Insurance cannot be provided "
                DISPLAY "due to high risk."
            END-IF.
            
            STOP RUN.
       END PROGRAM Screen4.
       