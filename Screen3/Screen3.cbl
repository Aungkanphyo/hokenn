      ******************************************************************
      * Author:
      * Date:
      * Purpose:
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. Screen3.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT OPTIONAL CUST-TABLE 
           ASSIGN TO "Screen3/T_Application.csv"
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS FS-CUST.

       DATA DIVISION.
       FILE SECTION.
       FD  CUST-TABLE.
       01  CUST-RECORD            PIC X(300).

       WORKING-STORAGE SECTION.
       01  FS-CUST                PIC XX.

       01  WS-INPUT-FIELDS.
           05  WS-CUST-NAME       PIC X(20) VALUE SPACES.
           05  WS-EMAIL           PIC X(25) VALUE SPACES.
           05  WS-DOB             PIC X(8)  VALUE SPACES.
           05  WS-ADDRESS         PIC X(40) VALUE SPACES.

       01  WS-PHONE-BUFFER        PIC X(15) VALUE SPACES.
       01  WS-PHONE-FINAL         PIC X(11) VALUE SPACES.


       01  WS-HEADER-LINE.
           05  FILLER PIC X(30) VALUE "NAME,PHONE,EMAIL,DOB,ADDRESS,".
           05  FILLER PIC X(30) VALUE "DATE,TIME,STATUS,DEVICE_TYPE,".
           05  FILLER PIC X(30) VALUE "DEVICE_MODEL,PRICE,PURCHASE_".
           05  FILLER PIC X(30) VALUE "DATE,COVERAGE_PERIOD,PREMIUM,".
           05  FILLER PIC X(10) VALUE "PLAN_CODE".

       01  WS-TEMP-NUMBERS.
           05  WS-TEMP-PRICE      PIC ZZZZZZZ9.
           05  WS-TEMP-PERIOD     PIC Z9.
           05  WS-TEMP-PREMIUM    PIC ZZZZZZZ9.

       01  WS-VALIDATION-FLAGS.
           05  WS-ERR-FLAG        PIC 9     VALUE 0.
           05  WS-ERR-MSG         PIC X(50) VALUE SPACES.
           05  WS-SPACE-COUNT     PIC 99    VALUE 0.
           05  WS-AT-COUNT        PIC 99    VALUE 0.
           05  WS-DOT-COUNT       PIC 99    VALUE 0.

       01  WS-SYSTEM-METADATA.
           05  CURRENT-SYS-DATE.
               10  SYS-YEAR       PIC X(4).
               10  SYS-MONTH      PIC X(2).
               10  SYS-DAY        PIC X(2).
           05  CURRENT-SYS-TIME.
               10  SYS-HOUR       PIC X(2).
               10  SYS-MIN        PIC X(2).
               10  SYS-SEC        PIC X(2).
               10  SYS-MS         PIC X(2).

       01  WS-FINAL-STATUS        PIC X(15) VALUE "PENDING".
       01  WS-RESPONSE            PIC X     VALUE SPACES.

       01  WS-STEP-TRACKER        PIC 9     VALUE 1.

      *Receiving all data sent from Screen 1 & 2
       LINKAGE SECTION.
       01 LNK-CombineData.
           05 LNK-C-DeviceType     PIC X(10).
           05 LNK-C-DeviceModel    PIC X(25).
           05 LNK-C-PurchasePrice  PIC 9(8).
           05 LNK-C-PurchaseDate   PIC X(10).
           05 LNK-C-CoveragePeriod PIC 99.
           05 LNK-C-EstPremium     PIC 9(8).
           05 LNK-C-PlanCode       PIC X(5). 

       PROCEDURE DIVISION USING LNK-CombineData.
       MAIN-FLOW.
           PERFORM INITIALIZE-CSV-HEADER

           DISPLAY "== SYSTEM ONLINE. AWAITING ENTRY ==".
           MOVE 1 TO WS-STEP-TRACKER.

       DISPLAY-LOOP.
           DISPLAY "-------------------------------------------------".

           EVALUATE WS-STEP-TRACKER
               WHEN 1
                   MOVE SPACES TO WS-CUST-NAME
                   DISPLAY "Enter Customer Name :"
                   ACCEPT WS-CUST-NAME
                   PERFORM VALIDATE-NAME

               WHEN 2
                   MOVE SPACES TO WS-PHONE-BUFFER
                   DISPLAY "Enter Phone Number (Must be 11 digits):"
                   ACCEPT WS-PHONE-BUFFER
                   PERFORM VALIDATE-PHONE

               WHEN 3
                   MOVE SPACES TO WS-EMAIL
                   DISPLAY "Enter Email Address :"
                   ACCEPT WS-EMAIL
                   PERFORM VALIDATE-EMAIL
               WHEN 4
                   MOVE SPACES TO WS-DOB
                   DISPLAY "Enter DOB (YYYYMMDD):"
                   ACCEPT WS-DOB
                   PERFORM VALIDATE-DOB

               WHEN 5
                   MOVE SPACES TO WS-ADDRESS
                   DISPLAY "Enter Physical Address :"
                   ACCEPT WS-ADDRESS
                   PERFORM VALIDATE-ADDRESS
           END-EVALUATE.

           IF WS-ERR-FLAG = 1
               DISPLAY WS-ERR-MSG
               GO TO DISPLAY-LOOP
           ELSE
               ADD 1 TO WS-STEP-TRACKER
               IF WS-STEP-TRACKER <= 5
                   GO TO DISPLAY-LOOP
               ELSE
                   PERFORM GENERATE-AND-DISPLAY-SUMMARY
                   PERFORM SAVE-DATA-RECORD
                   DISPLAY WS-ERR-MSG
               END-IF
           END-IF.

           DISPLAY "Press Enter to exit application...".
           ACCEPT WS-RESPONSE.
           STOP RUN.

      *----------------------------------------------------------------
      * INPUT VALIDATION PARAGRAPHS
      *----------------------------------------------------------------
       VALIDATE-NAME.
           MOVE 0 TO WS-ERR-FLAG.
           IF WS-CUST-NAME = SPACES
               MOVE 1 TO WS-ERR-FLAG
               MOVE "ERROR: NAME FIELD CANNOT BE BLANK!" TO WS-ERR-MSG
           END-IF.

       VALIDATE-PHONE.
           MOVE 0 TO WS-ERR-FLAG.
           IF WS-PHONE-BUFFER = SPACES
               MOVE 1 TO WS-ERR-FLAG
               MOVE "ERROR: PHONE NUMBER IS REQUIRED!" TO WS-ERR-MSG
           ELSE
               IF WS-PHONE-BUFFER(11:1) = " "
                   MOVE 1 TO WS-ERR-FLAG
                   MOVE "ERROR: PHONE IS TOO SHORT! NEED 11 DIGITS."
                     TO WS-ERR-MSG
               ELSE
                   IF WS-PHONE-BUFFER(12:4) NOT = SPACES
                       MOVE 1 TO WS-ERR-FLAG
                       MOVE "ERROR: PHONE IS TOO LONG! MAX 11 DIGITS."
                         TO WS-ERR-MSG
                   ELSE
                       IF WS-PHONE-BUFFER(1:11) NOT NUMERIC
                           MOVE 1 TO WS-ERR-FLAG
                           MOVE "ERROR: PHONE MUST BE NUMERIC ONLY!"
                             TO WS-ERR-MSG
                       ELSE
                           MOVE WS-PHONE-BUFFER(1:11) TO WS-PHONE-FINAL
                       END-IF
                   END-IF
               END-IF
           END-IF.

       VALIDATE-EMAIL.
           MOVE 0 TO WS-ERR-FLAG.
           IF WS-EMAIL = SPACES
               MOVE 1 TO WS-ERR-FLAG
               MOVE "ERROR: EMAIL ADDRESS IS REQUIRED!" TO WS-ERR-MSG
           ELSE
               MOVE 0 TO WS-SPACE-COUNT
               INSPECT WS-EMAIL TALLYING WS-SPACE-COUNT FOR ALL " "
               IF WS-SPACE-COUNT > 0 AND WS-EMAIL(1:10) = " "
                   MOVE 1 TO WS-ERR-FLAG
                   MOVE "ERROR: SPACES NOT ALLOWED!" TO WS-ERR-MSG
               END-IF

               MOVE 0 TO WS-AT-COUNT
               MOVE 0 TO WS-DOT-COUNT
               INSPECT WS-EMAIL TALLYING WS-AT-COUNT FOR ALL "@"
               INSPECT WS-EMAIL TALLYING WS-DOT-COUNT FOR ALL "."
               IF WS-ERR-FLAG = 0 AND
                  (WS-AT-COUNT = 0 OR WS-DOT-COUNT = 0)
                   MOVE 1 TO WS-ERR-FLAG
                   MOVE "ERROR: INVALID EMAIL FORMAT!" TO WS-ERR-MSG
               END-IF
           END-IF.

       VALIDATE-DOB.
           MOVE 0 TO WS-ERR-FLAG.
           IF WS-DOB = SPACES
               MOVE 1 TO WS-ERR-FLAG
               MOVE "ERROR: DOB IS REQUIRED!" TO WS-ERR-MSG
           ELSE
               IF WS-DOB NOT NUMERIC
                   MOVE 1 TO WS-ERR-FLAG
                   MOVE "ERROR: DOB MUST BE NUMERIC!" TO WS-ERR-MSG
               END-IF
           END-IF.

       VALIDATE-ADDRESS.
           MOVE 0 TO WS-ERR-FLAG.
           IF WS-ADDRESS = SPACES
               MOVE 1 TO WS-ERR-FLAG
               MOVE "ERROR: ADDRESS CANNOT BE BLANK!" TO WS-ERR-MSG
           END-IF.

      *----------------------------------------------------------------
      * AUTOMATED METADATA & SUMMARY SCREEN DISPLAY
      *----------------------------------------------------------------
       GENERATE-AND-DISPLAY-SUMMARY.
           MOVE FUNCTION CURRENT-DATE TO WS-SYSTEM-METADATA.
           ACCEPT CURRENT-SYS-TIME FROM TIME.
           MOVE "PENDING" TO WS-FINAL-STATUS.

           DISPLAY "=================================================".
           DISPLAY "         APPLICATION SUBMISSION SUCCESS          ".
           DISPLAY "=================================================".
           DISPLAY "Generated Status : " WS-FINAL-STATUS.
           DISPLAY "Created Date     : " SYS-YEAR "/" SYS-MONTH "/"
           SYS-DAY.
           DISPLAY "Created Time     : " SYS-HOUR ":" SYS-MIN ":"
           SYS-SEC.
           DISPLAY "=================================================".

      *----------------------------------------------------------------
      * INITIALIZE HEADER ROW
      *----------------------------------------------------------------
       INITIALIZE-CSV-HEADER.
           OPEN INPUT CUST-TABLE.


           IF FS-CUST = "35" OR FS-CUST = "10"
               CLOSE CUST-TABLE
               OPEN OUTPUT CUST-TABLE
               MOVE WS-HEADER-LINE TO CUST-RECORD
               WRITE CUST-RECORD
               CLOSE CUST-TABLE
           ELSE

               READ CUST-TABLE AT END MOVE "10" TO FS-CUST
               END-READ
               CLOSE CUST-TABLE


               IF CUST-RECORD = SPACES OR CUST-RECORD(1:4) NOT = "NAME"
                   OPEN OUTPUT CUST-TABLE
                   MOVE WS-HEADER-LINE TO CUST-RECORD
                   WRITE CUST-RECORD
                   CLOSE CUST-TABLE
               END-IF
           END-IF.

      *----------------------------------------------------------------
      * SAVE ROW RECORD TO CSV FILE
      *----------------------------------------------------------------
       SAVE-DATA-RECORD.
           MOVE FUNCTION CURRENT-DATE TO WS-SYSTEM-METADATA.
           ACCEPT CURRENT-SYS-TIME FROM TIME.

      *    Formatting Numerics to Remove Leading Zeros and Spaces
           MOVE LNK-C-PurchasePrice TO WS-TEMP-PRICE.
           MOVE LNK-C-CoveragePeriod TO WS-TEMP-PERIOD.
           MOVE LNK-C-EstPremium TO WS-TEMP-PREMIUM.

           OPEN EXTEND CUST-TABLE.

      *Combine all data into one line using STRING, separated by commas
           STRING
               FUNCTION TRIM(WS-CUST-NAME) DELIMITED BY SIZE ","
               FUNCTION TRIM(WS-PHONE-FINAL)     DELIMITED BY SIZE ","
               FUNCTION TRIM(WS-EMAIL)           DELIMITED BY SIZE ","
               FUNCTION TRIM(WS-DOB)             DELIMITED BY SIZE ","
               FUNCTION TRIM(WS-ADDRESS)         DELIMITED BY SIZE ","
               CURRENT-SYS-DATE                  DELIMITED BY SIZE ","
               CURRENT-SYS-TIME(1:6)             DELIMITED BY SIZE ","
               FUNCTION TRIM(WS-FINAL-STATUS)    DELIMITED BY SIZE ","
               FUNCTION TRIM(LNK-C-DeviceType)   DELIMITED BY SIZE ","
               FUNCTION TRIM(LNK-C-DeviceModel)  DELIMITED BY SIZE ","
               FUNCTION TRIM(WS-TEMP-PRICE)      DELIMITED BY SIZE ","
               FUNCTION TRIM(LNK-C-PurchaseDate) DELIMITED BY SIZE ","
               FUNCTION TRIM(WS-TEMP-PERIOD)     DELIMITED BY SIZE ","
               FUNCTION TRIM(WS-TEMP-PREMIUM)    DELIMITED BY SIZE ","
               FUNCTION TRIM(LNK-C-PlanCode)     DELIMITED BY SIZE
               INTO CUST-RECORD
           END-STRING.
           
           WRITE CUST-RECORD.
           CLOSE CUST-TABLE.

           MOVE"SUCCESS: Data saved successfully to CSV!" TO WS-ERR-MSG.
       END PROGRAM Screen3.
       