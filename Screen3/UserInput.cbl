      ******************************************************************
      * Author: GLOBAL-INNOVATION-CONSULTING
      * Date: 2026-06-18
      * Purpose: Fully Fixed All Validations & Strict Loop Sequence.
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. Screen3.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT OPTIONAL CUST-TABLE 
           ASSIGN TO "Data/T_Application.csv"
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS FS-CUST.

       DATA DIVISION.
       FILE SECTION.
       FD  CUST-TABLE.
       01  CUST-RECORD            PIC X(350).

       WORKING-STORAGE SECTION.
       01  FS-CUST                PIC XX.
       01  WS-EOF-FLAG            PIC X     VALUE 'N'.

       01  WS-INPUT-FIELDS.
           05  WS-CUST-NAME       PIC X(20) VALUE SPACES.
           05  WS-EMAIL           PIC X(45) VALUE SPACES.
           05  WS-EMAIL-FINAL     PIC X(40) VALUE SPACES.
           05  WS-DOB-INPUT       PIC X(10) VALUE SPACES.
           05  WS-ADDRESS         PIC X(100) VALUE SPACES.
           05  WS-ADDRESS-FINAL   PIC X(40) VALUE SPACES.
           05  WS-POSTAL-INPUT    PIC X(15) VALUE SPACES.
           05  WS-POSTAL-FINAL    PIC X(7)  VALUE SPACES.

       01  WS-PHONE-BUFFER        PIC X(15) VALUE SPACES.
       01  WS-PHONE-FINAL         PIC X(11) VALUE SPACES.

       01  WS-SPLIT-DATE.
           05  VAL-YEAR           PIC 9(4).
           05  FILLER             PIC X.
           05  VAL-MONTH          PIC 99.
           05  FILLER             PIC X.
           05  VAL-DAY            PIC 99.

       01  WS-LEAP-QUOTIENT       PIC 9(4)  VALUE 0.
       01  WS-LEAP-CALC.
           05  WS-REM-4           PIC 999.
           05  WS-REM-100         PIC 999.
           05  WS-REM-400         PIC 999.
           05  WS-IS-LEAP         PIC 9     VALUE 0.

       01  WS-MAX-DAYS            PIC 99    VALUE 0.
       01  WS-CALC-AGE            PIC S9(4) VALUE 0.

       01  WS-HEADER-LINE.
           05  FILLER             PIC X(66) VALUE
         "NAME,PHONE,EMAIL,DOB,ADDRESS,POSTAL_CODE,DATE,TIME,STATUS,".
           05  FILLER             PIC X(70) VALUE
         "DEVICE_TYPE,DEVICE_MODEL,PURCHASE_PRICE,PURCHASE_DATE,".
           05  FILLER             PIC X(50) VALUE
         "COVERAGE_PERIOD,EST_PREMIUM,PLAN_NAME,IMEI".
           05  FILLER             PIC X(160) VALUE SPACES.

       01  WS-VALIDATION-FLAGS.
           05  WS-ERR-FLAG        PIC 9     VALUE 0.
           05  WS-ALERT-FLAG      PIC 9     VALUE 0.
           05  WS-ERR-MSG         PIC X(60) VALUE SPACES.
           05  WS-SPACE-COUNT     PIC 99    VALUE 0.
           05  WS-TEMP-COUNT      PIC 99    VALUE 0.
           05  WS-POSTAL-LEN      PIC 99    VALUE 0.
           05  WS-IDX             PIC 99    VALUE 1.
           05  WS-AT-POS          PIC 99    VALUE 0.
           05  WS-CHAR-BEFORE-AT  PIC 99    VALUE 0.

       01  WS-SYSTEM-METADATA.
           05  CURRENT-SYS-DATE.
               10  SYS-YEAR       PIC 9(4).
               10  SYS-MONTH      PIC 9(2).
               10  SYS-DAY        PIC 9(2).
           05  CURRENT-SYS-TIME.
               10  SYS-HOUR       PIC X(2).
               10  SYS-MIN        PIC X(2).
               10  SYS-SEC        PIC X(2).
               10  SYS-MS         PIC X(2).

       01  WS-FINAL-STATUS        PIC X(15) VALUE "PENDING".
       01  WS-RESPONSE            PIC X     VALUE SPACES.
       01  WS-STEP-TRACKER        PIC 9     VALUE 1.

       01  WS-CSV-PARSE-BUFFERS.
           05  CH-NAME            PIC X(50).
           05  CH-PHONE           PIC X(20).
           05  CH-EMAIL           PIC X(50).

       LINKAGE SECTION.
       01  LNK-CombineData.
           05  LNK-C-DeviceType     PIC X(10).
           05  LNK-C-DeviceModel    PIC X(25).
           05  LNK-C-PurchasePrice  PIC 9(8).
           05  LNK-C-PurchaseDate   PIC X(10).
           05  LNK-C-CoveragePeriod PIC 99.
           05  LNK-C-EstPremium     PIC 9(8).
           05  LNK-C-IMEI           PIC X(15).
           05  LNK-C-PlanName       PIC X(20).

       PROCEDURE DIVISION USING LNK-CombineData.
       MAIN-FLOW.
           PERFORM INITIALIZE-CSV-HEADER
           DISPLAY "== SYSTEM ONLINE. AWAITING ENTRY ==".
           MOVE 0 TO WS-ALERT-FLAG.
           MOVE 1 TO WS-STEP-TRACKER.

       DISPLAY-LOOP.
           DISPLAY "-------------------------------------------------".
           MOVE 0 TO WS-ERR-FLAG.
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
                   MOVE SPACES TO WS-DOB-INPUT
                   DISPLAY "Enter DOB (YYYY/MM/DD):"
                   ACCEPT WS-DOB-INPUT
                   PERFORM VALIDATE-DOB
               WHEN 5
                   MOVE SPACES TO WS-ADDRESS
                   DISPLAY "Enter Physical Address :"
                   ACCEPT WS-ADDRESS
                   PERFORM VALIDATE-ADDRESS
               WHEN 6
                   MOVE SPACES TO WS-POSTAL-INPUT
                   DISPLAY "Enter Postal Code (5 to 7 digits):"
                   ACCEPT WS-POSTAL-INPUT
                   PERFORM VALIDATE-POSTAL-CODE
           END-EVALUATE.

           IF WS-ERR-FLAG = 1
               DISPLAY WS-ERR-MSG
               GO TO DISPLAY-LOOP
           ELSE
               ADD 1 TO WS-STEP-TRACKER
               IF WS-STEP-TRACKER <= 6
                   GO TO DISPLAY-LOOP
               ELSE
                   PERFORM GENERATE-AND-DISPLAY-SUMMARY
                   PERFORM SAVE-DATA-RECORD
                   IF WS-ALERT-FLAG = 1
       DISPLAY "ALERT: Address is long! Only First 40 Chars Were Saved "
                   END-IF
                   DISPLAY WS-ERR-MSG

                   CALL 'Screen4' USING LNK-C-IMEI
               END-IF
           END-IF.

           DISPLAY "Press Enter to exit application...".
           ACCEPT WS-RESPONSE.
           STOP RUN.

      *----------------------------------------------------------------
      * INPUT VALIDATION PARAGRAPHS
      *----------------------------------------------------------------
       VALIDATE-NAME.

           IF WS-CUST-NAME = SPACES
               MOVE 1 TO WS-ERR-FLAG
               MOVE "ERROR: NAME FIELD CANNOT BE BLANK!" TO WS-ERR-MSG
               EXIT PARAGRAPH
           END-IF.

        IF WS-CUST-NAME(20:1) NOT = " " AND WS-CUST-NAME(19:1) NOT = " "
               MOVE 0 TO WS-SPACE-COUNT
               INSPECT WS-CUST-NAME TALLYING WS-SPACE-COUNT FOR ALL " "
               IF WS-SPACE-COUNT = 0
                   MOVE 1 TO WS-ERR-FLAG
                  MOVE "ERROR: NAME IS TOO LONG! MAXIMUM 20 CHARACTERS."
                   TO WS-ERR-MSG
                   EXIT PARAGRAPH
               END-IF
           END-IF.

       VALIDATE-PHONE.
           PERFORM VARYING WS-IDX FROM 1 BY 1 UNTIL WS-IDX > 15
               IF WS-PHONE-BUFFER(WS-IDX:1) NOT = SPACES AND
                  WS-PHONE-BUFFER(WS-IDX:1) > X"7F"
                   MOVE 1 TO WS-ERR-FLAG
                   MOVE "ERROR: INVALID FONT/CHARACTERS DETECTED!"
                   TO WS-ERR-MSG
                   EXIT PARAGRAPH
               END-IF
           END-PERFORM.

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
           PERFORM VARYING WS-IDX FROM 1 BY 1 UNTIL WS-IDX > 45
               IF WS-EMAIL(WS-IDX:1) NOT = SPACES AND
                  WS-EMAIL(WS-IDX:1) > X"7F"
                   MOVE 1 TO WS-ERR-FLAG
                   MOVE "ERROR: INVALID FONT/CHARACTERS DETECTED!"
                   TO WS-ERR-MSG
                   EXIT PARAGRAPH
               END-IF
           END-PERFORM.

           IF WS-EMAIL(41:5) NOT = SPACES
               MOVE 1 TO WS-ERR-FLAG
               MOVE "ERROR: INVALID EMAIL. EXCEEDS 40 CHARACTERS."
               TO WS-ERR-MSG
               EXIT PARAGRAPH
           END-IF.

           MOVE WS-EMAIL(1:40) TO WS-EMAIL-FINAL.

           MOVE 0 TO WS-TEMP-COUNT.
           INSPECT WS-EMAIL-FINAL TALLYING WS-TEMP-COUNT FOR ALL "@".
           IF WS-TEMP-COUNT NOT = 1
               MOVE 1 TO WS-ERR-FLAG
              MOVE "ERROR: EMAIL MUST HAVE EXACTLY ONE @." TO WS-ERR-MSG
               EXIT PARAGRAPH
           END-IF.

           MOVE 0 TO WS-AT-POS.
           PERFORM VARYING WS-IDX FROM 1 BY 1 UNTIL WS-IDX > 40
               IF WS-EMAIL-FINAL(WS-IDX:1) = "@"
                   MOVE WS-IDX TO WS-AT-POS
               END-IF
           END-PERFORM.

           COMPUTE WS-CHAR-BEFORE-AT = WS-AT-POS - 1.
           IF WS-CHAR-BEFORE-AT < 3
               MOVE 1 TO WS-ERR-FLAG
               MOVE "ERROR: NEED AT LEAST 3 CHARS IN FRONT OF @."
               TO WS-ERR-MSG
               EXIT PARAGRAPH
           END-IF.

           PERFORM VARYING WS-IDX FROM 1 BY 1 UNTIL WS-IDX >= WS-AT-POS
               IF NOT (WS-EMAIL-FINAL(WS-IDX:1) NUMERIC OR
                       WS-EMAIL-FINAL(WS-IDX:1) ALPHABETIC)
                   MOVE 1 TO WS-ERR-FLAG
                   MOVE "ERROR: BEFORE @ MUST BE ALPHANUMERIC."
                   TO WS-ERR-MSG
                   EXIT PARAGRAPH
               END-IF
           END-PERFORM.

      * --- VALIDATE DOMAIN NAME (BEFORE THE DOT) ---
           MOVE 0 TO WS-TEMP-COUNT.
           ADD 1 TO WS-AT-POS GIVING WS-IDX.

           PERFORM VARYING WS-IDX FROM WS-IDX BY 1 UNTIL WS-IDX > 40
               EVALUATE TRUE
                   WHEN WS-EMAIL-FINAL(WS-IDX:1) = "."
                       IF WS-TEMP-COUNT < 1
                           MOVE 1 TO WS-ERR-FLAG
                     MOVE "ERROR: NEED AT LEAST 1 CHARS BETWEEN @ AND ."
                           TO WS-ERR-MSG
                           EXIT PARAGRAPH
                       ELSE
                           EXIT PERFORM
                       END-IF
                   WHEN WS-EMAIL-FINAL(WS-IDX:1) = SPACES
                       MOVE 1 TO WS-ERR-FLAG
                       MOVE "ERROR: MISSING '.' IN EMAIL DOMAIN."
                       TO WS-ERR-MSG
                       EXIT PARAGRAPH
                   WHEN NOT (WS-EMAIL-FINAL(WS-IDX:1) ALPHABETIC)
                       MOVE 1 TO WS-ERR-FLAG
                       MOVE "ERROR: DOMAIN MUST BE ALPHABETIC ONLY."
                       TO WS-ERR-MSG
                       EXIT PARAGRAPH
                   WHEN OTHER
                       ADD 1 TO WS-TEMP-COUNT
               END-EVALUATE
           END-PERFORM.

      * --- VALIDATE TLD EXTENSION (AFTER THE DOT) ---
           ADD 1 TO WS-IDX.
           MOVE 0 TO WS-TEMP-COUNT.

           PERFORM VARYING WS-IDX FROM WS-IDX BY 1 UNTIL WS-IDX > 40
               IF WS-EMAIL-FINAL(WS-IDX:1) NOT = SPACES
                   IF NOT (WS-EMAIL-FINAL(WS-IDX:1) ALPHABETIC OR
                           WS-EMAIL-FINAL(WS-IDX:1) = ".")
                       MOVE 1 TO WS-ERR-FLAG
                       MOVE "ERROR: TLD MUST BE ALPHABETIC ONLY."
                       TO WS-ERR-MSG
                       EXIT PARAGRAPH
                   ELSE
                       ADD 1 TO WS-TEMP-COUNT
                   END-IF
               ELSE
                   EXIT PERFORM
               END-IF
           END-PERFORM.

           IF WS-TEMP-COUNT < 2
               MOVE 1 TO WS-ERR-FLAG
               MOVE "ERROR: INVALID OR MISSING TLD EXTENSION AFTER DOT."
               TO WS-ERR-MSG
               EXIT PARAGRAPH
           END-IF.

           PERFORM CHECK-DUPLICATE-EMAIL.

       VALIDATE-DOB.
           MOVE 0 TO WS-ERR-FLAG.
           MOVE FUNCTION CURRENT-DATE TO WS-SYSTEM-METADATA.

           MOVE FUNCTION TRIM(WS-DOB-INPUT) TO WS-DOB-INPUT.

           IF FUNCTION LENGTH(WS-DOB-INPUT) NOT = 10
               MOVE 1 TO WS-ERR-FLAG
               MOVE "ERROR: USE YYYY/MM/DD FORMAT (EG. 1998/02/25)"
               TO WS-ERR-MSG
               EXIT PARAGRAPH
           END-IF.

           IF WS-DOB-INPUT(5:1) NOT = "/" OR WS-DOB-INPUT(8:1) NOT = "/"
               MOVE 1 TO WS-ERR-FLAG
               MOVE "ERROR: USE YYYY/MM/DD FORMAT (EG. 1998/02/25)"
               TO WS-ERR-MSG
               EXIT PARAGRAPH
           END-IF.

           MOVE WS-DOB-INPUT TO WS-SPLIT-DATE.

           IF VAL-YEAR NOT NUMERIC OR VAL-MONTH NOT NUMERIC OR
              VAL-DAY NOT NUMERIC
               MOVE 1 TO WS-ERR-FLAG
               MOVE "ERROR: DATE MUST CONTAIN NUMBERS ONLY!"
               TO WS-ERR-MSG
               EXIT PARAGRAPH
           END-IF.

           IF VAL-MONTH < 1 OR VAL-MONTH > 12
               MOVE 1 TO WS-ERR-FLAG
               MOVE "ERROR: INVALID MONTH! MUST BE 01-12." TO WS-ERR-MSG
               EXIT PARAGRAPH
           END-IF.

           MOVE 0 TO WS-IS-LEAP.
           DIVIDE VAL-YEAR BY 4 GIVING WS-LEAP-QUOTIENT
               REMAINDER WS-REM-4.
           DIVIDE VAL-YEAR BY 100 GIVING WS-LEAP-QUOTIENT
               REMAINDER WS-REM-100.
           DIVIDE VAL-YEAR BY 400 GIVING WS-LEAP-QUOTIENT
               REMAINDER WS-REM-400.

           IF (WS-REM-4 = 0 AND WS-REM-100 NOT = 0) OR (WS-REM-400 = 0)
               MOVE 1 TO WS-IS-LEAP
           END-IF.

           EVALUATE VAL-MONTH
               WHEN 01 WHEN 03 WHEN 05 WHEN 07 WHEN 08 WHEN 10 WHEN 12
                   MOVE 31 TO WS-MAX-DAYS
               WHEN 04 WHEN 06 WHEN 09 WHEN 11
                   MOVE 30 TO WS-MAX-DAYS
               WHEN 02
                   IF WS-IS-LEAP = 1
                       MOVE 29 TO WS-MAX-DAYS
                   ELSE
                       MOVE 28 TO WS-MAX-DAYS
                   END-IF
           END-EVALUATE.

           IF VAL-DAY < 1 OR VAL-DAY > WS-MAX-DAYS
               MOVE 1 TO WS-ERR-FLAG
               MOVE "ERROR: INVALID DAY FOR THIS MONTH!" TO WS-ERR-MSG
               EXIT PARAGRAPH
           END-IF.

           COMPUTE WS-CALC-AGE = SYS-YEAR - VAL-YEAR.
           IF SYS-MONTH < VAL-MONTH OR
              (SYS-MONTH = VAL-MONTH AND SYS-DAY < VAL-DAY)
               SUBTRACT 1 FROM WS-CALC-AGE
           END-IF.

           IF WS-CALC-AGE < 11 OR WS-CALC-AGE > 100
               MOVE 1 TO WS-ERR-FLAG
               MOVE "ERROR: INVALID AGE RANGE (MUST BE 11-100)!"
               TO WS-ERR-MSG
               EXIT PARAGRAPH
           END-IF.

       VALIDATE-ADDRESS.
           IF WS-ADDRESS = SPACES
               MOVE 1 TO WS-ERR-FLAG
               MOVE "ERROR: ADDRESS CANNOT BE BLANK!" TO WS-ERR-MSG
               EXIT PARAGRAPH
           END-IF.

           IF WS-ADDRESS(41:60) NOT = SPACES
               MOVE 1 TO WS-ALERT-FLAG
               MOVE WS-ADDRESS(1:40) TO WS-ADDRESS-FINAL
           ELSE
               MOVE WS-ADDRESS(1:40) TO WS-ADDRESS-FINAL
           END-IF.

       VALIDATE-POSTAL-CODE.
           PERFORM VARYING WS-IDX FROM 1 BY 1 UNTIL WS-IDX > 15
               IF WS-POSTAL-INPUT(WS-IDX:1) NOT = SPACES AND
                  WS-POSTAL-INPUT(WS-IDX:1) > X"7F"
                   MOVE 1 TO WS-ERR-FLAG
                   MOVE "ERROR: INVALID FONT/CHARACTERS DETECTED!"
                   TO WS-ERR-MSG
                   EXIT PARAGRAPH
               END-IF
           END-PERFORM.

           IF WS-POSTAL-INPUT = SPACES
               MOVE 1 TO WS-ERR-FLAG
               MOVE "ERROR: POSTAL CODE IS REQUIRED!" TO WS-ERR-MSG
               EXIT PARAGRAPH
           END-IF.

           MOVE 0 TO WS-SPACE-COUNT.
           INSPECT FUNCTION REVERSE(WS-POSTAL-INPUT)
               TALLYING WS-SPACE-COUNT FOR LEADING SPACES.
           COMPUTE WS-POSTAL-LEN = 15 - WS-SPACE-COUNT.

           IF WS-POSTAL-LEN < 5 OR WS-POSTAL-LEN > 7
               MOVE 1 TO WS-ERR-FLAG
            MOVE "ERROR: INVALID CODE! POSTAL CODE MUST BE 5-7 DIGITS."
               TO WS-ERR-MSG
               EXIT PARAGRAPH
           END-IF.

           IF WS-POSTAL-INPUT(1:WS-POSTAL-LEN) NOT NUMERIC
               MOVE 1 TO WS-ERR-FLAG
               MOVE "ERROR: POSTAL CODE MUST BE NUMERIC ONLY!"
               TO WS-ERR-MSG
           ELSE
               MOVE WS-POSTAL-INPUT(1:WS-POSTAL-LEN) TO WS-POSTAL-FINAL
           END-IF.

      *----------------------------------------------------------------
      * CSV FILE OPERATIONS & REPEAT CHECKS
      *----------------------------------------------------------------
       CHECK-DUPLICATE-EMAIL.
           OPEN INPUT CUST-TABLE.
           IF FS-CUST NOT = "00"
               CLOSE CUST-TABLE
               EXIT PARAGRAPH
           END-IF.

           MOVE "N" TO WS-EOF-FLAG.

           PERFORM UNTIL WS-EOF-FLAG = "Y"
               READ CUST-TABLE
                   AT END
                       MOVE "Y" TO WS-EOF-FLAG
                   NOT AT END
                       UNSTRING CUST-RECORD DELIMITED BY ","
                       INTO CH-NAME, CH-PHONE, CH-EMAIL

                       IF FUNCTION UPPER-CASE(FUNCTION TRIM(CH-EMAIL)) =
                      FUNCTION UPPER-CASE(FUNCTION TRIM(WS-EMAIL-FINAL))
                           MOVE 1 TO WS-ERR-FLAG
                           MOVE "EMAIL ALREADY EXISTS! TRY ANOTHER"
                           TO WS-ERR-MSG
                           MOVE "Y" TO WS-EOF-FLAG
                       END-IF
               END-READ
           END-PERFORM.
           CLOSE CUST-TABLE.

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

       SAVE-DATA-RECORD.
           MOVE FUNCTION CURRENT-DATE TO WS-SYSTEM-METADATA.
           ACCEPT CURRENT-SYS-TIME FROM TIME.

           OPEN EXTEND CUST-TABLE.
           MOVE SPACES TO CUST-RECORD.
           STRING
               FUNCTION TRIM(WS-CUST-NAME)     DELIMITED BY SIZE ","
               FUNCTION TRIM(WS-PHONE-FINAL)   DELIMITED BY SIZE ","
               FUNCTION TRIM(WS-EMAIL-FINAL)   DELIMITED BY SIZE ","
               FUNCTION TRIM(WS-DOB-INPUT)     DELIMITED BY SIZE ","
               FUNCTION TRIM(WS-ADDRESS-FINAL) DELIMITED BY SIZE ","
               FUNCTION TRIM(WS-POSTAL-FINAL)  DELIMITED BY SIZE ","
               CURRENT-SYS-DATE                DELIMITED BY SIZE ","
               CURRENT-SYS-TIME(1:6)           DELIMITED BY SIZE ","
               FUNCTION TRIM(WS-FINAL-STATUS)  DELIMITED BY SIZE ","
      * Data passing from QuoScreen and PlanSelection
               FUNCTION TRIM(LNK-C-DeviceType) DELIMITED BY SIZE ","
               FUNCTION TRIM(LNK-C-DeviceModel) DELIMITED BY SIZE ","
               FUNCTION TRIM(LNK-C-PurchasePrice) DELIMITED BY SIZE ","
               FUNCTION TRIM(LNK-C-PurchaseDate)  DELIMITED BY SIZE ","
               FUNCTION TRIM(LNK-C-CoveragePeriod) DELIMITED BY SIZE ","
               FUNCTION TRIM(LNK-C-EstPremium) DELIMITED BY SIZE ","
               FUNCTION TRIM(LNK-C-PlanName) DELIMITED BY SIZE ","
               FUNCTION TRIM(LNK-C-IMEI) DELIMITED BY SIZE       
               INTO CUST-RECORD
           END-STRING.

           WRITE CUST-RECORD.
           CLOSE CUST-TABLE.
           MOVE "SUCCESS:Data saved successfully to CSV!" TO WS-ERR-MSG.

           END PROGRAM Screen3.
