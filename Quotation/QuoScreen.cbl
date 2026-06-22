       IDENTIFICATION DIVISION.
       PROGRAM-ID. QuoScreen.
       AUTHOR. AUNGKANPHYO.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT OPTIONAL AppFile ASSIGN TO "Screen3/T_Application.csv"
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS FS-APP.
       
           SELECT DeviceFile ASSIGN TO "Quotation/DeviceType.csv"
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS FS-Device.
             
       DATA DIVISION.
       FILE SECTION.
       FD  AppFile.
       01  App-Record-Buf PIC X(400).
       
       FD DeviceFile.
       01 Device-Record-Buf PIC X(50).
       
       WORKING-STORAGE SECTION.
       01 FS-APP PIC XX.
       01 FS-Device PIC XX.
       01 WS-Eof-Device PIC X VALUE 'N'.
       
      *Array to store filtered models from DeviceType.csv
       01 WS-Filtered-Table.
           05 WS-Total-Models PIC 99 VALUE 0.
           05 WS-Model-Item OCCURS 50 TIMES INDEXED BY model-index.
               10 WS-Select-Model PIC X(50).
       01 WS-Display-Num PIC Z9.
       01 WS-DeviceType PIC X(10).
       01 WS-DeviceModel PIC X(25).
       01 WS-ModelChoice PIC 9 VALUE 0.
       01 WS-PurchasePrice PIC 9(8).
       01 WS-PurchaseDate PIC X(10).
      *To check the date by breaking it down into pieces
       01 WS-DateFields REDEFINES WS-PurchaseDate.
           05 WS-Date-Year   PIC 9(4).
           05 WS-Date-Sep1   PIC X.
           05 WS-Date-Month  PIC 9(2).
           05 WS-Date-Sep2   PIC X.
           05 WS-Date-Day    PIC 9(2).
       
       01 WS-DeviceIMEI PIC X(15) VALUE SPACES.
      *for IMEI 15 number check
       01 WS-IMEI-Input-Buf  PIC X(30) VALUE SPACES.
       01  WS-IMEI-Valid-Flag PIC X VALUE 'N'.
       01  WS-EOF-APP PIC X VALUE 'N'.
       01  WS-Dup-Flag PIC X VALUE 'N'.
      *Flag that control the entire process to restart from the beginning
       01 WS-Main-Loop-Flag PIC X VALUE 'N'.
       
      *To extract data from a CSV file and filter only the IMEI
       01  WS-CSV-FIELDS.
           05  F-NAME           PIC X(50).
           05  F-PHONE          PIC X(20).
           05  F-EMAIL          PIC X(50).
           05  F-DOB            PIC X(20).
           05  F-ADDRESS        PIC X(100).
           05  F-POSTAL-CODE    PIC X(15).
           05  F-DATE           PIC X(20).
           05  F-TIME           PIC X(20).
           05  F-STATUS         PIC X(20).
           05  F-DEVTYPE        PIC X(20).
           05  F-DEVMODEL       PIC X(50).
           05  F-PRICE          PIC X(20).
           05  F-PURDATE        PIC X(20).
           05  F-PERIOD         PIC X(20).
           05  F-PREMIUM        PIC X(20).
           05  F-PLANCODE       PIC X(20).
           05  F-IMEI           PIC X(15).
       
      * variables for date validation
       01 WS-DateValidation.
           05 WS-Date-Valid-Flag PIC X VALUE 'N'.
           05 WS-Max-Days        PIC 9(2) VALUE 0.
           05 WS-Temp-Calc       PIC 9(4) VALUE 0.
           05 WS-Rem-4           PIC 9(4) VALUE 0.
           05 WS-Rem-100         PIC 9(4) VALUE 0.
           05 WS-Rem-400         PIC 9(4) VALUE 0.

       
       01 WS-Current-Date-Data.
           05 WS-Current-Year    PIC 9(4).
           05 WS-Current-Month   PIC 9(2).
           05 WS-Current-Day     PIC 9(2).
           05 FILLER             PIC X(13).
       
       01 WS-Purchase.
           05 WS-CategoryChoice PIC 9 VALUE 0.
           05 WS-MinPrice PIC 9(8) VALUE 0.
           05 WS-MaxPrice PIC 9(8) VALUE 0.
           05 WS-ValidFlag PIC X VALUE 'N'.
           05 WS-Disp-MinPrice PIC ZZZ,ZZZ,ZZ9.
           05 WS-Disp-MaxPrice PIC ZZZ,ZZZ,ZZ9. 
       
      *Coverage Period Multiplier Constants
       01 WS-Multiplier-Constants.
           05 C-Mult-12 PIC 9V9 VALUE 1.0.
           05 C-MULT-24         PIC 9V9 VALUE 1.8.
           05 C-MULT-36         PIC 9V9 VALUE 2.5.
       01 WS-Premium.
           05 WS-CoveragePeriod PIC 99 VALUE 0.
           05 WS-InternalCode   PIC X(3) VALUE SPACES.
           05 WS-BaseRate       PIC 9V99 VALUE 0.02.
           05 WS-Multiplier     PIC 9V9 VALUE 0.0.
           05 WS-EstPremium     PIC 9(8) VALUE 0.
           05 WS-DisplayPremium PIC ZZZ,ZZZ,ZZ9.
       
      *    Grouping the data to be passed into a Group Item
           01 WS-QuoData.
               05 QD-DeviceType PIC X(10).
               05 QD-DeviceModel PIC X(25).
               05 QD-PurchasePrice PIC 9(8).
               05 QD-PurchaseDate PIC X(10).
               05 QD-CoveragePeriod PIC 99 VALUE 0.
               05 QD-EstPremium PIC 9(8) VALUE 0.
               05 QD-IMEI PIC X(15).
       
       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
           DISPLAY "================================================".
           DISPLAY "        QUOTATION SCREEN - INPUT ITEMS          ".
           DISPLAY "================================================".
           
      * Outer Loop for the entire process   
           MOVE 'N' TO WS-Main-Loop-Flag
           PERFORM UNTIL WS-Main-Loop-Flag = 'Y'
       
      *    Device Type Input & Validation
           MOVE SPACES TO WS-DeviceType
           PERFORM UNTIL WS-DeviceType = "iPhone" OR
           WS-DeviceType = "Andriod"
               DISPLAY "Enter Device Type (iPhone / Andriod): "
               WITH NO ADVANCING
               ACCEPT WS-DeviceType
               IF WS-DeviceType NOT = "iPhone" AND
               WS-DeviceType NOT = "Andriod"
               DISPLAY "Error: Please enter either 'iPhone' "
                       "or 'Android'."
               END-IF
           END-PERFORM
       
      *    Device Model Input & Validation
           MOVE 0 TO WS-Total-Models
           MOVE 'N' TO WS-Eof-Device
           OPEN INPUT DeviceFile
       
           IF FS-Device = "00"
            PERFORM UNTIL WS-Eof-Device = 'Y' OR WS-Total-Models >= 50
             READ DeviceFile
              AT END MOVE 'Y' TO WS-Eof-Device
               NOT AT END
      * if user choose iPhone show only iPhone
                IF WS-DeviceType = "iPhone" AND 
                Device-Record-Buf(1:6) = "iPhone"
                 ADD 1 TO WS-Total-Models
                  MOVE Device-Record-Buf TO 
                  WS-Select-Model(WS-Total-Models)
       
                ELSE IF WS-DeviceType = "Andriod" AND
                       Device-Record-Buf(1:6) NOT = "iPhone"
                       ADD 1 TO WS-Total-Models
                       MOVE Device-Record-Buf TO 
                       WS-Select-Model(WS-Total-Models)
                END-IF
           END-READ
           END-PERFORM
           ELSE
            DISPLAY "Error: Cannot open DeviceType.csv file! Code: " 
                    FS-Device
           END-IF
           CLOSE DeviceFile
       
           DISPLAY "------------------------------------------------"
           DISPLAY "Please choose a Device Model:"
           PERFORM VARYING model-index FROM 1 BY 1 UNTIL
               model-index > WS-Total-Models
               MOVE model-index TO WS-Display-Num
               DISPLAY FUNCTION TRIM(WS-Display-Num)". "
                       FUNCTION TRIM(WS-Select-Model(model-index))
           END-PERFORM
           DISPLAY "------------------------------------------------"
       
      *   Accept choice from user and check whether valid or not
           MOVE 0 TO WS-ModelChoice
           PERFORM UNTIL WS-ModelChoice >=1 AND 
                         WS-ModelChoice <= WS-Total-Models
               DISPLAY "Enter choice (1-" FUNCTION TRIM(WS-Total-Models)
                       "): " WITH NO ADVANCING
               ACCEPT WS-ModelChoice
       
               IF WS-ModelChoice < 1 OR WS-ModelChoice > WS-Total-Models
                  DISPLAY "Error: Invalid choice! Please enter 1 to 6."
               END-IF
           END-PERFORM
       
      *  Inserting the selected model into the main variable
           MOVE WS-Select-Model(WS-ModelChoice) TO WS-DeviceModel  
       
      *    Device IMEI Input & Duplicate Validation Flow
           MOVE 'N' TO WS-IMEI-Valid-Flag
           PERFORM UNTIL WS-IMEI-Valid-Flag = 'Y'
               MOVE SPACES TO WS-DeviceIMEI
               MOVE SPACES TO WS-IMEI-Input-Buf
               DISPLAY "Enter Device IMEI (15 digits): " 
               WITH NO ADVANCING
               ACCEPT WS-IMEI-Input-Buf
       
               IF WS-IMEI-Input-Buf(1:15) NOT NUMERIC OR 
                  WS-IMEI-Input-Buf(16:15) NOT = SPACES
                   DISPLAY "Error: IMEI must be EXACTLY "
                           "a 15-digit number!"
               ELSE
                   MOVE WS-IMEI-Input-Buf(1:15) TO WS-DeviceIMEI
                   MOVE 'N' TO WS-Dup-Flag
                   MOVE 'N' TO WS-EOF-APP
                   OPEN INPUT AppFile
       
                   IF FS-APP = "00"
                    PERFORM UNTIL WS-EOF-APP = 'Y' OR WS-Dup-Flag = 'Y'
                     READ AppFile
                      AT END MOVE 'Y' TO WS-EOF-APP
                       NOT AT END
                        INITIALIZE WS-CSV-FIELDS
                         UNSTRING App-Record-Buf DELIMITED BY ","
                          INTO F-NAME F-PHONE F-EMAIL F-DOB F-ADDRESS 
                           F-POSTAL-CODE F-DATE F-TIME F-STATUS 
                           F-DEVTYPE F-DEVMODEL F-PRICE F-PURDATE 
                           F-PERIOD F-PREMIUM F-PLANCODE F-IMEI
                           END-UNSTRING
                                   
                           IF FUNCTION TRIM(F-IMEI) = 
                           FUNCTION TRIM(WS-DeviceIMEI)
                               MOVE 'Y' TO WS-Dup-Flag
                           END-IF
                           END-READ
                       END-PERFORM
                   END-IF
                   CLOSE AppFile
                 IF WS-Dup-Flag = 'Y'
                       DISPLAY "Error: Duplicate Entry! "
                               "This IMEI already exists."
                       DISPLAY "Please restart the process."
                       DISPLAY "---------------------------------------"
      *  if duplicate exit imei loop and return the begin outer loop
                       MOVE 'Y' TO WS-IMEI-Valid-Flag
                   ELSE
                       MOVE 'Y' TO WS-IMEI-Valid-Flag
                   END-IF
               END-IF
           END-PERFORM
       
      * No duplidate, continue remaining steps
           IF WS-Dup-Flag = 'N'   
       
      *    Purchase Price Input & Validation
           DISPLAY "Allowed Price Range: 10,000 JPY to 200,000 JPY"
           DISPLAY "Please choose a Price Category:"
           DISPLAY "1. LOW    (10,000 ~ 50,000 JPY)"
           DISPLAY "2. MEDIUM (50,001 ~ 100,000 JPY)"
           DISPLAY "3. HIGH   (100,001 ~ 200,000 JPY)"
       
           MOVE 0 TO WS-CategoryChoice
           PERFORM UNTIL WS-CategoryChoice = 1 OR
                         WS-CategoryChoice = 2 OR
                         WS-CategoryChoice = 3
              DISPLAY "Enter choice (1, 2, or 3): " WITH NO ADVANCING
              ACCEPT WS-CategoryChoice
              IF WS-CategoryChoice NOT = 1 AND
                 WS-CategoryChoice NOT = 2 AND
                 WS-CategoryChoice NOT = 3
                 DISPLAY "Error: Invalid choice! Please enter 1, 2, "
                         "or 3."
              END-IF
           END-PERFORM
       
      *    Setting Min/Max Range based on selection
           EVALUATE WS-CategoryChoice
               WHEN 1
                   MOVE 10000 TO WS-MinPrice
                   MOVE 50000 TO WS-MaxPrice
               WHEN 2
                   MOVE 50001 TO WS-MinPrice
                   MOVE 100000 TO WS-MaxPrice
               WHEN 3
                   MOVE 100001 TO WS-MinPrice
                   MOVE 200000 TO WS-MaxPrice
           END-EVALUATE
       
           MOVE 'N' TO WS-ValidFlag
           PERFORM UNTIL WS-ValidFlag = 'Y'
              MOVE WS-MinPrice TO WS-Disp-MinPrice
              MOVE WS-MaxPrice TO WS-Disp-MaxPrice
              DISPLAY "Enter Price (" FUNCTION TRIM(WS-Disp-MinPrice) 
                      "-" FUNCTION TRIM(WS-Disp-MaxPrice) "):"
              WITH NO ADVANCING
              ACCEPT WS-PurchasePrice
       
              IF WS-PurchasePrice >= WS-MinPrice AND 
                 WS-PurchasePrice <= WS-MaxPrice
                 MOVE 'Y' TO WS-ValidFlag
              ELSE 
                 DISPLAY "Error: Price is out of the selected range!"
              END-IF
           END-PERFORM
       
      *    Purchase Date Input & Validation
           MOVE 'N' TO WS-Date-Valid-Flag
           PERFORM UNTIL WS-Date-Valid-Flag = 'Y'
               DISPLAY "Enter Purchase Date (YYYY/MM/DD): "
               WITH NO ADVANCING
               ACCEPT WS-PurchaseDate
       
      *        Format and Numeric Check (Example: 2026/04/01)
               IF WS-Date-Sep1 NOT = '/' OR WS-Date-Sep2 NOT = '/' OR
                   WS-Date-Year NOT NUMERIC OR
                   WS-Date-Month NOT NUMERIC OR
                   WS-Date-Day NOT NUMERIC
                   DISPLAY "Error: Invalid format! Format must be "
                           "YYYY/MM/DD."
                   MOVE 'N' TO WS-Date-Valid-Flag
               ELSE
      *            Month check
                   IF WS-Date-Month < 1 OR WS-Date-Month > 12
                       DISPLAY "Error: Invalid Month! Must be 01 to 12."
                       MOVE 'N' TO WS-Date-Valid-Flag
                   ELSE
      *       Calculating the date for the selected month
                       EVALUATE WS-Date-Month
                          WHEN 01 WHEN 03 WHEN 05 WHEN 07 WHEN 08 
                          WHEN 10 WHEN 12
                           MOVE 31 TO WS-Max-Days
                          WHEN 04 WHEN 06 WHEN 09 WHEN 11
                           MOVE 30 TO WS-Max-Days
                          WHEN 02
      *           Calculating whether it is a Leap Year or not
                           DIVIDE WS-Date-Year BY 4 
                               GIVING WS-Temp-Calc REMAINDER WS-Rem-4
                           DIVIDE WS-Date-Year BY 100 
                               GIVING WS-Temp-Calc REMAINDER WS-Rem-100
                           DIVIDE WS-Date-Year BY 400 
                               GIVING WS-Temp-Calc REMAINDER WS-Rem-400
                           IF (WS-Rem-4 = 0 AND WS-Rem-100 NOT = 0)
                               OR (WS-Rem-400 = 0)   
                               MOVE 29 TO WS-Max-Days
                           ELSE
                               MOVE 28 TO WS-Max-Days
                           END-IF
                       END-EVALUATE
       
                       IF WS-Date-Day < 1 OR WS-Date-Day > WS-Max-Days
                          DISPLAY "Error: Invalid Day for this month!"
                          DISPLAY "Maximum allowed days for this month:"
                                   WS-Max-Days
                          MOVE 'N' TO WS-Date-Valid-Flag
                       ELSE
      *                Future date checking logic
                          MOVE FUNCTION CURRENT-DATE TO 
                          WS-Current-Date-Data
                          
                          IF WS-Date-Year > WS-Current-Year OR
                             (WS-Date-Year = WS-Current-Year AND 
                              WS-Date-Month > WS-Current-Month) OR
                             (WS-Date-Year = WS-Current-Year AND 
                              WS-Date-Month = WS-Current-Month AND 
                              WS-Date-Day > WS-Current-Day)
                              
                              DISPLAY "Error: Future date "
                                      "is not allowed!"
                              MOVE 'N' TO WS-Date-Valid-Flag
                          ELSE
                              MOVE 'Y' TO WS-Date-Valid-Flag   
                       END-IF
                   END-IF
               END-IF
           END-PERFORM
       
      *    Coverage Period Input & Validation
           DISPLAY "------------------------------------------------"
           DISPLAY "Please choose a Coverage Period:"
           DISPLAY "- 12 months"
           DISPLAY "- 24 months"
           DISPLAY "- 36 months"
           DISPLAY "------------------------------------------------"
       
           MOVE 0 TO WS-CoveragePeriod
           PERFORM UNTIL WS-CoveragePeriod = 12 OR
                         WS-CoveragePeriod = 24 OR
                         WS-CoveragePeriod = 36
               DISPLAY "Enter Coverage Period (12 / 24 / 36): "
               WITH NO ADVANCING
               ACCEPT WS-CoveragePeriod
               
               IF WS-CoveragePeriod NOT = 12 AND
                  WS-CoveragePeriod NOT = 24 AND
                  WS-CoveragePeriod NOT = 36
                  DISPLAY "Error: Please select 12, 24, or 36 months."
               END-IF
           END-PERFORM
       
           EVALUATE WS-CoveragePeriod
               WHEN 12
                   MOVE C-Mult-12 TO WS-Multiplier
                   MOVE "P12" TO WS-InternalCode
               WHEN 24
                   MOVE C-MULT-24 TO WS-Multiplier
                   MOVE "P24" TO WS-InternalCode
               WHEN 36
                   MOVE C-MULT-36 TO WS-Multiplier
                   MOVE "P36" TO WS-InternalCode
           END-EVALUATE
       
      *    Calculate Estimated Premium
           COMPUTE WS-EstPremium ROUNDED = 
               WS-PurchasePrice * WS-BaseRate * WS-Multiplier
      *    display estimated premium in Screen 1
           MOVE WS-EstPremium TO WS-DisplayPremium
           DISPLAY "------------------------------------------------"
           DISPLAY "Estimate Premium: " WS-DisplayPremium
           DISPLAY "------------------------------------------------"
       
           MOVE WS-DeviceType TO QD-DeviceType
           MOVE WS-DeviceModel TO QD-DeviceModel
           MOVE WS-PurchasePrice TO QD-PurchasePrice
           MOVE WS-PurchaseDate TO QD-PurchaseDate
           MOVE WS-CoveragePeriod TO QD-CoveragePeriod
           MOVE WS-EstPremium TO QD-EstPremium
           MOVE WS-DeviceIMEI TO QD-IMEI
       
      *close main loop after all info entered correctly
           MOVE 'Y' TO WS-Main-Loop-Flag    
      *    Sending data to Screen 2 and CALL
           CALL 'PLAN-SELECTION-SYSTEM' USING WS-QuoData
           END-IF
           END-PERFORM.
       
           STOP RUN.

