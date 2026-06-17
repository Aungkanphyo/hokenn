       IDENTIFICATION DIVISION.
       PROGRAM-ID. QuoScreen.
       AUTHOR. AUNGKANPHYO.
      *
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
      *
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-DeviceType PIC X(10).
       01 WS-DeviceModel PIC X(25).
       01 WS-ModelChoice PIC 9 VALUE 0.
       01 WS-PurchasePrice PIC 9(8).
       01 WS-PurchaseDate PIC X(10).

       01 WS-Purchase.
           05 WS-CategoryChoice PIC 9 VALUE 0.
           05 WS-MinPrice PIC 9(8) VALUE 0.
           05 WS-MaxPrice PIC 9(8) VALUE 0.
           05 WS-ValidFlag PIC X VALUE 'N'.

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
               05 QD-DeviceType     PIC X(10).
               05 QD-DeviceModel    PIC X(25).
               05 QD-PurchasePrice  PIC 9(8).
               05 QD-PurchaseDate   PIC X(10).
               05 QD-CoveragePeriod PIC 99 VALUE 0.
               05 QD-EstPremium     PIC 9(8) VALUE 0. 

      *
       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
           DISPLAY "================================================".
           DISPLAY "        QUOTATION SCREEN - INPUT ITEMS          ".
           DISPLAY "================================================".
           
      *    Device Type Input & Validation
           MOVE SPACES TO WS-DeviceType.
           PERFORM UNTIL WS-DeviceType = "iPhone" OR
           WS-DeviceType = "Andriod"
               DISPLAY "Enter Device Type (iPhone / Andriod): "
               WITH NO ADVANCING
               ACCEPT WS-DeviceType
               IF WS-DeviceType NOT = "iPhone" AND
               WS-DeviceType NOT = "Andriod"
               DISPLAY "Error: Please enter either 'iPhone'
      -        "or 'Android'."
               END-IF
           END-PERFORM.

      *    Device Model Input & Validation
           DISPLAY "------------------------------------------------".
           DISPLAY "Please choose a Device Model:".
           DISPLAY "1. iPhone 14 pro max".
           DISPLAY "2. iPhone 15 pro".
           DISPLAY "3. iPhone 17".
           DISPLAY "4. Samsaung Galaxy A17".
           DISPLAY "5. Samsaung Galaxy S24".
           DISPLAY "6. Xiaomi Note 8".
           DISPLAY "------------------------------------------------".

           MOVE SPACES TO WS-ModelChoice.
           PERFORM UNTIL WS-ModelChoice >=1 AND WS-ModelChoice <= 6
               DISPLAY "Enter choice (1-6): " WITH NO ADVANCING
               ACCEPT WS-ModelChoice

               IF WS-ModelChoice < 1 OR WS-ModelChoice > 6
                  DISPLAY "Error: Invalid choice! Please enter 1 to 6."
               END-IF
           END-PERFORM.

           EVALUATE WS-ModelChoice
               WHEN 1  
                   MOVE "iPhone 14 pro max" TO WS-DeviceModel
               WHEN 2
                   MOVE "iPhone 15 pro" TO WS-DeviceModel
               WHEN 3
                   MOVE "iPhone 17" TO WS-DeviceModel
               WHEN 4
                   MOVE "Samsaung Galaxy A17" TO WS-DeviceModel
               WHEN 5
                   MOVE "Samsaung Galaxy S24" TO WS-DeviceModel
               WHEN 6
                   MOVE "Xiaomi Note 8"       TO WS-DeviceModel
           END-EVALUATE.

      *    Purchase Price Input & Validation
           DISPLAY "Allowed Price Range: 10,000 JPY to 200,000 JPY".
           DISPLAY "Please choose a Price Category:"
           DISPLAY "1. LOW    (10,000 ~ 50,000 JPY)".
           DISPLAY "2. MEDIUM (50,001 ~ 100,000 JPY)".
           DISPLAY "3. HIGH   (100,001 ~ 200,000 JPY)".

           MOVE 0 TO WS-CategoryChoice.
           PERFORM UNTIL WS-CategoryChoice = 1 OR
                         WS-CategoryChoice = 2 OR
                         WS-CategoryChoice = 3
              DISPLAY "Enter choice (1, 2, or 3): " WITH NO ADVANCING
              ACCEPT WS-CategoryChoice
              IF WS-CategoryChoice NOT = 1 AND
                 WS-CategoryChoice NOT = 2 AND
                 WS-CategoryChoice NOT = 3
                 DISPLAY "Error: Invalid choice! Please enter 1, 2,"
      -           "or 3."
              END-IF
           END-PERFORM.

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
           END-EVALUATE.

           MOVE 'N' TO WS-ValidFlag
           PERFORM UNTIL WS-ValidFlag = 'Y'
              DISPLAY "Enter Price (" WS-MinPrice "-" WS-MaxPrice "):"
              WITH NO ADVANCING
              ACCEPT WS-PurchasePrice

              IF WS-PurchasePrice >= WS-MinPrice AND 
                 WS-PurchasePrice <= WS-MaxPrice
                 MOVE 'Y' TO WS-ValidFlag
              ELSE 
                 DISPLAY "Error: Price is out of the selected range!"
              END-IF
           END-PERFORM.

      *    Purchase Date Input & Validation
           MOVE SPACES TO WS-PurchaseDate.
           PERFORM UNTIL WS-PurchaseDate NOT = SPACES
               DISPLAY "Enter Purchase Date (2026/04/01): " 
               WITH NO ADVANCING
               ACCEPT WS-PurchaseDate
               IF WS-PurchaseDate = SPACES
                   DISPLAY "Error: Purchase Date is required!"
               END-IF
           END-PERFORM.

      *    Coverage Period Input & Validation
           DISPLAY "------------------------------------------------".
           DISPLAY "Please choose a Coverage Period:".
           DISPLAY "- 12 months".
           DISPLAY "- 24 months".
           DISPLAY "- 36 months".
           DISPLAY "------------------------------------------------".

           MOVE 0 TO WS-CoveragePeriod.
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
           END-PERFORM.

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
           END-EVALUATE.

      *    Calculate Estimated Premium
           COMPUTE WS-EstPremium ROUNDED = 
               WS-PurchasePrice * WS-BaseRate * WS-Multiplier.

           MOVE WS-DeviceType TO QD-DeviceType.
           MOVE WS-DeviceModel TO QD-DeviceModel.
           MOVE WS-PurchasePrice TO QD-PurchasePrice.
           MOVE WS-PurchaseDate TO QD-PurchaseDate.
           MOVE WS-CoveragePeriod TO QD-CoveragePeriod.
           MOVE WS-EstPremium TO QD-EstPremium.
      *    Sending data to Screen 2 and CALL
           CALL 'PLAN-SELECTION-SYSTEM' USING WS-QuoData.

           STOP RUN.

