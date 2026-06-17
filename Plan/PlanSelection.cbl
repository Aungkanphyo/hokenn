       IDENTIFICATION DIVISION.
       PROGRAM-ID. PLAN-SELECTION-SYSTEM.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT MasterFile ASSIGN TO "Plan/M_Plan.csv"
               ORGANIZATION IS LINE SEQUENTIAL.
           SELECT CoverageFile ASSIGN TO "Plan/M_Plan_Coverage.csv"
               ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.
       FD MasterFile.
       01 M_Plan_Record.
           05 Plan_code    PIC X(5).
           05 F1           PIC X.
           05 Plan_Name    PIC X(20).
           05 F2           PIC X.
           05 Base_Rate    PIC 9(5).
           05 F3           PIC X.
           05 Max_Payout   PIC 9(8).
           05 F4           PIC X.
           05 Active_Flag  PIC X.

       FD CoverageFile.
       01 Coverage_Raw     PIC X(100).

       WORKING-STORAGE SECTION.
       01 User-Choice      PIC X(5).
       01 Found-Flag       PIC X VALUE 'N'.
       01 EOF-FLAG         PIC X VALUE 'N'.
      * UNSTRING variables
       01 WS-Plan-cd       PIC X(5).
       01 WS-Cov-Type      PIC X(20).
       01 WS-Enable-Flag   PIC X.

      * Structure to be merged to continue to Screen 3
       01 WS-CombineData.
           05 WS-C-DeviceType     PIC X(10).
           05 WS-C-DeviceModel    PIC X(25).
           05 WS-C-PurchasePrice  PIC 9(8).
           05 WS-C-PurchaseDate   PIC X(10).
           05 WS-C-CoveragePeriod PIC 99.
           05 WS-C-EstPremium     PIC 9(8).
           05 WS-C-PlanCode       PIC X(5).

      * data sent from Screen 1 will be received
       LINKAGE SECTION.
       01 LNK-QuoData.
           05 LNK-DeviceType     PIC X(10).
           05 LNK-DeviceModel    PIC X(25).
           05 LNK-PurchasePrice  PIC 9(8).
           05 LNK-PurchaseDate   PIC X(10).
           05 LNK-CoveragePeriod PIC 99.
           05 LNK-EstPremium     PIC 9(8).

       PROCEDURE DIVISION USING LNK-QuoData.
       MAIN-PROCEDURE.
      * AVAILABLE PLANS
           DISPLAY "--- Available Insurance Plans ---"
           OPEN INPUT MasterFile
           MOVE 'N' TO EOF-FLAG
           PERFORM UNTIL EOF-FLAG = 'Y'
               READ MasterFile
                   AT END MOVE 'Y' TO EOF-FLAG
                   NOT AT END
                    IF Active_Flag = 'Y'
                       DISPLAY "Plan Code: " Plan_code " | Name: "
                       Plan_Name
                    END-IF
               END-READ
           END-PERFORM
           CLOSE MasterFile

           DISPLAY "Enter Plan Code to view Coverage/Compare: "
           ACCEPT User-Choice.
           Move Function Upper-Case (User-Choice) to User-Choice
      * VALIDATION
           OPEN INPUT MasterFile
           MOVE 'N' TO Found-Flag
           MOVE 'N' TO EOF-FLAG
           PERFORM UNTIL EOF-FLAG = 'Y'
               READ MasterFile
                   AT END MOVE 'Y' TO EOF-FLAG
                   NOT AT END
                       IF Plan_code = User-Choice
                           AND Active_Flag = 'Y'
                           MOVE 'Y' TO Found-Flag
                       END-IF
               END-READ
           END-PERFORM
           CLOSE MasterFile

           IF Found-Flag = 'N'
               DISPLAY "Error: Invalid Plan Code or Plan is Inactive."
               STOP RUN
           END-IF.

      * SHOW COVERAGE
           DISPLAY "--- Coverage Comparison for " User-Choice " ---"
           OPEN INPUT CoverageFile
           MOVE 'N' TO EOF-FLAG
           PERFORM UNTIL EOF-FLAG = 'Y'
               READ CoverageFile
                   AT END MOVE 'Y' TO EOF-FLAG
                   NOT AT END
                       UNSTRING Coverage_Raw DELIMITED BY ","
                           INTO WS-Plan-cd WS-Cov-Type WS-Enable-Flag
                       END-UNSTRING

                       IF FUNCTION TRIM(WS-Plan-cd) = FUNCTION
                           TRIM(User-Choice)
                          AND WS-Enable-Flag = 'Y'
                           DISPLAY "- " FUNCTION TRIM(WS-Cov-Type) '  '
                           WS-Enable-Flag
                       END-IF
               END-READ
           END-PERFORM
           CLOSE CoverageFile.

           DISPLAY "Plan Selection Process Completed."

      *    Gather all the data and prepare to send it to Screen 3
           MOVE LNK-DeviceType TO WS-C-DeviceType.
           MOVE LNK-DeviceModel TO WS-C-DeviceModel.
           MOVE LNK-PurchasePrice TO WS-C-PurchasePrice.
           MOVE LNK-PurchaseDate TO WS-C-PurchaseDate.
           MOVE LNK-CoveragePeriod TO WS-C-CoveragePeriod.
           MOVE LNK-EstPremium TO WS-C-EstPremium.
           MOVE User-Choice TO WS-C-PlanCode.

           CALL 'Screen3' USING WS-CombineData.
           STOP RUN.
       END PROGRAM PLAN-SELECTION-SYSTEM.
