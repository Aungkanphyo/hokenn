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
       01 User-Choice      PIC 9(2).
       01 Selected-Plan-Cd PIC X(5).
       01 Target-Plan-Name PIC X(20).
       01 Target-Base-Rate PIC 9(5).
       01 Target-Max-Pay   PIC 9(8).

       *> Counter Variables for Dynamic Menu
       01 Plan-Counter     PIC 9 VALUE 0.
       01 Display-Num      PIC Z9.

       *> Control Flags
       01 Found-Flag       PIC X VALUE 'N'.
       01 EOF-FLAG         PIC X VALUE 'N'.
       01 Loop-Flag        PIC X VALUE 'Y'.
       01 Valid-Loop-Input PIC X VALUE 'N'.

       *> Editing Variables for Numbers
       01 Formatted-Base   PIC ZZ,ZZ9.
       01 Formatted-Max    PIC ZZ,ZZZ,ZZ9.

       *> UNSTRING variables
       01 WS-Plan-cd       PIC X(5).
       01 WS-Cov-Type      PIC X(20).
       01 WS-Enable-Flag   PIC X.

      ** Structure to be merged to continue to Screen 3
       01 WS-CombineData.
           05 WS-C-DeviceType     PIC X(10).
           05 WS-C-DeviceModel    PIC X(25).
           05 WS-C-PurchasePrice  PIC 9(8).
           05 WS-C-PurchaseDate   PIC X(10).
           05 WS-C-CoveragePeriod PIC 99.
           05 WS-C-EstPremium     PIC 9(8).
           05 WS-C-IMEI PIC X(15).
           05 WS-C-PlanName       PIC X(20).

       LINKAGE SECTION.
       01 LNK-QuoData.
           05 LNK-DeviceType     PIC X(10).
           05 LNK-DeviceModel    PIC X(15).
           05 LNK-PurchasePrice  PIC 9(8).
           05 LNK-PurchaseDate   PIC X(10).
           05 LNK-CoveragePeriod PIC 99.
           05 LNK-EstPremium     PIC 9(8).
           05 LNK-IMEI PIC X(15). 

       PROCEDURE DIVISION.
       MAIN-PROCEDURE.

      *>  MAIN LOOP
           PERFORM UNTIL Loop-Flag = 'N' OR 'n'

               DISPLAY " "
               DISPLAY "--- Available Insurance Plans ---"

      *>        Fetch and Display Dynamic Menu
               OPEN INPUT MasterFile
               MOVE 'N' TO EOF-FLAG
               MOVE 0 TO Plan-Counter
               PERFORM UNTIL EOF-FLAG = 'Y'
                   READ MasterFile
                       AT END MOVE 'Y' TO EOF-FLAG
                       NOT AT END
                           IF Active_Flag = 'Y'
                               ADD 1 TO Plan-Counter
                               MOVE Plan-Counter TO Display-Num
                               DISPLAY Display-Num ". "
                                       FUNCTION TRIM(Plan_Name)
                                       " (" Plan_code ")"
                           END-IF
                   END-READ
               END-PERFORM
               CLOSE MasterFile
               DISPLAY "---------------------------------"

      *>        VALIDATION LOOP FOR PLAN OPTION
               MOVE 'N' TO Found-Flag
               PERFORM UNTIL Found-Flag = 'Y'
                   DISPLAY "Enter Option Number: "
                   ACCEPT User-Choice

      *>            Validate User Input with Master File
                   OPEN INPUT MasterFile
                   MOVE 'N' TO EOF-FLAG
                   MOVE 0 TO Plan-Counter
                   PERFORM UNTIL EOF-FLAG = 'Y'
                       READ MasterFile
                           AT END MOVE 'Y' TO EOF-FLAG
                           NOT AT END
                               IF Active_Flag = 'Y'
                                   ADD 1 TO Plan-Counter

                                   IF Plan-Counter = User-Choice
                                       MOVE 'Y' TO Found-Flag
                                    MOVE Plan_code TO Selected-Plan-Cd
                                    MOVE Plan_Name TO Target-Plan-Name
                                    MOVE Base_Rate TO Target-Base-Rate
                                       MOVE Max_Payout TO Target-Max-Pay
                                   END-IF
                               END-IF
                       END-READ
                   END-PERFORM
                   CLOSE MasterFile

                   IF Found-Flag = 'N'
                     DISPLAY "Error: Invalid Option Number! Try again."
                     DISPLAY " "
                   END-IF
               END-PERFORM

      *>        Formatting Numbers
               MOVE Target-Base-Rate TO Formatted-Base
               MOVE Target-Max-Pay   TO Formatted-Max

      *>        SHOW PLAN DETAILS
               DISPLAY " "
               DISPLAY "=============================================="
               DISPLAY "Selected Plan : "
               FUNCTION TRIM(Target-Plan-Name)
               DISPLAY "Base Rate     : "
                       FUNCTION TRIM(Formatted-Base) " JPY"
               DISPLAY "Coverage      : "
                       FUNCTION TRIM(Formatted-Max) " JPY"
               DISPLAY "----------------------------------------------"
               DISPLAY "Coverages Included:"

               OPEN INPUT CoverageFile
               MOVE 'N' TO EOF-FLAG
               PERFORM UNTIL EOF-FLAG = 'Y'
                   READ CoverageFile
                       AT END MOVE 'Y' TO EOF-FLAG
                       NOT AT END
                           UNSTRING Coverage_Raw DELIMITED BY ","
                               INTO WS-Plan-cd WS-Cov-Type
                                    WS-Enable-Flag
                           END-UNSTRING

                           IF FUNCTION TRIM(WS-Plan-cd) = FUNCTION
                              TRIM(Selected-Plan-Cd)
                              AND WS-Enable-Flag = 'Y'
                               DISPLAY "- " FUNCTION TRIM(WS-Cov-Type)
                           END-IF
                   END-READ
               END-PERFORM
               CLOSE CoverageFile
               DISPLAY "=============================================="

      *>        VALIDATION LOOP FOR CONTINUE INPUT (Only Y or N)
               MOVE 'N' TO Valid-Loop-Input
               PERFORM UNTIL Valid-Loop-Input = 'Y'
                   DISPLAY " "
                   DISPLAY "Do you want to check another plan? (Y/N): "
                   ACCEPT Loop-Flag

                   IF Loop-Flag = 'Y' OR Loop-Flag = 'y'
                      OR Loop-Flag = 'N' OR Loop-Flag = 'n'
                       MOVE 'Y' TO Valid-Loop-Input
                   ELSE
                   DISPLAY "Error: Invalid Input! Please enter Y or N."
                   END-IF
               END-PERFORM

           END-PERFORM.

           DISPLAY " "
           DISPLAY "Plan Selection Process Completed. Thank you!"

      *Gather all the data and prepare to send it to Screen 3
           MOVE LNK-DeviceType TO WS-C-DeviceType.
           MOVE LNK-DeviceModel TO WS-C-DeviceModel.
           MOVE LNK-PurchasePrice TO WS-C-PurchasePrice.
           MOVE LNK-PurchaseDate TO WS-C-PurchaseDate.
           MOVE LNK-CoveragePeriod TO WS-C-CoveragePeriod.
           MOVE LNK-EstPremium TO WS-C-EstPremium.
           MOVE LNK-IMEI TO WS-C-IMEI.
           MOVE Target-Plan-Name TO WS-C-PlanName.

           

           STOP RUN.

       END PROGRAM PLAN-SELECTION-SYSTEM.