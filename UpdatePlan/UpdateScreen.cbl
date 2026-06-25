       IDENTIFICATION DIVISION.
       PROGRAM-ID. UpdateScreen.
       AUTHOR. AUNGKANPHYO.
      *
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT AppFile ASSIGN TO "Data/T_Application.csv"
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS FS-App.

           SELECT TmpAppFile ASSIGN TO "Data/T_Application_Tmp.csv"
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS FS-Temp.

           SELECT PlanFile ASSIGN TO "Data/M_Plan.csv"
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS FS-Plan.

           SELECT OPTIONAL AuditFile ASSIGN TO "Data/AuditLog.csv"
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS FS-Audit.
      
       DATA DIVISION.
       FILE SECTION.
       FD AppFile.
       01 App-Record-Buf PIC X(400).

       FD TmpAppFile.
       01 Temp-Record-Buff PIC X(400).

       FD PlanFile.
       01 M-Plan-Record.
           05 P-Plan-Code PIC X(5).
           05 F1 PIC X.
           05 P-Plan-Name PIC X(20).
           05 F2 PIC X.
           05 P-Base-Rate PIC 9(5).
           05 F3 PIC X.
           05 P-Max-Payout PIC 9(8).
           05 F4 PIC X.
           05 P-Active-Flag PIC X.

       FD AuditFile.
       01 Audit-Record-Buff PIC X(200).
       WORKING-STORAGE SECTION.
       01  FS-App PIC XX.
       01  FS-Temp PIC XX.
       01  FS-Plan PIC XX.
       01  FS-Audit PIC XX.
       
       01  WS-EOF-FLAG PIC X VALUE 'N'.
       01  WS-EOF-APP PIC X VALUE 'N'.
       01  WS-Rec-Found PIC X VALUE 'N'.
       01 WS-IMEI-FOUND PIC X VALUE 'N'.
       01 WS-Hold-Status PIC X(20) VALUE SPACES.
       01  WS-Search-IMEI PIC X(15) VALUE SPACES.
       01  WS-Confirm PIC X VALUE SPACES.

      *Records Extraction Variables
       01  WS-APP-FIELDS.
           05  F-NAME PIC X(50).
           05  F-PHONE PIC X(20).
           05  F-EMAIL PIC X(50).
           05  F-DOB PIC X(20).
           05  F-ADDRESS PIC X(100).
           05  F-POSTAL-CODE PIC X(15).
           05  F-DATE PIC X(20).
           05  F-TIME PIC X(20).
           05  F-STATUS PIC X(20).
           05  F-DEVTYPE PIC X(20).
           05  F-DEVMODEL PIC X(50).
           05  F-PRICE PIC X(20).
           05  F-PURDATE PIC X(20).
           05  F-PERIOD PIC X(20).
           05  F-PREMIUM PIC X(20).
           05  F-PLANNAME PIC X(20).
           05  F-IMEI PIC X(15).

      *    Temporary variables to hold current user info
       01  WS-Hold-CustName PIC X(50) VALUE SPACES.
       01  WS-Hold-OldPlan PIC X(20) VALUE SPACES.
       01  WS-Hold-DevModel PIC X(50) VALUE SPACES.

      *Dynamic Plan Table Array
       01  WS-Plan-Table.
           05 WS-Total-Plans PIC 99 VALUE 0.
           05 WS-Plan-Item OCCURS 20 TIMES INDEXED BY plan-idx.
               10 WS-Sel-Plan-Name PIC X(20).

       01  WS-Plan-Choice PIC 99 VALUE 0.
       01  WS-New-PlanName PIC X(20) VALUE SPACES.
       01  WS-Display-Num PIC Z9.

      *Date and Time for Audit Log
       01  WS-Sys-Date-Time.
           05 WS-Sys-Year PIC 9(4).
           05 WS-Sys-Month PIC 9(2).
           05 WS-Sys-Day PIC 9(2).
           05 WS-Sys-Hour PIC 9(2).
           05 WS-Sys-Min PIC 9(2).
           05 WS-Sys-Sec PIC 9(2).
           05 FILLER PIC X(4).
      *
       PROCEDURE DIVISION.
       MAIN-FLOW.
           DISPLAY " "
           DISPLAY "================================================"
           DISPLAY "         CHANGE EXISTING INSURANCE PLAN         "
           DISPLAY "================================================"

           DISPLAY "Enter Device IMEI (15 digits): " WITH NO ADVANCING
           ACCEPT WS-Search-IMEI

      *    Find IMEI in Application File
           PERFORM Find-Application-Record

           IF WS-Rec-Found = 'N'
               IF WS-IMEI-Found = 'Y'
               DISPLAY " "
                   DISPLAY "Error: Cannot update this application!"
                   DISPLAY "Reason: Status is '" 
                           FUNCTION TRIM(WS-Hold-Status) "'."
                   DISPLAY "Only 'Accepted' applications "
                           "can be modified."
               ELSE
                   DISPLAY "Error: Application with IMEI " 
                            WS-Search-IMEI
                           " not found!"
               END-IF
               GOBACK
           END-IF

      *    Show Customer Info & Fetch Plans
           DISPLAY "------------------------------------------------"
           DISPLAY "Customer Name : " FUNCTION TRIM(WS-Hold-CustName)
           DISPLAY "Device Model  : " FUNCTION TRIM(WS-Hold-DevModel)
           DISPLAY "Current Plan  : " FUNCTION TRIM(WS-Hold-OldPlan)
           DISPLAY "------------------------------------------------"

           PERFORM Fetch-And-Dispaly-Plans

      *    Accept and Validate New Plan Choice
           MOVE 0 TO WS-Plan-Choice
           PERFORM UNTIL WS-Plan-Choice >= 1 AND
                         WS-Plan-Choice <= WS-Total-Plans
                DISPLAY "Select New Plan Number (1-" 
                        FUNCTION TRIM(WS-Total-Plans) "): " 
                        WITH NO ADVANCING
                ACCEPT WS-Plan-Choice
                IF WS-Plan-Choice < 1 OR WS-Plan-Choice > WS-Total-Plans
                   DISPLAY "Error: Invalid selection! Try again."
                END-IF
           END-PERFORM

           MOVE WS-Sel-Plan-Name(WS-Plan-Choice) TO WS-New-PlanName

           IF FUNCTION TRIM(WS-New-PlanName) = 
              FUNCTION TRIM(WS-Hold-OldPlan)
               DISPLAY "Error: The new plan is the same as current "
                       "plan."
               DISPLAY "Modification Cancelled."
               GOBACK
           END-IF

      *    Confirm and Update Record
           DISPLAY "Are you sure you want to change plan to '" 
                   FUNCTION TRIM(WS-New-PlanName) "'? (Y/N): "
                   WITH NO ADVANCING
           ACCEPT WS-Confirm

           IF WS-Confirm = 'Y' OR WS-Confirm = 'y'
              PERFORM Update-Application-File
              PERFORM Write-Audit-Log
              DISPLAY "SUCCESS: Insurance Plan Updated Successfully!"
           ELSE
              DISPLAY "Process Cancelled By User."
           END-IF.

           GOBACK.

      * Paragraph working section
       Find-Application-Record.
           OPEN INPUT AppFile
           IF FS-App NOT = "00"
               DISPLAY "Error: Cannot open T_Application.csv File!"
               CLOSE AppFile
               EXIT PARAGRAPH
           END-IF.

           MOVE 'N' TO WS-EOF-FLAG
           MOVE 'N' TO WS-Rec-Found

           MOVE 'N' TO WS-IMEI-Found
           MOVE SPACES TO WS-Hold-Status

           PERFORM UNTIL WS-EOF-FLAG = 'Y' OR WS-Rec-Found = 'Y'
               READ AppFile
                 AT END MOVE 'Y' TO WS-EOF-FLAG
                 NOT AT END
                   IF App-Record-Buf(1:4) NOT = "NAME"
                       INITIALIZE WS-APP-FIELDS
                       UNSTRING App-Record-Buf DELIMITED BY ","
                         INTO F-NAME F-PHONE F-EMAIL F-DOB F-ADDRESS 
                              F-POSTAL-CODE F-DATE F-TIME F-STATUS 
                              F-DEVTYPE F-DEVMODEL F-PRICE F-PURDATE 
                              F-PERIOD F-PREMIUM F-PLANNAME F-IMEI
                       END-UNSTRING

                       IF FUNCTION TRIM(F-IMEI) = 
                          FUNCTION TRIM(WS-Search-IMEI)

                          MOVE 'Y' TO WS-IMEI-Found
                          MOVE F-STATUS TO WS-Hold-Status

                          IF FUNCTION TRIM(F-STATUS) = "Accepted"
                               MOVE 'Y' TO WS-Rec-Found
                               MOVE F-NAME TO WS-Hold-CustName
                               MOVE F-PLANNAME TO WS-Hold-OldPlan
                               MOVE F-DEVMODEL TO WS-Hold-DevModel
                          END-IF     
                       END-IF
                   END-IF
               END-READ
           END-PERFORM
           CLOSE AppFile.
       
      *Start
       Fetch-And-Dispaly-Plans.
           DISPLAY "Available Plans to Upgrade/Downgrade:"
           OPEN INPUT PlanFile
           MOVE 'N' TO WS-EOF-FLAG
           MOVE 0 TO WS-Total-Plans

           PERFORM UNTIL WS-EOF-FLAG = 'Y' OR WS-Total-Plans >= 20
              READ PlanFile
               AT END MOVE 'Y' TO WS-EOF-FLAG
               NOT AT END
                   IF P-Active-Flag = 'Y'
                      ADD 1 TO WS-Total-Plans
                      MOVE P-Plan-Name TO 
                           WS-Sel-Plan-Name(WS-Total-Plans)
                      MOVE WS-Total-Plans TO WS-Display-Num
                      DISPLAY FUNCTION TRIM(WS-Display-Num) ". "
                              FUNCTION TRIM(P-Plan-Name)
                   END-IF
              END-READ
           END-PERFORM
           CLOSE PlanFile
           DISPLAY "------------------------------------------------".
      *End

      * Start
       Update-Application-File.
      *Read from Original and Write to Temp with updated data
           OPEN INPUT AppFile
           OPEN OUTPUT TmpAppFile
           MOVE 'N' TO WS-EOF-APP

           PERFORM UNTIL WS-EOF-APP = 'Y'
             READ AppFile
               AT END MOVE 'Y' TO WS-EOF-APP
               NOT AT END
                 IF App-Record-Buf(1:4) = "NAME"
                   WRITE Temp-Record-Buff FROM App-Record-Buf
                 ELSE
                   INITIALIZE WS-APP-FIELDS
                   UNSTRING App-Record-Buf DELIMITED BY ","
                       INTO F-NAME F-PHONE F-EMAIL F-DOB F-ADDRESS 
                            F-POSTAL-CODE F-DATE F-TIME F-STATUS 
                            F-DEVTYPE F-DEVMODEL F-PRICE F-PURDATE 
                            F-PERIOD F-PREMIUM F-PLANNAME F-IMEI
                   END-UNSTRING

                   IF FUNCTION TRIM(F-IMEI) = 
                   FUNCTION TRIM(WS-Search-IMEI)
                    MOVE SPACES TO Temp-Record-Buff
                    STRING
                     FUNCTION TRIM(F-NAME) DELIMITED BY SIZE ","
                     FUNCTION TRIM(F-PHONE) DELIMITED BY SIZE ","
                     FUNCTION TRIM(F-EMAIL) DELIMITED BY SIZE ","
                     FUNCTION TRIM(F-DOB) DELIMITED BY SIZE ","
                     FUNCTION TRIM(F-ADDRESS) DELIMITED BY SIZE ","
                     FUNCTION TRIM(F-POSTAL-CODE) DELIMITED BY SIZE ","
                     FUNCTION TRIM(F-DATE) DELIMITED BY SIZE ","
                     FUNCTION TRIM(F-TIME) DELIMITED BY SIZE ","
                     FUNCTION TRIM(F-STATUS) DELIMITED BY SIZE ","
                     FUNCTION TRIM(F-DEVTYPE) DELIMITED BY SIZE ","
                     FUNCTION TRIM(F-DEVMODEL) DELIMITED BY SIZE ","
                     FUNCTION TRIM(F-PRICE) DELIMITED BY SIZE ","
                     FUNCTION TRIM(F-PURDATE) DELIMITED BY SIZE ","
                     FUNCTION TRIM(F-PERIOD) DELIMITED BY SIZE ","
                     FUNCTION TRIM(F-PREMIUM) DELIMITED BY SIZE ","
                     FUNCTION TRIM(WS-New-PlanName) DELIMITED 
                     BY SIZE ","
                     FUNCTION TRIM(F-IMEI) DELIMITED BY SIZE
                        INTO Temp-Record-Buff
                    END-STRING
                    WRITE Temp-Record-Buff
                    ELSE
                       WRITE Temp-Record-Buff FROM App-Record-Buf
                   END-IF
                 END-IF
             END-READ
           END-PERFORM
           CLOSE AppFile
           CLOSE TmpAppFile.

      *    Write back from Temp to Original File
           OPEN INPUT TmpAppFile
           OPEN OUTPUT AppFile
           MOVE 'N' TO WS-EOF-APP
           PERFORM UNTIL WS-EOF-APP = 'Y'
              READ TmpAppFile
               AT END MOVE 'Y' TO WS-EOF-APP
               NOT AT END
                   WRITE App-Record-Buf FROM Temp-Record-Buff
              END-READ
           END-PERFORM
           CLOSE TmpAppFile
           CLOSE AppFile.
      *End

      *Start
       Write-Audit-Log.
           MOVE FUNCTION CURRENT-DATE TO WS-Sys-Date-Time
           OPEN EXTEND AuditFile
           MOVE SPACES TO Audit-Record-Buff
           STRING
               WS-Sys-Year "/" WS-Sys-Month "/" WS-Sys-Day ","
               WS-Sys-Hour ":" WS-Sys-Min ":" WS-Sys-Sec ","
               FUNCTION TRIM(WS-Search-IMEI) DELIMITED BY SIZE ","
               FUNCTION TRIM(WS-Hold-CustName) DELIMITED BY SIZE ","
               FUNCTION TRIM(WS-Hold-OldPlan) DELIMITED BY SIZE ","
               FUNCTION TRIM(WS-New-PlanName) DELIMITED BY SIZE ","
               "PLAN_CHANGED" DELIMITED BY SIZE
               INTO Audit-Record-Buff
           END-STRING
           WRITE Audit-Record-Buff
           CLOSE AuditFile.
      *End

       END PROGRAM UpdateScreen.
