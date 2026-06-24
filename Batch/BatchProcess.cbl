       IDENTIFICATION DIVISION.
       PROGRAM-ID. PROGRAM-NAME.
       AUTHOR. AUNGKANPHYO.
      *
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE CONTROL.
           SELECT AppFile ASSIGN TO "Screen3/T_Application.csv"
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS FS-APP.

           SELECT TmpFile ASSIGN TO "Batch/T_Application_BatchTmp.csv"
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS FS-TMP.

           SELECT Screen4File ASSIGN TO "Screen4/screen4_output.csv"
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS FS-S4.
      *
       DATA DIVISION.
       FILE SECTION.
       FD  AppFile.
       01  App-Record-Buf      PIC X(500).

       FD  TmpFile.
       01  Tmp-Record-Buf      PIC X(500).

       FD  Screen4File.
       01  S4-Record-Buf       PIC X(500).

       WORKING-STORAGE SECTION.
       01  FS-APP              PIC XX.
       01  FS-TMP              PIC XX.
       01  FS-S4               PIC XX.
       
       01  WS-EOF-APP          PIC X     VALUE 'N'.
       01  WS-EOF-S4           PIC X     VALUE 'N'.
       01  WS-MATCH-FOUND      PIC X     VALUE 'N'.
       01  WS-LOOP-COUNT       PIC 99    VALUE 0.
       01  WS-UPDATE-COUNT     PIC 9(4)  VALUE 0.

       01  WS-APP-FIELDS.
           05 F-NAME           PIC X(20).
           05 F-PHONE          PIC X(11).
           05 F-EMAIL          PIC X(40).
           05 F-DOB            PIC X(10).
           05 F-ADDRESS        PIC X(40).
           05 F-POSTAL-CODE    PIC X(7).
           05 F-DATE           PIC X(8).
           05 F-TIME           PIC X(6).
           05 F-STATUS         PIC X(15).
           05 F-DEVTYPE        PIC X(10).
           05 F-DEVMODEL       PIC X(25).
           05 F-PRICE          PIC X(8).
           05 F-PURDATE        PIC X(10).
           05 F-PERIOD         PIC X(2).
           05 F-PREMIUM        PIC X(8).
           05 F-PLANNAME       PIC X(20).
           05 F-IMEI           PIC X(15).

      *    Temporary buffer for Screen4 IMEI
       01  S4-IMEI             PIC X(15).
      *
       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
           DISPLAY " "
           DISPLAY "=================================================="
           DISPLAY "     EXECUTING BACKGROUND BATCH PROCESSING        "
           DISPLAY "=================================================="
           DISPLAY "Processing updates tonight. Simulating 8s delay..."

      *    Progress Bar 8 Seconds Delay Simulation
           PERFORM VARYING WS-LOOP-COUNT FROM 1 BY 1 
           UNTIL WS-LOOP-COUNT > 8
               CALL "C$SLEEP" USING BY VALUE 1
               DISPLAY "[" WS-LOOP-COUNT "/8 Seconds Processed...]"
           END-PERFORM
           DISPLAY "Connecting to database files..."

           OPEN INPUT AppFile
           OPEN OUTPUT TmpFile

           IF FS-APP NOT = "00"
               DISPLAY "ERROR: Cannot open T_Application.csv! Code: " 
                       FS-APP
               GO TO TERMINATE-PROGRAM
           END-IF.

           READ AppFile
           WRITE Tmp-Record-Buf FROM App-Record-Buf.

           MOVE "N" TO WS-EOF-APP.
           MOVE 0 TO WS-UPDATE-COUNT.

      *    Read application records and update PENDING statuses
           PERFORM UNTIL WS-EOF-APP = 'Y'
               READ AppFile
                   AT END MOVE 'Y' TO WS-EOF-APP
                   NOT AT END
                       INITIALIZE WS-APP-FIELDS
                       UNSTRING App-Record-Buf DELIMITED BY ","
                           INTO F-NAME F-PHONE F-EMAIL F-DOB F-ADDRESS 
                                F-POSTAL-CODE F-DATE F-TIME F-STATUS 
                                F-DEVTYPE F-DEVMODEL F-PRICE F-PURDATE 
                                F-PERIOD F-PREMIUM F-PLANNAME F-IMEI
                       END-UNSTRING

                       *> Match and process only PENDING records
                       IF FUNCTION UPPER-CASE(FUNCTION TRIM(F-STATUS)) 
                          = "PENDING"
                           PERFORM LOOKUP-SCREEN4-STATUS
                           IF WS-MATCH-FOUND = 'Y'
                               ADD 1 TO WS-UPDATE-COUNT
                           END-IF
                       END-IF

                       *> Re-string all 17 fields into CSV format
                       MOVE SPACES TO Tmp-Record-Buf
                       STRING
                           FUNCTION TRIM(F-NAME) DELIMITED BY SIZE ","
                           FUNCTION TRIM(F-PHONE) DELIMITED BY SIZE ","
                           FUNCTION TRIM(F-EMAIL) DELIMITED BY SIZE ","
                           FUNCTION TRIM(F-DOB) DELIMITED BY SIZE ","
                           FUNCTION TRIM(F-ADDRESS) 
                           DELIMITED BY SIZE ","
                           FUNCTION TRIM(F-POSTAL-CODE) 
                           DELIMITED BY SIZE ","
                           FUNCTION TRIM(F-DATE) DELIMITED BY SIZE ","
                           FUNCTION TRIM(F-TIME) DELIMITED BY SIZE ","
                           FUNCTION TRIM(F-STATUS) DELIMITED BY SIZE ","
                           FUNCTION TRIM(F-DEVTYPE) 
                           DELIMITED BY SIZE ","
                           FUNCTION TRIM(F-DEVMODEL) 
                           DELIMITED BY SIZE ","
                           FUNCTION TRIM(F-PRICE) DELIMITED BY SIZE ","
                           FUNCTION TRIM(F-PURDATE) 
                           DELIMITED BY SIZE ","
                           FUNCTION TRIM(F-PERIOD) DELIMITED BY SIZE ","
                           FUNCTION TRIM(F-PREMIUM) 
                           DELIMITED BY SIZE ","
                           FUNCTION TRIM(F-PLANNAME) 
                           DELIMITED BY SIZE ","
                           FUNCTION TRIM(F-IMEI) DELIMITED BY SIZE
                           INTO Tmp-Record-Buf
                       END-STRING
                       
                       WRITE Tmp-Record-Buf
               END-READ
           END-PERFORM.

           CLOSE AppFile
           CLOSE TmpFile.

      *    Overwrite the original CSV file with updated data
           PERFORM COPY-BACK-TO-ORIGINAL.

           DISPLAY "--------------------------------------------------"
           DISPLAY "SUCCESS: Batch processing engine completed."
           DISPLAY "Total Application Statuses Updated: " 
           WS-UPDATE-COUNT
           DISPLAY "==================================================".
           
       TERMINATE-PROGRAM.
           EXIT PROGRAM.

       LOOKUP-SCREEN4-STATUS.
           MOVE 'N' TO WS-MATCH-FOUND.
           MOVE 'N' TO WS-EOF-S4.
           OPEN INPUT Screen4File.

           IF FS-S4 = "00"
               PERFORM UNTIL WS-EOF-S4 = 'Y' OR WS-MATCH-FOUND = 'Y'
                   READ Screen4File
                       AT END MOVE 'Y' TO WS-EOF-S4
                       NOT AT END
                           INITIALIZE S4-IMEI
                           *> Extract IMEI from Screen4 CSV line
                           UNSTRING S4-Record-Buf DELIMITED BY " , " 
                           OR ","
                               INTO S4-IMEI
                           END-UNSTRING

                           IF FUNCTION TRIM(F-IMEI) = 
                           FUNCTION TRIM(S4-IMEI)
                               MOVE 'Y' TO WS-MATCH-FOUND
                               
      *                        Scan for final decision strings safely
                               MOVE 0 TO WS-LOOP-COUNT
                               INSPECT S4-Record-Buf 
                               TALLYING WS-LOOP-COUNT 
                                       FOR ALL "APPROVED"
                               IF WS-LOOP-COUNT > 0
                                   MOVE "APPROVED" TO F-STATUS
                               ELSE
                                   MOVE 0 TO WS-LOOP-COUNT
                                   INSPECT S4-Record-Buf 
                                   TALLYING WS-LOOP-COUNT 
                                           FOR ALL "REJECTED"
                                   IF WS-LOOP-COUNT > 0
                                       MOVE "REJECTED" TO F-STATUS
                                   END-IF
                               END-IF
                           END-IF
                   END-READ
               END-PERFORM
           END-IF.
           CLOSE Screen4File.

       COPY-BACK-TO-ORIGINAL.
           OPEN INPUT TmpFile.
           OPEN OUTPUT AppFile.
           MOVE 'N' TO WS-EOF-APP.
           
           PERFORM UNTIL WS-EOF-APP = 'Y'
               READ TmpFile
                   AT END MOVE 'Y' TO WS-EOF-APP
                   NOT AT END
                       WRITE App-Record-Buf FROM Tmp-Record-Buf
               END-READ
           END-PERFORM.
           
           CLOSE TmpFile.
           CLOSE AppFile.