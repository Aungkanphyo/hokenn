       IDENTIFICATION DIVISION.
       PROGRAM-ID. screen5.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT CSV-FILE ASSIGN TO "Data/T_Application.csv"
               ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.
       FD  CSV-FILE.
       01  CSV-RECORD              PIC X(500).

       WORKING-STORAGE SECTION.
       01  WS-EOF                  PIC X(1) VALUE "N".
       01  WS-FIRST-ROW            PIC X(1) VALUE "Y".
       01  WS-FOUND                PIC X(1) VALUE "N".

       01  Formatted-Est-Premium   PIC ZZ,ZZZ,ZZ9.

       01  WS-CSV-FIELDS.
           05 W-NAME               PIC X(30).
           05 W-PHONE              PIC X(15).
           05 W-EMAIL              PIC X(40).
           05 W-DOB                PIC X(10).
           05 W-ADDRESS            PIC X(30).
           05 W-POSTAL-CODE        PIC X(10).
           05 W-DATE               PIC X(8).
           05 W-TIME               PIC X(6).
           05 W-STATUS             PIC X(10).
           05 W-DEVICE-TYPE        PIC X(15).
           05 W-DEVICE-MODEL       PIC X(20).
           05 W-PURCHASE-PRICE     PIC X(10).
           05 W-PURCHASE-DATE      PIC X(10).
           05 W-COVERAGE-PERIOD    PIC X(5).
           05 W-EST-PREMIUM        PIC X(10).
           05 W-PLAN-NAME          PIC X(15).
           05 W-IMEI               PIC X(15).

       01  WS-Display-Fields.
           05 DET-IMEI             PIC X(16) VALUE SPACES.
           05 DET-NAME             PIC X(12) VALUE SPACES.
           05 DET-PLAN             PIC X(12) VALUE SPACES.
           05 DET-PREMIUM          PIC X(10) VALUE SPACES.

       LINKAGE SECTION.
       01  LNK-IMEI            PIC X(15).

       PROCEDURE DIVISION USING LNK-IMEI.
       MAIN-PROCEDURE.
           
           OPEN INPUT CSV-FILE   
           PERFORM UNTIL WS-EOF = "Y"
               READ CSV-FILE
                   AT END
                       MOVE "Y" TO WS-EOF
                   NOT AT END
                       IF WS-FIRST-ROW = "Y"
                           MOVE "N" TO WS-FIRST-ROW
                       ELSE
                           PERFORM PARSE-AND-CHECK
                       END-IF
               END-READ
           END-PERFORM
           
           CLOSE CSV-FILE
           
           IF WS-FOUND = "N"
               DISPLAY "---------------------------------------------"
               DISPLAY "Error: Record with IMEI " 
               DISPLAY FUNCTION TRIM(LNK-IMEI) " not found!"
               DISPLAY "---------------------------------------------"
           ELSE
               DISPLAY "---------------------------------------------"
           END-IF
           
           STOP RUN.

       PARSE-AND-CHECK.
           INITIALIZE WS-CSV-FIELDS
           UNSTRING CSV-RECORD DELIMITED BY ","
               INTO W-NAME, W-PHONE, W-EMAIL, W-DOB, 
                    W-ADDRESS, W-POSTAL-CODE, W-DATE, 
                    W-TIME, W-STATUS, W-DEVICE-TYPE, 
                    W-DEVICE-MODEL, W-PURCHASE-PRICE, 
                    W-PURCHASE-DATE, W-COVERAGE-PERIOD, 
                    W-EST-PREMIUM, W-PLAN-NAME, W-IMEI
           END-UNSTRING

           IF W-IMEI = LNK-IMEI
               MOVE "Y" TO WS-FOUND
               
               MOVE W-IMEI        TO DET-IMEI
               MOVE W-NAME        TO DET-NAME
               MOVE W-PLAN-NAME   TO DET-PLAN
               MOVE W-EST-PREMIUM TO DET-PREMIUM
               MOVE DET-PREMIUM   TO Formatted-Est-Premium
               
               DISPLAY "============ APPLICATION RECEIPT ============"
               DISPLAY " IMEI NO     :      " DET-IMEI
               DISPLAY " CUSTOMER    :      " DET-NAME
               DISPLAY " PLAN NAME   :      " DET-PLAN
               DISPLAY " EST PREMIUM : " Formatted-Est-Premium "JPY"
               DISPLAY "============================================="
               DISPLAY "Your Application has been submitted."
               DISPLAY "The underwriting result will be "
               DISPLAY "processed by the nightly batch!"
           END-IF.
           