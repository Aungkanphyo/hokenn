      ******************************************************************
      * Author:
      * Date:
      * Purpose:
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. YOUR-PROGRAM-NAME.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT Device ASSIGN TO "DeviceType.csv"
               ORGANIZATION IS Line SEQUENTIAL.
          
       DATA DIVISION.
       FILE SECTION.
       FD Device.
       01 Device_Record.
           05 Device_Name pic X(30).
       
       WORKING-STORAGE SECTION.
       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
           open output Device
           move 'iPhone XR' to Device_Name
           write Device_Record.
           MOVE 'iPhone 11' TO Device_Name
        WRITE Device_Record.

        MOVE 'iPhone 12' TO Device_Name
        WRITE Device_Record.

        MOVE 'iPhone 13' TO Device_Name
        WRITE Device_Record.

        MOVE 'iPhone 14' TO Device_Name
        WRITE Device_Record.

        MOVE 'iPhone 15' TO Device_Name
        WRITE Device_Record.

        MOVE 'iPhone 16' TO Device_Name
        WRITE Device_Record.

        MOVE 'iPhone 15 Pro' TO Device_Name
        WRITE Device_Record.

        MOVE 'iPhone 16 Pro' TO Device_Name
        WRITE Device_Record.
        
        MOVE 'Samsung S24 Ultra' TO Device_Record
        WRITE Device_Record.

        MOVE 'Google Pixel 8 Pro' TO Device_Record
        WRITE Device_Record.

        MOVE 'OnePlus 12' TO Device_Record
        WRITE Device_Record.

        MOVE 'Xiaomi 14 Ultra' TO Device_Record
        WRITE Device_Record.

        MOVE 'Samsung Galaxy A55' TO Device_Record
        WRITE Device_Record.

        MOVE 'Oppo Reno 12' TO Device_Record
        WRITE Device_Record.

        MOVE 'Vivo V30 Pro' TO Device_Record
        WRITE Device_Record.

        MOVE 'Realme GT 6' TO Device_Record
        WRITE Device_Record.

       MOVE 'iPhone 14 pro max' TO Device_Record
       WRITE Device_Record.
           close  Device
            STOP RUN.
       END PROGRAM YOUR-PROGRAM-NAME.
