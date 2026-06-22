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
           SELECT MasterFile ASSIGN TO "M_Plan.csv"
               ORGANIZATION IS Line SEQUENTIAL.
           SELECT CoverageFile ASSIGN To 'M_Plan_Coverage.csv'
               ORGANIZATION IS Line SEQUENTIAL.
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
       01 Coverage_Record.
           05 Plan_cd pic X(6).
           05 B1 PIC X.
           05 Coverage_Type pic x(20).
           05 B2 PIC X.
           05 Enable_Flag pic X.
       WORKING-STORAGE SECTION.
       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
           OPEN OUTPUT MasterFile.
           MOVE 'PLN-L' to Plan_code
           MOVE ',' To F1
           MOVE 'Light' To Plan_Name
            MOVE ',' To F2
           MOVE 500 To Base_Rate
            MOVE ',' To F3
           MOVE 30000 To Max_Payout
            MOVE ',' To F4
           MOVE 'Y' To Active_Flag
           WRITE M_Plan_Record.

           MOVE 'PLN-H' to Plan_code
           MOVE ',' To F1
           MOVE 'High' To Plan_Name
            MOVE ',' To F2
           MOVE 500 To Base_Rate
            MOVE ',' To F3
           MOVE 30000 To Max_Payout
            MOVE ',' To F4
           MOVE 'Y' To Active_Flag
           WRITE M_Plan_Record.

           MOVE 'PLN-S' to Plan_code
            MOVE ',' To F1
           MOVE 'Standard' To Plan_Name
            MOVE ',' To F2
           MOVE 2000 To Base_Rate
            MOVE ',' To F3
           MOVE 60000 To Max_Payout
            MOVE ',' To F4
           MOVE 'Y' To Active_Flag
           WRITE M_Plan_Record.

           MOVE 'PLN-P' to Plan_code
           MOVE ',' To F1
           MOVE 'Premium' To Plan_Name
            MOVE ',' To F2
           MOVE 5000 To Base_Rate
            MOVE ',' To F3
           MOVE 100000 To Max_Payout
            MOVE ',' To F4
           MOVE 'Y' To Active_Flag
           WRITE M_Plan_Record.

           CLOSE MasterFile.

      *>      CoverageFile
           OPEN OUTPUT CoverageFile.
           MOVE 'PLN-L' to Plan_cd
           MOVE ',' to B1
           Move 'Screen Damage' to Coverage_Type
           MOVE ',' to B2
           Move 'Y' To Enable_Flag
           WRITE Coverage_Record.

         

           MOVE 'PLN-L' to Plan_cd
           MOVE ',' to B1
           Move 'Water Damage' to Coverage_Type
           MOVE ',' to B2
           Move 'N' To Enable_Flag
           WRITE Coverage_Record.

           MOVE 'PLN-L' to Plan_cd
           MOVE ',' to B1
           Move 'Natural Failure' to Coverage_Type
           MOVE ',' to B2
           Move 'N' To Enable_Flag
           WRITE Coverage_Record.

           MOVE 'PLN-L' to Plan_cd
           MOVE ',' to B1
           Move 'Theft' to Coverage_Type
           MOVE ',' to B2
           Move 'N' To Enable_Flag
           WRITE Coverage_Record.

             MOVE 'PLN-H' to Plan_cd
           MOVE ',' to B1
           Move 'Screen Damage' to Coverage_Type
           MOVE ',' to B2
           Move 'Y' To Enable_Flag
           WRITE Coverage_Record.

      *>      Standard Plan
           MOVE 'PLN-S' to Plan_cd
           MOVE ',' to B1
           Move 'Screen Damage' to Coverage_Type
           MOVE ',' to B2
           Move 'Y' To Enable_Flag
           WRITE Coverage_Record.

           MOVE 'PLN-S' to Plan_cd
           MOVE ',' to B1
           Move 'Water Damage' to Coverage_Type
           MOVE ',' to B2
           Move 'Y' To Enable_Flag
           WRITE Coverage_Record.

           MOVE 'PLN-S' to Plan_cd
           MOVE ',' to B1
           Move 'Natural Failure' to Coverage_Type
           MOVE ',' to B2
           Move 'Y' To Enable_Flag
           WRITE Coverage_Record.

           MOVE 'PLN-S' to Plan_cd
           MOVE ',' to B1
           Move 'Theft' to Coverage_Type
           MOVE ',' to B2
           Move 'N' To Enable_Flag
           WRITE Coverage_Record.
      *>      Premium Plan

           MOVE 'PLN-P' to Plan_cd
           MOVE ',' to B1
           Move 'Screen Damage' to Coverage_Type
           Move 'Y' To Enable_Flag
           WRITE Coverage_Record.

           MOVE 'PLN-P' to Plan_cd
           MOVE ',' to B1
           Move 'Water Damage' to Coverage_Type
           MOVE ',' to B2
           Move 'Y' To Enable_Flag
           WRITE Coverage_Record.

           MOVE 'PLN-P' to Plan_cd
           MOVE ',' to B1
           Move 'Natural Failure' to Coverage_Type
           MOVE ',' to B2
           Move 'Y' To Enable_Flag
           WRITE Coverage_Record.

           MOVE 'PLN-P' to Plan_cd
           MOVE ',' to B1
           Move 'Theft' to Coverage_Type
           MOVE ',' to B2
           Move 'Y' To Enable_Flag
           WRITE Coverage_Record.
           CLOSE CoverageFile.
            STOP RUN.
       END PROGRAM YOUR-PROGRAM-NAME.