       IDENTIFICATION DIVISION.
       PROGRAM-ID. MainMenu.
       AUTHOR. AUNGKANPHYO.
      *
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
      *
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-MenuChoice PIC 9 VALUE 0.
       01 WS-LoopFlag PIC X VALUE 'Y'.
      *
       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
           PERFORM UNTIL WS-LoopFlag = 'N' OR 'n'
           DISPLAY " "
           DISPLAY "================================================"
           DISPLAY "         INSURANCE MANAGEMENT SYSTEM            "
           DISPLAY "================================================"
           DISPLAY " 1. Register New Insurance Policy"
           DISPLAY " 2. Change Existing Insurance Plan"
           DISPLAY " 3. View Plan Information"
           DISPLAY " 4. Exit System"
           DISPLAY "================================================"
           DISPLAY "Enter choice (1-3): " WITH NO ADVANCING
           ACCEPT WS-MenuChoice

           EVALUATE WS-MenuChoice
               WHEN 1
                   DISPLAY "Launching New Registration..."
                   CALL 'QuoScreen'
               WHEN 2
                   DISPLAY "Launching Plan Modification ..."
                   CALL 'UpdateScreen'
               WHEN 3
                   DISPLAY "Launching View Plan Information ..."
                   CALL 'Mypage'
               WHEN 4
                   MOVE 'N' TO WS-LoopFlag
                   DISPLAY "Thank you for using the system. Goodbye!"
               WHEN OTHER
                   DISPLAY "Error: Invalid choice! Please enter "
                           "1, 2, 3 or 4."
               END-EVALUATE
           END-PERFORM.
           STOP RUN.
           