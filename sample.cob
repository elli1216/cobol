       IDENTIFICATION DIVISION.
       PROGRAM-ID. BANK-FILTER-PROG.
       AUTHOR. DARL-FLORESCA.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           *> Use Case: Linking physical files 
           *> to our program with status tracking
           SELECT SAVINGS-FILE ASSIGN TO "SAVINGS.DAT"
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS WS-FS-SAVINGS.

           SELECT REPORT-FILE  ASSIGN TO "SAVINGS-REPORT.TXT"
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS WS-FS-REPORT.

       DATA DIVISION.
       FILE SECTION.
       FD  SAVINGS-FILE.
       *> FI Prefix: Identifies this as File Input data
       01  FI-SAVINGS-RECORD.
           05 FI-ACCT-ID       PIC 9(10).
           05 FI-ACCT-NAME     PIC X(30).
           05 FI-ACCT-STATUS   PIC X(01). *> 'A' for Active, 'I' for Inactive
           05 FI-BALANCE       PIC 9(8)V99.

       FD  REPORT-FILE.
       *> FO Prefix: Identifies this as File Output data
       01  FO-REPORT-RECORD.
           05 FO-ACCT-NAME     PIC X(30).
           05 FILLER           PIC X(05) VALUE SPACES.
           05 FO-DISPLAY-BAL   PIC Z,ZZZ,ZZ9.99.

       WORKING-STORAGE SECTION.
       *> WS Prefix: General working variables
       01  WS-FS-SAVINGS       PIC XX.  *> File Status for Input
       01  WS-FS-REPORT        PIC XX.  *> File Status for Output
       
       *> Use Case: Level-88 Flag for Program Flow (EOF)
       01  WS-EOF-SWITCH       PIC X(01) VALUE 'N'.
           88  END-OF-FILE               VALUE 'Y'.
           88  NOT-END-OF-FILE           VALUE 'N'.

       *> Use Case: Level-88 Flag for Business Logic (Account Status)
       01  WS-ACCT-FLAG        PIC X(01).
           88  ACCOUNT-IS-ACTIVE         VALUE 'A'.
           88  ACCOUNT-IS-INACTIVE       VALUE 'I'.

       PROCEDURE DIVISION.
       0000-MAIN-CONTROL.
           PERFORM 1000-OPEN-FILES.
           
           *> The Priming Read: Getting the first record before the loop
           PERFORM 2000-READ-SAVINGS.

           *> Use Case: Using the EOF flag to control the loop
           PERFORM 3000-PROCESS-RECORDS UNTIL END-OF-FILE.

           PERFORM 4000-CLOSE-FILES.
           STOP RUN.

       1000-OPEN-FILES.
           OPEN INPUT SAVINGS-FILE.
           *> Error Handling: Checking if input file exists
           *> (Status 35 = Not Found)
           IF WS-FS-SAVINGS NOT = "00"
               DISPLAY "CRITICAL ERROR: SAVINGS FILE NOT FOUND (FS: "
                WS-FS-SAVINGS ")"
               STOP RUN
           END-IF.

           OPEN OUTPUT REPORT-FILE.
           IF WS-FS-REPORT NOT = "00"
               DISPLAY "CRITICAL ERROR: COULD NOT CREATE REPORT (FS: "
                WS-FS-REPORT ")"
               STOP RUN
           END-IF.

       2000-READ-SAVINGS.
           READ SAVINGS-FILE
               AT END SET END-OF-FILE TO TRUE
           END-READ.

       3000-PROCESS-RECORDS.
           *> Use Case: Moving input data to our status flag variable
           MOVE FI-ACCT-STATUS TO WS-ACCT-FLAG.

           *> Use Case: Conditional logic using Level-88 Flag
           IF ACCOUNT-IS-ACTIVE
               MOVE FI-ACCT-NAME TO FO-ACCT-NAME
               MOVE FI-BALANCE   TO FO-DISPLAY-BAL
               
               WRITE FO-REPORT-RECORD
               
               *> Error Handling: Catching full disk or write permissions
               IF WS-FS-REPORT NOT = "00"
                   DISPLAY "WRITE ERROR DETECTED: " WS-FS-REPORT
                   SET END-OF-FILE TO TRUE
               END-IF
           END-IF.

           *> Get the next record to keep the loop moving
           PERFORM 2000-READ-SAVINGS.

       4000-CLOSE-FILES.
           CLOSE SAVINGS-FILE.
           CLOSE REPORT-FILE.
           DISPLAY "PROCESS COMPLETE. ACTIVE ACCOUNTS REPORTED.".