       IDENTIFICATION DIVISION.
       PROGRAM-ID. EZT-TO-COBOL-PRACTICE.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT IN-FILE  ASSIGN TO "INPUT.DAT"
               ORGANIZATION IS LINE SEQUENTIAL.
           SELECT OUT-FILE ASSIGN TO "OUTPUT.DAT"
               ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.
       FD  IN-FILE.
       01  IN-RECORD.
           05 IN-STUDENT-ID    PIC 9(05).
           05 FILLER           PIC X(01).
           05 IN-STUDENT-NAME  PIC X(20).

       FD  OUT-FILE.
       01  OUT-RECORD.
           05 OUT-STUDENT-ID   PIC 9(05).
           05 FILLER           PIC X(01) VALUE SPACE.
           05 OUT-STUDENT-NAME PIC X(20).

       WORKING-STORAGE SECTION.
       01  WS-FLAGS.
           05 WS-EOF-FLAG      PIC X(01) VALUE 'N'.
              88 END-OF-FILE             VALUE 'Y'.

       PROCEDURE DIVISION.
       0000-MAIN.
           OPEN INPUT  IN-FILE
                OUTPUT OUT-FILE

           *> The first READ (The "Prime" Read)
           READ IN-FILE
               AT END SET END-OF-FILE TO TRUE
           END-READ

           *> This is your "JOB INPUT" loop from Easytrieve
           PERFORM 1000-PROCESS-RECORDS UNTIL END-OF-FILE

           CLOSE IN-FILE OUT-FILE
           STOP RUN.

       1000-PROCESS-RECORDS.
           IF IN-STUDENT-ID > 10000
               MOVE IN-STUDENT-ID   TO OUT-STUDENT-ID
               MOVE IN-STUDENT-NAME TO OUT-STUDENT-NAME
               
               WRITE OUT-RECORD
      
               READ IN-FILE
                   AT END SET END-OF-FILE TO TRUE
               END-READ
           END-IF.
           