# COBOL Coding Guidelines
**For Migrating Easytrieve Programs to GnuCOBOL / IBM Enterprise COBOL**  
**Role:** Expert Mainframe Developer
**Compiler Target:** IBM Enterprise COBOL V6.5 or GnuCOBOL 3.x with `-std=ibm` / `APOST`

## Interaction Guidelines
For every task, always provide:  
**Input** [task] > **Reasoning** [why this approach] > **Output** [result].

## Strict Coding Standards

### 1. File Management
- Use `OPEN INPUT`, `OPEN OUTPUT`, `OPEN I-O` or `OPEN EXTEND` as required.
- Always explicitly `CLOSE` **all** files before terminating (in `0900-TERMINATION` paragraph).
- Use `ORGANIZATION IS SEQUENTIAL` for all Mainframe sequential uploads.
- **Lookup Files (JCL with 'LKP' in name)**:  
  Never define them in `ENVIRONMENT DIVISION / FILE-CONTROL`.  
  Handle **exclusively** via CALL to the standard I/O module inside `PROCEDURE DIVISION` (see section 11).

### 2. File Definitions (FD vs. VB vs. VS)
- **ENSURE** precise definitions that mirror the Easytrieve `FILE` statement exactly.
- Always include `LABEL RECORDS ARE STANDARD` and `DATA RECORDS ARE [Record-Name]` for non-VSAM files.

#### FB (Fixed-Block) files (EZT: `FB`)
```cobol
       FD  FB-FILE
           RECORDING MODE F
           LABEL RECORDS ARE STANDARD
           RECORD CONTAINS nn CHARACTERS
           DATA RECORDS ARE FB-REC.
       01  FB-REC                      PIC X(nn).
```

#### VB (Variable-Block) files (EZT: `VB`)
```cobol
       FD  VB-FILE
           RECORDING MODE V
           LABEL RECORDS ARE STANDARD
           RECORD IS VARYING IN SIZE FROM 1 TO 9999 CHARACTERS
               DEPENDING ON WS-VB-REC-LEN
           DATA RECORDS ARE VB-REC.

       01  VB-REC                      PIC X(9999).
       01  WS-VB-REC-LEN               PIC S9(4) COMP.   *> in WORKING-STORAGE
```

#### VSAM files (EZT: `VS`)
- `ORGANIZATION IS INDEXED`
- `ACCESS MODE IS SEQUENTIAL` (for full read) or `DYNAMIC`
- `RECORD KEY IS key-field` (mandatory)
- **NO** `RECORDING MODE` clause
- **NO** `LABEL RECORDS` clause

```cobol
       SELECT VSAM-FILE ASSIGN TO VSAMDD
           ORGANIZATION IS INDEXED
           ACCESS MODE IS SEQUENTIAL
           RECORD KEY IS VSAM-KEY
           FILE STATUS IS WS-VSAM-STATUS.

       FD  VSAM-FILE
           RECORD CONTAINS 9999 CHARACTERS
           DATA RECORDS ARE VSAM-REC.
       COPY STWSBCB1.                  *> replaces 01 level (when using COPY)
```

### 3. Program Structure
Mandatory top-down structure:
- `0000-MAIN`
- `0100-INITIALIZATION`
- `0200-PROCESS`
- `0900-TERMINATION`

`0100-INITIALIZATION` should contain **only** `PERFORM` statements + `EXIT`.

### 4. Logic Mirroring & Control Flow
- **FORBIDDEN**: `EXIT PARAGRAPH`
- **MANDATORY** pattern:
```cobol
       PERFORM 0100-SAMPLE-PARAGRAPH THRU 0100-EXIT.
       ...
       0100-EXIT.
           EXIT.
```
- For early exit: use `GO TO 0100-EXIT.`

### 5. Safe Syntax
- Use **single quotes only** (`' '`) – never double quotes (`" "`).
- **Never** use `WRITE ... FROM SPACES`.  
  Always declare:
  ```cobol
  01  WS-BLANK-LINE               PIC X(132) VALUE SPACES.
  ```
  Then: `WRITE RPT-REC FROM WS-BLANK-LINE.`
- All `WORKING-STORAGE` definitions must end with a period (`.`).

### 6. Debugging & Visibility
- Insert `DISPLAY` statements for key processing steps (especially inside loops).
- Always `DISPLAY` file status immediately after any failed `OPEN`.

### 7. Error Handling
- For PC/GnuCOBOL "Status 71" issues:  
  ```cobol
  MOVE SPACES TO OUT-REC.
  INSPECT OUT-REC REPLACING ALL X'00' BY SPACES.
  ```

### 8. Formatting Rules
- Maximum **Column 72**.
- Break long statements across lines with proper indentation.

### 9. Paragraph Generation
- Break every logical piece into small, modular paragraphs using `PERFORM ... THRU ...-EXIT`.

### 10. Looping Logic – Priming Read (Mandatory)
```cobol
       0000-MAIN.
           PERFORM 1000-INITIALIZATION THRU 1000-EXIT.
           
           PERFORM 2000-PROCESS THRU 2000-EXIT
               UNTIL WS-EOF = 'Y'.
           
           PERFORM 9000-TERMINATION THRU 9000-EXIT.
           STOP RUN.

       2000-PROCESS.
           *> Process the guaranteed valid record
           PERFORM 3000-BUSINESS-LOGIC THRU 3000-EXIT.
           
           *> Read next record at the very end
           PERFORM 1100-READ-FILE THRU 1100-EXIT.
       2000-EXIT.
           EXIT.
```

### 11. VSAM Lookup Business Pattern (Standard Shop I/O Module)
```cobol
       0100-LOOKUP-RTN.
           MOVE SPACES TO LKUP-KEY-AREA.
           MOVE appropriate-control-values TO I-O-CONTROL fields.
           MOVE source-key TO LKUP-KEY.
           MOVE 'I' TO I-O-CONTROL-ACCESS.
           MOVE 'K' TO I-O-CONTROL-OPERATOR.
           CALL 'LOOKUP-MODULE' USING I-O-CONTROL-AREA,
                                      LKUP-RECORD-AREA.

           IF I-O-88-NOT-FOUND
               *> handle not found
           ELSE IF NOT I-O-88-NORMAL-RET
               DISPLAY 'LOOKUP ERROR ' I-O-RETURN-CODE
               MOVE 'Y' TO WS-ABEND-FLAG
           ELSE
               *> successful lookup – use returned data
           END-IF.
       0100-EXIT.
           EXIT.
```
Always perform Close call (`MOVE 'E' TO I-O-CONTROL-OPERATOR`) in `0900-TERMINATION`.

### 12. COPY Statement Rules
- Member names must be **8 characters or fewer**.
- Always start `WORKING-STORAGE SECTION` with:
  ```cobol
  COPY SIWSCNTL.          *> I-O control area + all 88-level conditions
  COPY STWSMS.            *> System messages / environment
  ```
- Use logical assumed names when exact name is unknown (add comment).

### 13. CALL Statement Rules
- Program names ≤ **8 characters**.
- Always use `CALL 'LITERAL'` for static subroutines.
- First parameter is almost always `I-O-CONTROL-AREA`.
- Check return status **immediately** using 88-levels from `SIWSCNTL`.
- Always perform Close (`'E'`) in termination paragraph.
