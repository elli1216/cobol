# COBOL File Definitions: Fixed (FB) vs. Variable (VB)
**Cheat Sheet for Easytrieve → IBM Enterprise COBOL / GnuCOBOL Migration**  
**Version:** 1.1 (March 2026)  

---

## Quick Comparison

| Feature          | **FB (Fixed Block)**                          | **VB (Variable Block)**                          |
|------------------|-----------------------------------------------|--------------------------------------------------|
| **Analogy**      | A wall of identical bricks                    | A mailbag of different-sized letters             |
| **Structure**    | Every record = **exact same length**          | Records vary (RDW handled by OS)                 |
| **COBOL Mode**   | `RECORDING MODE IS F`                         | `RECORDING MODE IS V`                            |
| **Efficiency**   | Faster I/O (predictable)                      | Saves DASD space (no padding)                    |
| **EZT Syntax**   | `FILE xxx FB(80 800)`                         | `FILE xxx VB(18054 27998)`                       |

---

## How to Identify in Easytrieve

```easytrieve
FILE MYFILE FB(80 800)          *> Fixed Block
FILE STACTMI VB(18054 27998)    *> Variable Block
FILE VSAMFILE VS                *> VSAM (never use RECORDING MODE)
```

---

## COBOL Implementation Guide

### Scenario A: Fixed Block (FB)
```cobol
       FD  INPUT-FILE
           RECORDING MODE IS F
           LABEL RECORDS ARE STANDARD
           BLOCK CONTAINS 0 RECORDS
           RECORD CONTAINS 80 CHARACTERS
           DATA RECORDS ARE INPUT-REC.
       01  INPUT-REC                      PIC X(80).
```

### Scenario B: Variable Block (VB)
```cobol
       FD  STACTMI-FILE
           RECORDING MODE IS V
           LABEL RECORDS ARE STANDARD
           BLOCK CONTAINS 0 RECORDS
           RECORD IS VARYING IN SIZE FROM 1 TO 18054
               DEPENDING ON WS-REC-LEN
           DATA RECORDS ARE STACTMI-REC.

       01  STACTMI-REC                    PIC X(18054).
       01  WS-REC-LEN                     PIC S9(4) COMP.   *> WORKING-STORAGE
```

### Scenario C: VSAM (Most Common in Banks)
```cobol
       SELECT VSAM-FILE ASSIGN TO STBCRM
           ORGANIZATION IS INDEXED
           ACCESS MODE IS SEQUENTIAL          *> or DYNAMIC
           RECORD KEY IS BC1-CTL-KEY
           FILE STATUS IS WS-VSAM-STATUS.

       FD  VSAM-FILE
           RECORD CONTAINS 1700 CHARACTERS
           DATA RECORDS ARE ST-BCR-1.
```

> [!IMPORTANT]  
> **Never** put `RECORDING MODE` on VSAM files.  
> **Never** define lookup files (`*LKP*` in JCL) in `FILE-CONTROL` — use `CALL` only.

---

## Expanded Troubleshooting Table (All Common Migration Errors)

| Error / Abend          | Meaning                                      | Typical EZT → COBOL Cause                              | Fix |
|------------------------|----------------------------------------------|--------------------------------------------------------|-----|
| **S013**               | Record length mismatch                       | FD size ≠ actual dataset LRECL                         | Match `RECORD CONTAINS` exactly to EZT layout |
| **Status 35**          | File not found / Open failed                 | Wrong DDNAME in JCL or missing dataset                 | Verify JCL `//DDNAME DD DSN=...` |
| **Status 39**          | Attribute mismatch                           | `RECORDING MODE F` on a VB file                        | Use `V` for VB files |
| **Status 23**          | Record not found (VSAM)                      | Random READ without START or wrong key                 | Use `START` + `READ NEXT` or check key |
| **Status 97**          | VSAM file not closed properly last run       | Previous job abended                                   | Run `IDCAMS VERIFY` on the cluster |
| **Status 71**          | (GnuCOBOL/PC only) Nulls in record           | Uninitialized fields from EZT                          | `MOVE SPACES TO REC` + `INSPECT ... REPLACING ALL X'00' BY SPACES` |
| **IGYDS1089-S**        | RECORDING MODE in wrong place                | Put `RECORDING MODE` in `SELECT` instead of `FD`       | Move to FD |
| **IGYPS0086-I**        | EXIT PARAGRAPH used                          | Copied old code                                        | Use `GO TO xxx-EXIT` pattern |
| **Severe Error**       | `WRITE ... FROM SPACES`                      | Literal SPACES not allowed                             | Use `01 WS-BLANK-LINE PIC X(132) VALUE SPACES` |
| **ERROR_TOKEN**        | Missing period (.)                           | Previous line has no `.`                               | Add period to line above |
| **IGYPAxxxx**          | COPY book not found                          | Member name > 8 chars or wrong PDS                     | Use 8-char name (e.g. `STWSBCB1`) |
| **SOC4 / Protection**  | Data exception / subscript out of range      | Uninitialized fields or OCCURS without index init      | `MOVE SPACES TO ALL` in initialization |
| **Double-quote error** | Invalid character (")                        | Used `" "` with `APOST` compiler option                | Change all to single quotes `' '` |
| **IGZ0037S**          | A severe COBOL runtime error indicating that the program's control flow exceeded the final instruction, typically caused by a missing GOBACK or STOP RUN statement.  | The code execution reached the end of the PROCEDURE DIVISION without a proper exit command.  | Add GOBACK or STOP RUN to the end of the program. |
| **IGYSC1088**          | Continuation error                           | Literal split without `-` in column 7                 | Use `-` in col 7 for continuation |
| **No FILE STATUS**     | Silent I/O failures                          | Forgot to declare/check FILE STATUS                    | Always declare + check after every I/O |

---

## Common Logic & Syntax Fixes

### 1. EXIT PARAGRAPH Trap (Most Common Copy-Paste Error)
```cobol
*> WRONG (falls through!)
IF ERROR-FOUND
   EXIT PARAGRAPH.
END-IF.

*> CORRECT
IF ERROR-FOUND
   GO TO 2100-EXIT.
END-IF.
...
2100-EXIT.
   EXIT.
```

### 2. Writing Blank Lines (FB vs VB)
```cobol
*> For 132-column report (FB)
01  WS-BLANK-LINE              PIC X(132) VALUE SPACES.

WRITE RPT-REC FROM WS-BLANK-LINE AFTER ADVANCING 1 LINE.

*> For VB file
01  WS-BLANK-VB                PIC X(18054) VALUE SPACES.   *> or just PIC X(1)
```

### 3. Missing Periods = Silent Killer
Always check the **line above** the compiler error.

### 4. Priming Read (EZT JOB INPUT equivalent)
```cobol
       0000-MAIN.
           PERFORM 1000-INITIALIZATION THRU 1000-EXIT.
           PERFORM 2000-PROCESS THRU 2000-EXIT
               UNTIL WS-EOF = 'Y'.
           PERFORM 9000-TERMINATION THRU 9000-EXIT.
           STOP RUN.
```

---

## Best Practices Checklist (Tick before Compile)

- [ ] All files explicitly `OPEN` and `CLOSE`
- [ ] Lookup files (`*LKP*`) handled only via `CALL`
- [ ] Single quotes everywhere
- [ ] `WS-BLANK-LINE` for every report
- [ ] Priming Read pattern used
- [ ] `PERFORM xxx THRU xxx-EXIT` everywhere
- [ ] Column 72 limit respected
- [ ] `FILE STATUS` declared and checked after every I/O
- [ ] `STOP RUN` only in `0000-MAIN`
