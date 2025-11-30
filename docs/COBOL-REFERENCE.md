# HOPPER - COBOL Language Reference

Complete reference of COBOL features supported by the HOPPER interpreter.

## Table of Contents

- [Data Division](#data-division)
- [Procedure Division](#procedure-division)
- [String Manipulation](#string-manipulation-cobol-7485)
- [File Operations](#file-operations)
- [Table Handling](#table-handling)
- [Subprograms](#subprograms-cobol-85)
- [Copybooks](#copybooks-cobol-74)
- [Screen Control](#screen-control-ibm-3270-extensions)
- [Figurative Constants](#figurative-constants)
- [Not Yet Implemented](#not-yet-implemented)

---

## Data Division

| Feature | Syntax | Status |
|---------|--------|--------|
| Level numbers | `01` to `49`, `66`, `77`, `88` | ✅ |
| PICTURE clause | `PIC X(10)`, `PIC 9(5)V99`, `PIC S9(4)` | ✅ |
| VALUE clause | `VALUE "text"`, `VALUE 123`, `VALUE ZEROS` | ✅ |
| REDEFINES | `05 VAR-B REDEFINES VAR-A` | ✅ |
| OCCURS | `OCCURS 10 TIMES`, `OCCURS 1 TO 100 DEPENDING ON` | ✅ |
| INDEXED BY | `OCCURS 10 INDEXED BY IDX-1` | ✅ |
| 88-level conditions | `88 IS-VALID VALUE "Y" "O".` | ✅ |
| VALUE THRU | `88 IN-RANGE VALUE 1 THRU 100.` | ✅ |
| WORKING-STORAGE | `WORKING-STORAGE SECTION.` | ✅ |
| FILE SECTION | `FD`, `SELECT`, `ASSIGN` | ✅ |
| USAGE | `COMP`, `BINARY`, `PACKED-DECIMAL` | ⚠️ Parsed |

---

## Procedure Division

### Basic Statements

| Statement | Syntax Example | Status |
|-----------|----------------|--------|
| DISPLAY | `DISPLAY "Hello" WS-VAR` | ✅ |
| ACCEPT | `ACCEPT WS-INPUT` | ✅ |
| MOVE | `MOVE X TO Y Z` | ✅ |
| INITIALIZE | `INITIALIZE WS-RECORD` | ✅ |
| SET | `SET idx TO 5`, `SET idx UP BY 1`, `SET cond TO TRUE` | ✅ |
| CONTINUE | `CONTINUE` | ✅ |

### Arithmetic

| Statement | Syntax Example | Status |
|-----------|----------------|--------|
| ADD | `ADD A B TO C GIVING D` | ✅ |
| SUBTRACT | `SUBTRACT A FROM B GIVING C` | ✅ |
| MULTIPLY | `MULTIPLY A BY B GIVING C` | ✅ |
| DIVIDE | `DIVIDE A INTO B GIVING C REMAINDER R` | ✅ |
| COMPUTE | `COMPUTE X = (A + B) * C / D ** 2` | ✅ |

### Control Flow

| Statement | Syntax Example | Status |
|-----------|----------------|--------|
| IF/ELSE | `IF cond THEN ... ELSE ... END-IF` | ✅ |
| EVALUATE | `EVALUATE var WHEN val ... END-EVALUATE` | ✅ |
| PERFORM | `PERFORM para`, `PERFORM n TIMES`, `PERFORM UNTIL`, `PERFORM VARYING` | ✅ |
| GO TO | `GO TO paragraph-name` | ✅ |
| EXIT | `EXIT`, `EXIT PARAGRAPH` | ✅ |
| STOP RUN | `STOP RUN` | ✅ |

---

## String Manipulation (COBOL-74/85)

| Statement | Syntax Example | Status |
|-----------|----------------|--------|
| STRING | `STRING a DELIMITED BY " " b DELIMITED BY SIZE INTO c` | ✅ |
| UNSTRING | `UNSTRING src DELIMITED BY ";" INTO a b c TALLYING cnt` | ✅ |
| INSPECT TALLYING | `INSPECT var TALLYING cnt FOR ALL "X"` | ✅ |
| INSPECT REPLACING | `INSPECT var REPLACING ALL "X" BY "Y"` | ✅ |
| INSPECT CONVERTING | `INSPECT var CONVERTING "abc" TO "ABC"` | ✅ |
| Reference Modification | `MOVE WS-VAR(1:5) TO WS-SUB` | ✅ |
| Subscripting | `MOVE TABLE-ITEM(I) TO WS-VAR` | ✅ |

### STRING Examples

```cobol
      * Concatenate first name and last name
       STRING WS-PRENOM DELIMITED BY SPACE
              " " DELIMITED BY SIZE
              WS-NOM DELIMITED BY SPACE
              INTO WS-NOM-COMPLET
              WITH POINTER WS-PTR
       END-STRING.

      * With overflow handling
       STRING A B C DELIMITED BY SIZE INTO RESULT
              ON OVERFLOW DISPLAY "Overflow!"
              NOT ON OVERFLOW DISPLAY "OK"
       END-STRING.
```

### UNSTRING Examples

```cobol
      * Parse CSV line
       UNSTRING WS-CSV-LINE DELIMITED BY ";" OR ","
               INTO WS-FIELD-1 WS-FIELD-2 WS-FIELD-3
               TALLYING IN WS-FIELD-COUNT
       END-UNSTRING.

      * With delimiter capture
       UNSTRING WS-SOURCE DELIMITED BY ALL SPACES
               INTO WS-WORD-1 DELIMITER IN WS-DELIM-1
                    WS-WORD-2 COUNT IN WS-LEN-2
       END-UNSTRING.
```

### INSPECT Examples

```cobol
      * Count occurrences
       MOVE 0 TO WS-COUNT.
       INSPECT WS-TEXT TALLYING WS-COUNT FOR ALL "A".

      * Replace characters
       INSPECT WS-TEXT REPLACING ALL "-" BY " ".
       INSPECT WS-TEXT REPLACING LEADING ZEROS BY SPACES.
       INSPECT WS-TEXT REPLACING FIRST "ERROR" BY "OK   ".

      * Convert case (uppercase)
       INSPECT WS-TEXT CONVERTING
               "abcdefghijklmnopqrstuvwxyz"
               TO "ABCDEFGHIJKLMNOPQRSTUVWXYZ".
```

### Reference Modification Examples

```cobol
      * Extract substring (position:length)
       MOVE WS-DATE(1:4) TO WS-YEAR.    *> First 4 chars
       MOVE WS-DATE(5:2) TO WS-MONTH.   *> Chars 5-6
       MOVE WS-DATE(7:2) TO WS-DAY.     *> Chars 7-8

      * With variable position
       MOVE WS-TEXT(WS-START:WS-LEN) TO WS-RESULT.
```

---

## File Operations

| Statement | Syntax Example | Status |
|-----------|----------------|--------|
| OPEN | `OPEN INPUT file`, `OPEN OUTPUT file`, `OPEN I-O file` | ✅ |
| CLOSE | `CLOSE file-1 file-2` | ✅ |
| READ | `READ file AT END ... NOT AT END ... END-READ` | ✅ |
| READ KEY | `READ file KEY IS key-field INVALID KEY ...` | ✅ |
| WRITE | `WRITE record-name` | ✅ |
| REWRITE | `REWRITE record-name` | ✅ |
| DELETE | `DELETE file-name INVALID KEY ...` | ✅ |
| START | `START file KEY >= key-field` | ✅ |
| SORT | `SORT file ON ASCENDING KEY ... USING ... GIVING ...` | ✅ |
| MERGE | `MERGE file ON ASCENDING KEY ... USING ... GIVING ...` | ✅ |
| RELEASE | `RELEASE sort-record` | ✅ |
| RETURN | `RETURN sort-file AT END ...` | ✅ |

### File Organizations

| Type | Status | Notes |
|------|--------|-------|
| SEQUENTIAL | ✅ | Line sequential access |
| INDEXED | ✅ | Key-based access via IndexedDB |
| RELATIVE | ⚠️ | Parsed, limited support |

---

## Table Handling

| Feature | Syntax Example | Status |
|---------|----------------|--------|
| SEARCH | `SEARCH table AT END ... WHEN cond ... END-SEARCH` | ✅ |
| SEARCH ALL | `SEARCH ALL table WHEN key = value ...` | ✅ |
| OCCURS | `OCCURS 100 TIMES` | ✅ |
| OCCURS DEPENDING ON | `OCCURS 1 TO 100 DEPENDING ON WS-SIZE` | ✅ |
| INDEXED BY | `INDEXED BY IDX-1` | ✅ |
| ASCENDING KEY | `ASCENDING KEY IS field-name` | ✅ |

### Table Examples

```cobol
       01 WS-PRODUCT-TABLE.
          05 WS-PRODUCT OCCURS 100 TIMES
             INDEXED BY PROD-IDX
             ASCENDING KEY IS WS-PROD-CODE.
             10 WS-PROD-CODE   PIC X(5).
             10 WS-PROD-NAME   PIC X(30).
             10 WS-PROD-PRICE  PIC 9(5)V99.

       PROCEDURE DIVISION.
      * Linear search
           SEARCH WS-PRODUCT
               AT END DISPLAY "Not found"
               WHEN WS-PROD-CODE(PROD-IDX) = "A001"
                   DISPLAY WS-PROD-NAME(PROD-IDX)
           END-SEARCH.

      * Binary search (table must be sorted)
           SEARCH ALL WS-PRODUCT
               AT END DISPLAY "Not found"
               WHEN WS-PROD-CODE(PROD-IDX) = WS-SEARCH-KEY
                   MOVE WS-PROD-PRICE(PROD-IDX) TO WS-PRICE
           END-SEARCH.
```

---

## Subprograms (COBOL-85)

| Statement | Syntax Example | Status |
|-----------|----------------|--------|
| CALL | `CALL "subprogram" USING arg1 arg2` | ✅ |
| CALL BY REFERENCE | `CALL "sub" USING BY REFERENCE var` | ✅ |
| CALL BY CONTENT | `CALL "sub" USING BY CONTENT var` | ✅ |
| CALL BY VALUE | `CALL "sub" USING BY VALUE var` | ✅ |
| ON EXCEPTION | `CALL ... ON EXCEPTION ... END-CALL` | ✅ |
| NOT ON EXCEPTION | `CALL ... NOT ON EXCEPTION ... END-CALL` | ✅ |
| CANCEL | `CANCEL "subprogram"` | ✅ |

### CALL Examples

```cobol
      * Call internal paragraph as subprogram
       CALL "CALCULATE-TAX" USING WS-AMOUNT WS-TAX.

      * Call with exception handling
       CALL "EXTERNAL-ROUTINE"
           ON EXCEPTION
               DISPLAY "Error: subprogram not found"
           NOT ON EXCEPTION
               DISPLAY "Call successful"
       END-CALL.

      * Different parameter passing modes
       CALL "PROCESS" USING
           BY REFERENCE WS-MODIFIABLE
           BY CONTENT WS-READ-ONLY
           BY VALUE WS-COPY
       END-CALL.
```

---

## Copybooks (COBOL-74)

| Feature | Syntax Example | Status |
|---------|----------------|--------|
| COPY | `COPY copybook-name.` | ✅ |
| COPY REPLACING | `COPY name REPLACING ==old== BY ==new==.` | ✅ |

### Built-in Copybooks

HOPPER includes several pre-registered copybooks:

| Copybook | Contents |
|----------|----------|
| `DATE-VARS` | Date/time variables (WS-YEAR, WS-MONTH, WS-DAY, etc.) |
| `SCREEN-CONTROL` | Screen positioning variables and attributes |
| `ERROR-HANDLING` | Error code and message fields with 88-levels |
| `FILE-STATUS` | File status codes with 88-level conditions |

### COPY Examples

```cobol
       DATA DIVISION.
       WORKING-STORAGE SECTION.
      * Include standard date variables
       COPY DATE-VARS.
      * Include file status with prefix replacement
       COPY FILE-STATUS REPLACING ==WS-FS== BY ==CUST-FS==.

       PROCEDURE DIVISION.
           MOVE 2024 TO WS-YEAR.
           MOVE 12 TO WS-MONTH.
           MOVE 25 TO WS-DAY.
           DISPLAY WS-YEAR "/" WS-MONTH "/" WS-DAY.
```

---

## Screen Control (IBM 3270 Extensions)

HOPPER supports IBM 3270 style screen control for interactive terminal applications.

### DISPLAY Options

| Option | Description |
|--------|-------------|
| `LINE n` | Row position (1-based) |
| `POSITION n` / `COLUMN n` / `COL n` | Column position (1-based) |
| `ERASE EOS` | Erase to End Of Screen |
| `ERASE EOL` | Erase to End Of Line |
| `ERASE SCREEN` | Clear entire screen |
| `HIGHLIGHT` | Bright/bold text |
| `BLINK` | Blinking text |
| `REVERSE-VIDEO` | Inverted colors |
| `UNDERLINE` | Underlined text |
| `BELL` / `BEEP` | Terminal beep |

### ACCEPT Options

| Option | Description |
|--------|-------------|
| `LINE n` | Row position for input |
| `POSITION n` / `COLUMN n` | Column position for input |
| `SECURE` | Password input (hidden) |
| Display attributes | HIGHLIGHT, REVERSE-VIDEO, etc. |

### Screen Control Examples

```cobol
      * Clear screen and position cursor
       DISPLAY " " LINE 1 POSITION 1 ERASE SCREEN.

      * Display with formatting
       DISPLAY "=== MENU ===" LINE 3 POSITION 30 HIGHLIGHT.
       DISPLAY "1. Option A" LINE 5 POSITION 25.
       DISPLAY "2. Option B" LINE 6 POSITION 25.

      * Input with positioning
       DISPLAY "Choice: " LINE 10 POSITION 25.
       ACCEPT WS-CHOICE LINE 10 POSITION 33.

      * Password entry
       DISPLAY "Password: " LINE 12 POSITION 25.
       ACCEPT WS-PASSWORD LINE 12 POSITION 35 SECURE.

      * Highlight and blink for warnings
       DISPLAY "WARNING!" LINE 20 POSITION 35 HIGHLIGHT BLINK.
```

---

## Figurative Constants

| Constant | Value | Usage |
|----------|-------|-------|
| SPACES / SPACE | `" "` | Fill with spaces |
| ZEROS / ZEROES / ZERO | `"0"` | Fill with zeros |
| LOW-VALUES | `X"00"` | Null characters |
| HIGH-VALUES | `X"FF"` | Maximum value characters |
| QUOTES | `"` | Quote character |

---

## Legend

| Symbol | Meaning |
|--------|---------|
| ✅ | Fully implemented |
| ⚠️ | Partially implemented or parsed only |
| ❌ | Not implemented |

---

## Not Yet Implemented

| Feature | Standard | Notes |
|---------|----------|-------|
| LINAGE | COBOL-74 | Print page control |
| WRITE ADVANCING | COBOL-68 | Printer line control |
| DECLARATIVES | COBOL-85 | Error handling procedures |
| ALTER | COBOL-68 | Dynamic GO TO modification (obsolete) |

---

## See Also

- [README.md](../README.md) - Project overview and quick start
- [COBOL Examples](../cobol-examples/) - Sample programs
