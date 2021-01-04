       IDENTIFICATION DIVISION.
     ‚*
         PROGRAM-ID.              CFP0013.
           COPY COPYRIGHT  OF APG-QCPYSRC.
     ‚*
     ‚*---------------------------------------------------------------*
     ‚* DISPLAY OHNE LOGIK ANZEIGEN                                   *
     ‚*---------------------------------------------------------------*
     ‚*                                                               *
     ‚* AUTOR         :  A. PIEGER                                    *
     ‚*                                                               *
     ‚* ERSTELLT AM   :  04.08.2010                                   *
     ‚*                                                               *
     ‚* FUNKTION      :  DISPLAY ANZEIGEN                             *
     ‚*                                                               *
     ‚* ÄNDERUNGEN:                                                   *
     ‚* DATUM      VON   GRUND DER ÄNDERUNG                           *
     ‚*                                                               *
     ‚*****************************************************************
     ‚*
       ENVIRONMENT DIVISION.
        CONFIGURATION SECTION.
        SPECIAL-NAMES. , DECIMAL-POINT IS COMMA.
      /
       INPUT-OUTPUT SECTION.
         FILE-CONTROL.

     ‚*-------------------------------------------------------------
     ‚*-   BILDSCHIRM-DATEI
     ‚*-------------------------------------------------------------
           SELECT WS-DISPLAY
                  ASSIGN       TO  WORKSTATION-CFD0013DE
                  ORGANIZATION IS  TRANSACTION
                  CONTROL-AREA IS  WS-CONTROL
                  ACCESS       IS  DYNAMIC
                  RELATIVE KEY IS  SUBFILE-RECORD-NUMBER
                  FILE STATUS  IS  BS-STAT.

     ‚*-------------------------------------------------------------
     ‚* LOGISCHE DATEI PCFGFFD NACH LIB/FILE
     ‚*-------------------------------------------------------------
           SELECT PCFGFFD-DL
                  ASSIGN       TO  DATABASE-PCFGFFDL1
                  ORGANIZATION IS  INDEXED
                  RECORD KEY   IS  EXTERNALLY-DESCRIBED-KEY
                                   WITH DUPLICATES
                  ACCESS       IS  DYNAMIC
                  FILE STATUS  IS  FILE-STATUS.
      /
     ‚*
       DATA DIVISION.
       FILE SECTION.

     ‚*-----BILDSCHIRMDATEI -------------------------
       FD   WS-DISPLAY
           LABEL RECORDS ARE OMITTED.
       01  WS-REC                        PIC X(4096).
      /
     ‚*--- PCFGFFD NACH LIB/FILE
       FD  PCFGFFD-DL
           LABEL RECORDS ARE STANDARD.
       01  PCFGFFD-L.
           COPY DDS-ALL-FORMATS OF PCFGFFDL1.
      /
       WORKING-STORAGE SECTION.

     ‚* STANDARDFELDER
           COPY WRKSTD      OF APG-QCPYSRC.

     ‚* STANDARDFELDER DISPLAYHANDLING
           COPY WRKDISPLAY  OF APG-QCPYSRC.

     ‚* STANDARDFELDER CL-PROGRAMME
           COPY WRKPGMPARM OF APG-QCPYSRC.

       01  PGM-WRK                       PIC X(10) VALUE "CFP0013".
       01  PGM-TYP                       PIC X(8)  VALUE "CONFIG".

     ‚* PARAMETER FÜR CL-PROGRAMME
       01  RTVOBJD-REC                   TYPE RTVOBJD-TYPE.

       01  POS                           PIC 9(4).
       01  LEN                           PIC 9(4).
     ‚*-------------------------------------------------------
       LINKAGE SECTION.
       01  X-DISPLAY                     PIC X(10).
       01  X-FORMAT                      PIC X(10).
       01  X-SUBFILE                     PIC X(1).
     ‚*-------------------------------------------------------

     ‚*-------------------------------------------------------
       PROCEDURE DIVISION USING X-DISPLAY
                                X-FORMAT
                                X-SUBFILE.
     ‚*-------------------------------------------------------
       STEUER SECTION.
       ANFANG.

           IF       FIRST-CALL NOT = "1"
                    MOVE "1" TO FIRST-CALL
                    OPEN     INPUT PCFGFFD-DL
           END-IF.

     ‚* DISPLAY ÖFFNEN
           MOVE     "*LIBL"     TO CONST-LIB.
           MOVE     X-DISPLAY   TO DISPLAY-WRK.
           PERFORM  COPY-DISPLAY-OPEN.
           IF       BS-STAT NOT = ZEROES
                    GO TO ENDE
           END-IF.

     ‚* BIBLIOTHEK DES DISPLAYS
           PERFORM  GET-LIBRARY.

           IF       X-SUBFILE = "0"
                    MOVE     X-FORMAT        TO  FMT-WRK
                    MOVE     SPACES          TO  WSREC-WRK
                    PERFORM  FILL-WSREC-DEFAULT
                    PERFORM  COPY-DISPLAY-WRITE
                    PERFORM  COPY-DISPLAY-READ
           ELSE
     ‚*             FUNKTIONSTASTEN
                    MOVE     SPACES  TO WSREC-WRK
                    MOVE     "FMT04" TO FMT-WRK
                    PERFORM  FILL-WSREC-DEFAULT
                    PERFORM  COPY-DISPLAY-WRITE
     ‚*             FEHLERMELDUNG
                    MOVE     SPACES  TO WSREC-WRK
                    MOVE     "FMT05" TO FMT-WRK
                    PERFORM  FILL-WSREC-DEFAULT
                    PERFORM  COPY-DISPLAY-WRITE
     ‚*             SUBFILE-CONTROL
                    MOVE     "FMT03"        TO FMT-WRK
                    MOVE     SPACES         TO WSREC-WRK
                    PERFORM  FILL-WSREC-DEFAULT
                    PERFORM  COPY-DISPLAY-WRITE
                    PERFORM  COPY-DISPLAY-READ
           END-IF.

       ENDE.
     ‚* DISPLAY SCHLIEßEN
           PERFORM  COPY-DISPLAY-CLOSE.

           GOBACK.
      /
     ‚*--------------------------------------------------------------
     ‚* WSREC FÜLLEN
     ‚*--------------------------------------------------------------
       FILL-WSREC-DEFAULT SECTION.
       FIL-WSD-00.

     ‚* ERSTMAL DIE PCFGDDF (PXMLFFD) FÜLLEN
           CALL     "CMP0005" USING RTNLIB OF RTVOBJD-REC
                                    DISPLAY-WRK
           END-CALL.

     ‚* LESEN ALLER DATUMSFELDER
           INITIALIZE PCFGFFDF OF PCFGFFD-L.
           MOVE     RTNLIB OF RTVOBJD-REC TO WHLIB  OF PCFGFFD-L.
           MOVE     DISPLAY-WRK           TO WHFILE OF PCFGFFD-L.
           MOVE     FMT-WRK               TO WHNAME OF PCFGFFD-L.
           MOVE     SPACES                TO WHFLDE OF PCFGFFD-L.
           START    PCFGFFD-DL KEY NOT < EXTERNALLY-DESCRIBED-KEY.
           IF       FILE-STATUS NOT = ZEROES
                    GO TO FIL-WSD-90
           END-IF.

       FIL-WSD-20.
           READ     PCFGFFD-DL NEXT RECORD.
           IF       FILE-STATUS NOT = ZEROES
                    GO TO FIL-WSD-90
           END-IF.

           IF       WHLIB  OF PCFGFFD-L NOT = RTNLIB OF RTVOBJD-REC
            OR      WHFILE OF PCFGFFD-L NOT = DISPLAY-WRK
            OR      WHNAME OF PCFGFFD-L NOT = FMT-WRK
                    GO TO FIL-WSD-90
           END-IF.

           MOVE     WHFOBO OF PCFGFFD-L TO POS.
           MOVE     WHFLDB OF PCFGFFD-L TO LEN.
     ‚* FELD FÜLLEN
           EVALUATE WHFLDT OF PCFGFFD-L
                    WHEN "S" MOVE ZEROES TO WSREC-WRK(POS:LEN)
                    WHEN "A" IF   WHFLDE OF PCFGFFD-L(1:1) NOT = "*"
                                  PERFORM  FILL-DEF-LFS
                             END-IF
           END-EVALUATE.

           IF       X-SUBFILE = "1"
                    IF   WHFLDE OF PCFGFFD-L = "*IN96"
                         MOVE AN TO WSREC-WRK(POS:LEN)
                    END-IF
           END-IF.

           GO TO    FIL-WSD-20.

       FIL-WSD-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* JE NACH FELDNAMEN GGF. LFS-WERTE ÜBERNEHMEN
     ‚*--------------------------------------------------------------
       FILL-DEF-LFS SECTION.
       FIL-DFL-00.

           EVALUATE WHFLDE OF PCFGFFD-L
                    WHEN "KONZNR" MOVE "999"      TO WSREC-WRK(POS:LEN)
                    WHEN "FIRMNR" MOVE "099"      TO WSREC-WRK(POS:LEN)
                    WHEN "KONZBZ"
                         MOVE "Konzern/NL"        TO WSREC-WRK(POS:LEN)
                    WHEN "FIRMBZ"
                         MOVE "Test-Firma"        TO WSREC-WRK(POS:LEN)
                    WHEN "PGM0990"
                         MOVE "Lagerführung und Materialflusssteuerung"
                              TO WSREC-WRK(POS:LEN)
                    WHEN OTHER
                         MOVE WHFLDE OF PCFGFFD-L TO WSREC-WRK(POS:LEN)
           END-EVALUATE .

       FIL-DFL-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* BIBLIOTHEK ERMITTELN
     ‚*--------------------------------------------------------------
       GET-LIBRARY SECTION.
       GET-LIB-00.

           INITIALIZE RTVOBJD-REC.
           MOVE     DISPLAY-WRK  TO OBJ     OF RTVOBJD-REC.
           MOVE     "*LIBL"      TO OBJLIB  OF RTVOBJD-REC.
           MOVE     "*FILE"      TO OBJTYPE OF RTVOBJD-REC.
           CALL     "CLP0006" USING RTVOBJD-REC
                                    RET-CODE
           END-CALL.

       GET-LIB-90.
           EXIT.
      /
     ‚*---------------------------------------------------------------
     ‚* COPY-DSP-*: ALLE DISPLAYROUTINEN
     ‚*---------------------------------------------------------------

           COPY     DSPWORKING OF APG-QCPYSRC.
      /
