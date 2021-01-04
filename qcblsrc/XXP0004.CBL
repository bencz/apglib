       IDENTIFICATION DIVISION.
     ‚*
         PROGRAM-ID.              XXP0004.
           COPY COPYRIGHT  OF APG-QCPYSRC.
     ‚*
     ‚*---------------------------------------------------------------*
     ‚* AUSFÜHREN DER COMMAND-ROUTINEN                                *
     ‚*---------------------------------------------------------------*
     ‚*                                                               *
     ‚* AUTOR         :  A. PIEGER                                    *
     ‚*                                                               *
     ‚* ERSTELLT AM   :  09.02.2010                                   *
     ‚*                                                               *
     ‚* FUNKTION      :  AUSFÜHREN APG-PROGRAMME                      *
     ‚*                                                               *
     ‚* ÄNDERUNGEN:                                                   *
     ‚* DATUM      VON   GRUND DER ÄNDERUNG                           *
     ‚*                                                               *
     ‚*****************************************************************
     ‚*
       ENVIRONMENT DIVISION.
        CONFIGURATION SECTION.
         SOURCE-COMPUTER.         IBM-AS400.
         OBJECT-COMPUTER.         IBM-AS400.
        SPECIAL-NAMES. , DECIMAL-POINT IS COMMA.
      /
       INPUT-OUTPUT SECTION.
         FILE-CONTROL.

      /
       DATA DIVISION.
       FILE SECTION.
      /
       WORKING-STORAGE SECTION.

     ‚* STANDARDFELDER
           COPY WRKSTD     OF APG-QCPYSRC.

     ‚* WORKBEREICH ÜBERGABEPARAMETER
           COPY WRKPGMPARM OF APG-QCPYSRC.

     ‚* WORKBEREICH CFG-CPY
           COPY WRKCFGCPY  OF APG-QCPYSRC.

       01  PGM-WRK                       PIC X(10) VALUE "XXP0004".
       01  PGM-TYP                       PIC X(8)  VALUE "TEST".

     ‚* PARAMETER FÜR CL-PROGRAMME
       01  CHKOBJ-REC                    TYPE CHKOBJ-TYPE.

       01  PARM-WRK                      PIC X(1027).
       01  PARM-TMP                      PIC X(1027).
       01  PARM-LEN                      PIC 9(4).
       01  PARM-TYP                      PIC X(1).
       01  GET-PARM-WRK                  PIC X(4).

       01  POS                           PIC 9(4).
       01  POS2                          PIC 9(4).

     ‚* PARAMETER FÜR DAS HLP0008
       01  CALL-PGM                      PIC X(10).
       01  CALL-PARM                     PIC X(1027).
       01  BATCH-KZ                      PIC X(1) VALUE SPACES.
       01  F3                            PIC X(1).
       01  F12                           PIC X(1).
      /
     ‚*-------------------------------------------------------------
       LINKAGE SECTION.
       01  X-CALLPGM                     PIC X(10).
       01  X-PARM-01                     PIC X(50).
       01  X-PARM-02                     PIC X(50).
       01  X-PARM-03                     PIC X(50).
       01  X-PARM-04                     PIC X(50).
       01  X-PARM-05                     PIC X(50).
       01  X-PARM-06                     PIC X(50).
       01  X-PARM-07                     PIC X(50).
       01  X-PARM-08                     PIC X(50).
       01  X-PARM-09                     PIC X(50).
       01  X-PARM-10                     PIC X(50).
     ‚*-------------------------------------------------------------
       PROCEDURE DIVISION USING X-CALLPGM
                                X-PARM-01
                                X-PARM-02
                                X-PARM-03
                                X-PARM-04
                                X-PARM-05
                                X-PARM-06
                                X-PARM-07
                                X-PARM-08
                                X-PARM-09
                                X-PARM-10.
     ‚*-------------------------------------------------------------
       STEUER SECTION.
       ANFANG.

     ‚* FÜLLEN PGM-WRK UND AUSFÜHREN STANDARDROUTINEN
           MOVE     "XXP0004" TO PGM-WRK.
           PERFORM  COPY-PGM-INIT-SIMPLE.

     ‚* KONFIGURATION ERMITTELN
           INITIALIZE PCONFIGF OF CFG-CPY.
           MOVE     "A200"              TO CFID   OF CFG-CPY.
           MOVE     X-CALLPGM           TO CFKEY  OF CFG-CPY.
           PERFORM  COPY-CFG-CPY.
           IF       CFG-DA OF CFG-CPY = ZEROES
                    GO TO ENDE
           END-IF.

     ‚* PARAMETER ÜBERNEHMEN UND PARAMETER-TABELLE FÜLLEN
           MOVE     SPACES              TO CALL-PARM.
           MOVE     CFDAT1 OF CFG-CPY   TO CALL-PGM.
           MOVE     CFBG01 OF CFG-CPY   TO PARM-WRK.
           PERFORM  CHECK-PARAMETER.

     ‚* JETZT PRÜFEN OB ES DAS PGM AUCH GIBT
           PERFORM  CHECK-OBJECT.
           IF       RET-CODE NOT = SPACES
                    GO TO ENDE
           END-IF.

           INITIALIZE F3, F12.
     ‚* PROGRAMM AUFRUFEN
           CALL     "HLP0008" USING CALL-PGM
                                    CALL-PARM
                                    BATCH-KZ
                                    F3
                                    F12
           END-CALL.

       ENDE.
           PERFORM  COPY-PGM-EXIT-SIMPLE.

           GOBACK.
      /
     ‚*--------------------------------------------------------------
     ‚* SETZEN DES PARAMETER-STRINGS
     ‚*--------------------------------------------------------------
       SET-PARAMETER SECTION.
       SET-PRM-00.

     ‚* LÄNGE ERMITTELN
           MOVE     LENGTH OF CALL-PARM TO POS2.
           PERFORM  WITH TEST BEFORE UNTIL POS2 = ZEROES
                     OR CALL-PARM(POS2:1)   NOT = SPACES
                    SUBTRACT 1 FROM POS2
           END-PERFORM.
           IF       CALL-PARM NOT = SPACES
                    ADD      1            TO POS2
           END-IF.

     ‚* STARTANFÜHRUNGSZEICHEN
           MOVE     "'"                   TO CALL-PARM(POS2 + 1:1).
           ADD      1                     TO POS2.

           MOVE     PARM-TMP(1:PARM-LEN)  TO CALL-PARM(POS2 + 1:).
           ADD      PARM-LEN              TO POS2.

           MOVE     "'"                   TO CALL-PARM(POS2 + 1:1).
           ADD      1                     TO POS2.

       SET-PRM-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* ERMITTELN DES PARAMETERTYPEN
     ‚*--------------------------------------------------------------
       GET-PARM-TYPE SECTION.
       GET-PTY-00.

           MOVE     SPACES TO PARM-TYP.
           MOVE     ZEROES TO PARM-LEN.
           MOVE     SPACES TO PARM-TMP.

           MOVE     1      TO POS.
     ‚* FORMAT: '&1:A:10' => PARAMETER 1, ALPHANUMIERSCH, 10 STELLEN
           PERFORM  WITH TEST BEFORE UNTIL POS > LENGTH OF PARM-WRK
                     OR PARM-WRK(POS:) = SPACES
     ‚*             TYP UND POSITION SICHERN
                    IF   PARM-WRK(POS:2) = GET-PARM-WRK
                         MOVE PARM-WRK(POS + 3:1) TO PARM-TYP
                         IF   PARM-WRK(POS + 5:2) IS NUMERIC
                              MOVE PARM-WRK(POS + 5:2) TO PARM-LEN
                         END-IF
                    END-IF
     ‚*             F3/F12 ERSTELLEN
                    IF   PARM-WRK(POS:3) = GET-PARM-WRK
                     OR  PARM-WRK(POS:4) = GET-PARM-WRK
                         MOVE 1 TO PARM-LEN
                    END-IF
                    ADD 1 TO POS
           END-PERFORM.

       GET-PTY-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* PARAMETER-TABELLE FÜLLEN
     ‚*--------------------------------------------------------------
       CHECK-PARAMETER SECTION.
       CHK-PAR-00.

     ‚* WURDE EIN PARAMETER ÜBERGEBEN?
           IF       ADDRESS OF X-PARM-01 NOT = NULL
                    MOVE    "&1" TO GET-PARM-WRK
                    PERFORM GET-PARM-TYPE
     ‚*             PARAMETER SETZEN
                    IF   PARM-LEN NOT = ZEROES
                         MOVE    X-PARM-01(1:PARM-LEN) TO PARM-TMP
                         PERFORM SET-PARAMETER
                    END-IF
           END-IF.
           IF       ADDRESS OF X-PARM-02 NOT = NULL
                    MOVE    "&2" TO GET-PARM-WRK
                    PERFORM GET-PARM-TYPE
     ‚*             PARAMETER SETZEN
                    IF   PARM-LEN NOT = ZEROES
                         MOVE    X-PARM-02(1:PARM-LEN) TO PARM-TMP
                         PERFORM SET-PARAMETER
                    END-IF
           END-IF.
           IF       ADDRESS OF X-PARM-03 NOT = NULL
                    MOVE    "&3" TO GET-PARM-WRK
                    PERFORM GET-PARM-TYPE
     ‚*             PARAMETER SETZEN
                    IF   PARM-LEN NOT = ZEROES
                         MOVE    X-PARM-03(1:PARM-LEN) TO PARM-TMP
                         PERFORM SET-PARAMETER
                    END-IF
           END-IF.
           IF       ADDRESS OF X-PARM-04 NOT = NULL
                    MOVE    "&4" TO GET-PARM-WRK
                    PERFORM GET-PARM-TYPE
     ‚*             PARAMETER SETZEN
                    IF   PARM-LEN NOT = ZEROES
                         MOVE    X-PARM-04(1:PARM-LEN) TO PARM-TMP
                         PERFORM SET-PARAMETER
                    END-IF
           END-IF.
           IF       ADDRESS OF X-PARM-05 NOT = NULL
                    MOVE    "&5" TO GET-PARM-WRK
                    PERFORM GET-PARM-TYPE
     ‚*             PARAMETER SETZEN
                    IF   PARM-LEN NOT = ZEROES
                         MOVE    X-PARM-05(1:PARM-LEN) TO PARM-TMP
                         PERFORM SET-PARAMETER
                    END-IF
           END-IF.
           IF       ADDRESS OF X-PARM-06 NOT = NULL
                    MOVE    "&6" TO GET-PARM-WRK
                    PERFORM GET-PARM-TYPE
     ‚*             PARAMETER SETZEN
                    IF   PARM-LEN NOT = ZEROES
                         MOVE    X-PARM-06(1:PARM-LEN) TO PARM-TMP
                         PERFORM SET-PARAMETER
                    END-IF
           END-IF.
           IF       ADDRESS OF X-PARM-07 NOT = NULL
                    MOVE    "&7" TO GET-PARM-WRK
                    PERFORM GET-PARM-TYPE
     ‚*             PARAMETER SETZEN
                    IF   PARM-LEN NOT = ZEROES
                         MOVE    X-PARM-07(1:PARM-LEN) TO PARM-TMP
                         PERFORM SET-PARAMETER
                    END-IF
           END-IF.
           IF       ADDRESS OF X-PARM-08 NOT = NULL
                    MOVE    "&8" TO GET-PARM-WRK
                    PERFORM GET-PARM-TYPE
     ‚*             PARAMETER SETZEN
                    IF   PARM-LEN NOT = ZEROES
                         MOVE    X-PARM-08(1:PARM-LEN) TO PARM-TMP
                         PERFORM SET-PARAMETER
                    END-IF
           END-IF.
           IF       ADDRESS OF X-PARM-09 NOT = NULL
                    MOVE    "&9" TO GET-PARM-WRK
                    PERFORM GET-PARM-TYPE
     ‚*             PARAMETER SETZEN
                    IF   PARM-LEN NOT = ZEROES
                         MOVE    X-PARM-09(1:PARM-LEN) TO PARM-TMP
                         PERFORM SET-PARAMETER
                    END-IF
           END-IF.

     ‚* F3-TASTE
           MOVE     "&F3" TO GET-PARM-WRK.
           PERFORM  GET-PARM-TYPE.
           IF       PARM-LEN NOT = ZEROES
                    MOVE    "&F3"  TO PARM-TMP
                    PERFORM SET-PARAMETER
           END-IF.

     ‚* F3-TASTE
           MOVE     "&F12" TO GET-PARM-WRK.
           PERFORM  GET-PARM-TYPE.
           IF       PARM-LEN NOT = ZEROES
                    MOVE    "&F12" TO PARM-TMP
                    PERFORM SET-PARAMETER
           END-IF.

       CHK-PAR-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* PRÜFEN OB ES DAS PROGRAMM GIBT
     ‚*--------------------------------------------------------------
       CHECK-OBJECT SECTION.
       CHK-OBJ-00.

           INITIALIZE RET-CODE.
     ‚* PRÜFEN OB OBJEKT VORHANDEN IST
           INITIALIZE CHKOBJ-REC.
           MOVE     CALL-PGM     TO OBJ     OF CHKOBJ-REC.
           MOVE     "*LIBL"      TO LIB     OF CHKOBJ-REC.
           MOVE     "*PGM"       TO OBJTYPE OF CHKOBJ-REC.
           CALL     "CLP0004" IN LIBRARY CONST-LIB
                              USING CHKOBJ-REC
           END-CALL.
           IF       OBJ-EXIST OF CHKOBJ-REC = ZEROES
                    MOVE "1" TO RET-CODE
                    GO TO CHK-OBJ-90
           END-IF.

       CHK-OBJ-90.
           EXIT.
      /
     ‚*---------------------------------------------------------------
     ‚* COPY-CFG-CPY: LESEN DER KONFIGURATION
     ‚*---------------------------------------------------------------

           COPY     CFGCPY     OF APG-QCPYSRC.
      /
     ‚*---------------------------------------------------------------
     ‚* COPY-PGM-INIT/COPY-PGM-EXIT: STANDARDROUTINEN START UND ENDE
     ‚*---------------------------------------------------------------

           COPY     PGMWRK     OF APG-QCPYSRC.
      /
