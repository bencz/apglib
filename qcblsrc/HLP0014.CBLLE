       IDENTIFICATION DIVISION.
     ‚*
         PROGRAM-ID.             HLP0014.
           COPY COPYRIGHT  OF APG-QCPYSRC.
     ‚*
     ‚*---------------------------------------------------------------*
     ‚* AKTIVIERUNGSGRUPPE ANZEIGEN (SOFERN NICHT *CALLER)            *
     ‚*---------------------------------------------------------------*
     ‚*                                                               *
     ‚* AUTOR         :  A. PIEGER                                    *
     ‚*                                                               *
     ‚* ERSTELLT AM   :  09.02.2010                                   *
     ‚*                                                               *
     ‚* FUNKTION      :  AKTIVIERUNGSGRUPPE ANZEIGEN                  *
     ‚*                                                               *
     ‚* ÄNDERUNGEN:                                                   *
     ‚* DATUM      VON   GRUND DER ÄNDERUNG                           *
     ‚*                                                               *
     ‚*****************************************************************
     ‚*
       ENVIRONMENT DIVISION.
        CONFIGURATION SECTION.
        SPECIAL-NAMES. , DECIMAL-POINT IS COMMA.

       INPUT-OUTPUT SECTION.
         FILE-CONTROL.
      /
     ‚*
       DATA DIVISION.
       FILE SECTION.

       WORKING-STORAGE SECTION.

     ‚* STANDARDFELDER
           COPY WRKSTD     OF APG-QCPYSRC.

     ‚* STANDARDFELDER CL-PROGRAMME
           COPY WRKPGMPARM OF APG-QCPYSRC.

       01  PGM-WRK                       PIC X(10) VALUE "HLP0014 ".
       01  PGM-TYP                       PIC X(8)  VALUE "HILFE".

       01  RCVVAR                        PIC X(1000).
       01  RCVVAR-LEN                    PIC 9(9) BINARY VALUE 1000.
       01  RCVVAR-FMT                    PIC X(8) VALUE "PGMI0100".
       01  PGM-NAME                      PIC X(20).
       01  ERR-WRK                       PIC X(8) VALUE ALL X"00".
       01  ACTGRP                        PIC X(10).
      /
     ‚*-------------------------------------------------------
       LINKAGE SECTION.
     ‚*-------------------------------------------------------
       01 X-PGM                             PIC X(10).
       01 X-LIB                             PIC X(10).
     ‚*-------------------------------------------------------
       PROCEDURE DIVISION USING      X-PGM
                                     X-LIB.
     ‚*-------------------------------------------------------
       STEUER SECTION.
       ANFANG.

     ‚* WENN SOURCE IN DER SRC STEHT, DANN PGM ALS LIB NEHMEN
           IF       X-LIB(4:3) = "SRC"
                    MOVE "PGM" TO X-LIB(4:3)
           END-IF.

     ‚* GIBT ES DAS PROGRAMM?
           PERFORM  CHECK-TEILDATEI.
           IF       RET-CODE NOT = SPACES
                    GO TO ENDE
           END-IF.

           MOVE     X-PGM TO PGM-NAME(1:10).
           MOVE     X-LIB TO PGM-NAME(11:10).

           CALL     "QCLRPGMI"  USING RCVVAR
                                      RCVVAR-LEN
                                      RCVVAR-FMT
                                      PGM-NAME
                                      ERR-WRK
           END-CALL.
           IF       RCVVAR(369:10) NOT = "*CALLER"
                    DISPLAY X-PGM " " X-LIB " HAT "
                            RCVVAR(369:10)
           END-IF.

         ENDE.

           GOBACK.
      /
     ‚*--------------------------------------------------------------
     ‚* PRÜFEN OB ES DIE DATEI GIBT
     ‚*--------------------------------------------------------------
       CHECK-TEILDATEI SECTION.
       CHK-TED-00.

           INITIALIZE RET-CODE.
     ‚* PRÜFEN OB OBJEKT VORHANDEN IST
           INITIALIZE CHKOBJ-REC.
           MOVE     X-LIB        TO LIB     OF CHKOBJ-REC.
           MOVE     X-PGM        TO OBJ     OF CHKOBJ-REC.
           MOVE     "*PGM"       TO OBJTYPE OF CHKOBJ-REC.
           CALL     "CLP0004" USING CHKOBJ-REC
           END-CALL.
           IF       OBJ-EXIST OF CHKOBJ-REC = ZEROES
                    MOVE "1" TO RET-CODE
                    GO TO CHK-TED-90
           END-IF.

       CHK-TED-90.
           EXIT.
      /
