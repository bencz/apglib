       IDENTIFICATION DIVISION.
     ‚*
         PROGRAM-ID.              XXP0000.
           COPY COPYRIGHT  OF APG-QCPYSRC.
     ‚*
     ‚*---------------------------------------------------------------*
     ‚* STARTEN APG-PROGRAMME                                         *
     ‚*---------------------------------------------------------------*
     ‚*                                                               *
     ‚* AUTOR         :  A. PIEGER                                    *
     ‚*                                                               *
     ‚* ERSTELLT AM   :  21.09.2009                                   *
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

       01  PGM-WRK                       PIC X(10) VALUE "XXP0000".
       01  PGM-TYP                       PIC X(8)  VALUE "TEST".
      /
     ‚*-------------------------------------------------------------
       LINKAGE SECTION.
       01  X-CALLPGM                     PIC X(10).
     ‚*-------------------------------------------------------------
       PROCEDURE DIVISION USING X-CALLPGM.
     ‚*-------------------------------------------------------------
       STEUER SECTION.
       ANFANG.

     ‚* FÜLLEN PGM-WRK
           MOVE     "XXP0000" TO PGM-WRK.

     ‚* ERSTMAL MEINE BIBLIOTHEK VORSCHALTEN
           PERFORM  ADD-APG-LIB.
           IF       RET-CODE NOT = SPACES
                    GO TO ENDE
           END-IF.

     ‚* JETZT PRÜFEN OB ES DAS PGM AUCH GIBT
           PERFORM  CHECK-OBJECT.
           IF       RET-CODE NOT = SPACES
                    GO TO ENDE
           END-IF.

     ‚* JETZT DAS PROGRAMM AUSFÜHREN (UND HOFFEN DASS ES WIRKLICH
     ‚* KEINE PARAMETER BRAUCHT)
           CALL     X-CALLPGM IN LIBRARY CONST-LIB
           END-CALL.

     ‚* APG-BIBLIOTHEK LÖSCHEN
           PERFORM  DEL-APG-LIB.

       ENDE.

           GOBACK.
      /
     ‚*--------------------------------------------------------------
     ‚* APG-BIBLIOTHEK VORSCHALTEN
     ‚*--------------------------------------------------------------
       ADD-APG-LIB SECTION.
       ADD-APG-00.

     ‚* "APG" BIBLIOTHEK HINZUFÜGEN
           INITIALIZE ADDLIBLE-REC.
           MOVE     "APG"        TO LIB     OF ADDLIBLE-REC.
           MOVE     "*FIRST"     TO LIBPOS  OF ADDLIBLE-REC.
           CALL     "CLP0007" IN LIBRARY CONST-LIB
                              USING ADDLIBLE-REC
                                    RET-CODE
           END-CALL.

       ADD-APG-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* APG-BIBLIOTHEK ENTFERNEN
     ‚*--------------------------------------------------------------
       DEL-APG-LIB SECTION.
       DEL-APG-00.

     ‚* "APG" BIBLIOTHEK HINZUFÜGEN
           INITIALIZE RMVLIBLE-REC.
           MOVE     "APG"        TO LIB     OF RMVLIBLE-REC.
           CALL     "CLP0009" USING RMVLIBLE-REC
           END-CALL.

       DEL-APG-90.
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
           MOVE     X-CALLPGM    TO OBJ     OF CHKOBJ-REC.
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
