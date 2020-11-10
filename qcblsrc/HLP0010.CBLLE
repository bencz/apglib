       IDENTIFICATION DIVISION.
     ‚*
         PROGRAM-ID.              HLP0010.
           COPY COPYRIGHT  OF APG-QCPYSRC.
     ‚*
     ‚*---------------------------------------------------------------*
     ‚* ERMITTELN OBJEKTTYP (DMP1500)                                 *
     ‚*---------------------------------------------------------------*
     ‚*                                                               *
     ‚* AUTOR         :  A. PIEGER                                    *
     ‚*                                                               *
     ‚* ERSTELLT AM   :  06.11.2009                                   *
     ‚*                                                               *
     ‚* FUNKTION      :  ERMITTELN DES OBJEKTTYPS                     *
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

       INPUT-OUTPUT SECTION.
         FILE-CONTROL.

       DATA DIVISION.
       FILE SECTION.

       WORKING-STORAGE SECTION.

     ‚* STANDARDFELDER
           COPY WRKSTD     OF APG-QCPYSRC.

     ‚*  WORKBEREICH ÜBERGABEPARAMETER
           COPY WRKPGMPARM OF APG-QCPYSRC.

       01  PGM-WRK                       PIC X(10) VALUE "HLP0010 ".
       01  PGM-TYP                       PIC X(8)  VALUE "HILFE".

       01  RTVOBJD-REC                   TYPE RTVOBJD-TYPE.
     ‚*-------------------------------------------------------
       LINKAGE SECTION.
       01  X-RTVOBJD-REC                 TYPE RTVOBJD-TYPE.
       01  X-RETCODE                     PIC X(1).
     ‚*-------------------------------------------------------
     ‚*-------------------------------------------------------
       PROCEDURE DIVISION USING X-RTVOBJD-REC
                                X-RETCODE.
     ‚*-------------------------------------------------------
       STEUER SECTION.
       ANFANG.

           INITIALIZE X-RETCODE.
     ‚* RTVOBJD AUSFÜHREN
           PERFORM  GET-OBJECT-DESC.

     ‚* RECORD ZURÜCKGEBEN
           INITIALIZE X-RTVOBJD-REC.
           MOVE     RTVOBJD-REC TO X-RTVOBJD-REC.
           MOVE     RET-CODE    TO X-RETCODE.

       ENDE.

           GOBACK.
      /
     ‚*--------------------------------------------------------------
     ‚* OBJEKTBESCHREIBUNG ERMITTELN
     ‚*--------------------------------------------------------------
       GET-OBJECT-DESC SECTION.
       GET-DES-00.

           INITIALIZE RET-CODE.
     ‚* JETZT OBJEKTTYP ERMITTELN
           INITIALIZE RTVOBJD-REC.
           MOVE     X-RTVOBJD-REC TO RTVOBJD-REC.

     ‚* ANHAND DATEINAMEN "RATEN"
     ‚*
     ‚* PHYSISCHE/LOGISCHE DATEIEN MIT "P" BEGINNEND
     ‚* ODER DISPLAYS UND PRINTERFILES MIT "D" BZW. "F" AN DER
     ‚* DRITTEN STELLE
           IF       OBJ OF RTVOBJD-REC(1:1) = "P"
            OR      OBJ OF RTVOBJD-REC(3:1) = "D"
            OR      OBJ OF RTVOBJD-REC(3:1) = "F"
                    PERFORM TRY-FILENAME
                    IF   RET-CODE = SPACES
                         GO TO GET-DES-90
                    END-IF
           END-IF.

     ‚* PROGRAMME HABEN EIN "P" AN DER 3. STELLE
           IF       OBJ OF RTVOBJD-REC(3:1) = "P"
                    PERFORM TRY-PROGRAM
                    IF   RET-CODE = SPACES
                         GO TO GET-DES-90
                    END-IF
           END-IF.

     ‚* PANELS HABEN EIN "H" AN DER 3. STELLE
           IF       OBJ OF RTVOBJD-REC(3:1) = "H"
                    PERFORM TRY-PANELGRP
                    IF   RET-CODE = SPACES
                         GO TO GET-DES-90
                    END-IF
           END-IF.

     ‚* WENN SONST NICHTS KLAPPT ...
           MOVE     "*PGM"  TO OBJTYPE OF RTVOBJD-REC.

       GET-DES-20.
     ‚* OBJEKTBIBLIOTHEK ERMITTELN
           PERFORM  CALL-CLP0006.
           IF       RET-CODE NOT = SPACES
     ‚*             WURDEN DIE INFOS NICHT ERMITTELT, MIT ANDEREM
     ‚*             OBJEKTTYP ERNEUT VERSUCHEN
                    EVALUATE OBJTYPE OF RTVOBJD-REC
                         WHEN "*PGM"
                              MOVE "*FILE" TO OBJTYPE OF RTVOBJD-REC
                         WHEN "*FILE"
                              MOVE "*MODULE" TO OBJTYPE OF RTVOBJD-REC
                         WHEN "*MODULE"
                              MOVE "*SRVPGM" TO OBJTYPE OF RTVOBJD-REC
                         WHEN "*SRVPGM"
                              MOVE "*CMD"    TO OBJTYPE OF RTVOBJD-REC
                         WHEN "*CMD"
                              MOVE "*MENU"   TO OBJTYPE OF RTVOBJD-REC
                         WHEN "*MENU"
                              MOVE "*PNLGRP" TO OBJTYPE OF RTVOBJD-REC
                         WHEN OTHER
                              GO TO GET-DES-90
                    END-EVALUATE
                    GO TO GET-DES-20
           END-IF.

       GET-DES-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* ES KÖNNTE EIN OBJEKT VOM TYP *FILE SEIN
     ‚*--------------------------------------------------------------
       TRY-FILENAME SECTION.
       TRY-FNM-00.

     ‚* OBJEKTDATEN ERMITTELN
           MOVE     "*FILE" TO OBJTYPE OF RTVOBJD-REC.
           PERFORM  CALL-CLP0006.

       TRY-FNM-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* ES KÖNNTE EIN OBJEKT VOM TYP *PGM ODER *MODULE SEIN
     ‚*--------------------------------------------------------------
       TRY-PROGRAM SECTION.
       TRY-PGM-00.

     ‚* OBJEKTDATEN ERMITTELN
           MOVE     "*PGM" TO OBJTYPE OF RTVOBJD-REC.
           PERFORM  CALL-CLP0006.
           IF       RET-CODE NOT = SPACES
                    MOVE     "*MODULE" TO OBJTYPE OF RTVOBJD-REC
                    PERFORM  CALL-CLP0006
           END-IF.

       TRY-PGM-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* ES KÖNNTE EIN OBJEKT VOM TYP *PNLGRP SEIN
     ‚*--------------------------------------------------------------
       TRY-PANELGRP SECTION.
       TRY-PGR-00.

     ‚* OBJEKTDATEN ERMITTELN
           MOVE     "*PNLGRP" TO OBJTYPE OF RTVOBJD-REC.
           PERFORM  CALL-CLP0006.

       TRY-PGR-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* AUFRUF DES PROGRAMMS
     ‚*--------------------------------------------------------------
       CALL-CLP0006 SECTION.
       CAL-CLP-00.

           INITIALIZE RET-CODE.
           CALL     "CLP0006" USING RTVOBJD-REC
                                    RET-CODE
           END-CALL.

       CAL-CLP-90.
           EXIT.
      /
