       IDENTIFICATION DIVISION.
     ‚*
         PROGRAM-ID.              CFP9003.
           COPY COPYRIGHT  OF APG-QCPYSRC.
     ‚*
     ‚*---------------------------------------------------------------*
     ‚* HINZUFÜGEN DER MSGF FÜR APG-PROGRAMME                         *
     ‚*---------------------------------------------------------------*
     ‚*                                                               *
     ‚* AUTOR         :  A. PIEGER                                    *
     ‚*                                                               *
     ‚* ERSTELLT AM   :  11.01.2009                                   *
     ‚*                                                               *
     ‚* FUNKTION      :  MSGF ERSTELLEN UND SETZEN SPRACHE            *
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
           COPY WRKSTD      OF APG-QCPYSRC.

     ‚* WORKBEREICH ÜBERGABEPARAMETER
           COPY WRKPGMPARM  OF APG-QCPYSRC.

       01  PGM-WRK                       PIC X(10) VALUE "CFP9003".
       01  PGM-TYP                       PIC X(8)  VALUE "CONFIG".
      /
     ‚*-------------------------------------------------------------
       LINKAGE SECTION.
     ‚*-------------------------------------------------------------

     ‚*-------------------------------------------------------------
       PROCEDURE DIVISION.
     ‚*-------------------------------------------------------------
       STEUER SECTION.
       ANFANG.

     ‚* FÜLLEN PGM-WRK UND AUSFÜHREN STANDARDROUTINEN
           MOVE     "CFP9003" TO PGM-WRK.
           PERFORM  COPY-PGM-INIT.

     ‚* PRÜFEN OB OBJEKT VORHANDEN IST
           INITIALIZE CHKOBJ-REC.
           MOVE     "APGSPRACH"  TO OBJ     OF CHKOBJ-REC.
           MOVE     "QTEMP"      TO LIB     OF CHKOBJ-REC.
           MOVE     "*MSGF"      TO OBJTYPE OF CHKOBJ-REC.
           CALL     "CLP0004" USING CHKOBJ-REC
           END-CALL.
           IF       OBJ-EXIST OF CHKOBJ-REC = ZEROES
                    INITIALIZE CRTMSGF-REC
                    MOVE "MSGF ZUM ABLEGEN DER AKTIVEN SPRACHE"
                                     TO MSGTXT OF CRTMSGF-REC
                    MOVE "APGSPRACH" TO MSGF   OF CRTMSGF-REC
                    MOVE "QTEMP"     TO MSGLIB OF CRTMSGF-REC
                    CALL "CLP0005" USING CRTMSGF-REC
                    IF   SPRACHE-WRK = SPACES
                         MOVE "DEU" TO SPRACHE-WRK
                    END-IF
     ‚*             MSG HINZUFÜGEN
                    INITIALIZE ADDMSG-REC
                    MOVE "SP00001"     TO MSGID  OF ADDMSG-REC
                    MOVE "QTEMP"       TO MSGLIB OF ADDMSG-REC
                    MOVE "APGSPRACH"   TO MSGF   OF ADDMSG-REC
                    MOVE "DEU"         TO MSG    OF ADDMSG-REC
                    CALL "CLP0003" USING ADDMSG-REC
                                         RET-CODE
                    END-CALL
           END-IF.

       ENDE.
           PERFORM  COPY-PGM-EXIT.

           GOBACK.
      /
     ‚*---------------------------------------------------------------
     ‚* COPY-PGM-INIT/COPY-PGM-EXIT: STANDARDROUTINEN START UND ENDE
     ‚*---------------------------------------------------------------

           COPY     PGMWRK     OF APG-QCPYSRC.
      /
