       IDENTIFICATION DIVISION.
     ‚*
         PROGRAM-ID.              CMP9000.
           COPY COPYRIGHT  OF APG-QCPYSRC.
     ‚*
     ‚*---------------------------------------------------------------*
     ‚* TEMPORÄRE DATEIEN ERSTELLEN                                   *
     ‚*---------------------------------------------------------------*
     ‚*                                                               *
     ‚* AUTOR         :  A. PIEGER                                    *
     ‚*                                                               *
     ‚* ERSTELLT AM   :  21.09.2010                                   *
     ‚*                                                               *
     ‚* FUNKTION      :  PTMPLST (DSPFD -> *MBRLIST)                  *
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

     ‚* STANDARDFELDER CL-PROGRAMME
           COPY WRKPGMPARM OF APG-QCPYSRC.

       01  PGM-WRK                       PIC X(10) VALUE "CMP9000".
       01  PGM-TYP                       PIC X(8)  VALUE "COMPILER".

       01  DSPFD-REC                     TYPE DSPFD-TYPE.
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
           MOVE     "CMP9000" TO PGM-WRK.
           PERFORM  COPY-PGM-INIT.

     ‚* DARF DIESES PROGRAMM BENUTZT WERDEN?
           PERFORM  COPY-PERMISSION.
           IF       RET-CODE NOT = SPACES
                    GO TO ENDE
           END-IF.

     ‚* TEMPORÄRE DATEI ERSTELLEN
           PERFORM  CREATE-PTMPLST.
           IF       RET-CODE NOT = SPACES
                    GO TO ENDE
           END-IF.

       ENDE.
           PERFORM  COPY-PGM-EXIT.

           GOBACK.
      /
     ‚*--------------------------------------------------------------
     ‚* DSPFD AUSFÜHREN UM ALLE SOURCEN ZU ERMITTELN
     ‚*--------------------------------------------------------------
       CREATE-PTMPLST SECTION.
       CRT-TMP-00.

           INITIALIZE RET-CODE, DSPFD-REC.
           MOVE     "APG"               TO LIBNAME   OF DSPFD-REC.
           MOVE     "QSETSRC"           TO FILENAME  OF DSPFD-REC.
           MOVE     "*OUTFILE"          TO OUTPUTTYP OF DSPFD-REC.
           MOVE     "*MBRLIST"          TO TYPNAME   OF DSPFD-REC.
           MOVE     "PTMPLST"           TO OUTFILE   OF DSPFD-REC.
           MOVE     "QTEMP"             TO OUTLIB    OF DSPFD-REC.
           MOVE     "*REPLACE"          TO OUTOPT    OF DSPFD-REC.
           CALL     "CLP0042" USING DSPFD-REC
                                    RET-CODE
           END-CALL.

       CRT-TMP-90.
           EXIT.
      /
     ‚*---------------------------------------------------------------
     ‚* COPY-PERMISSION: BERECHTIGUNGSPRÜFUNG
     ‚*---------------------------------------------------------------

           COPY     PERMISSION OF APG-QCPYSRC.
      /
     ‚*---------------------------------------------------------------
     ‚* COPY-PGM-INIT/COPY-PGM-EXIT: STANDARDROUTINEN START UND ENDE
     ‚*---------------------------------------------------------------

           COPY     PGMWRK     OF APG-QCPYSRC.
      /
