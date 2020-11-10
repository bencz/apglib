       IDENTIFICATION DIVISION.
     ‚*
         PROGRAM-ID.              CMP0006.
           COPY COPYRIGHT  OF APG-QCPYSRC.
     ‚*
     ‚*---------------------------------------------------------------*
     ‚* FÜLLEN PTMPFFD                                                *
     ‚*---------------------------------------------------------------*
     ‚*                                                               *
     ‚* AUTOR         :  A. PIEGER                                    *
     ‚*                                                               *
     ‚* ERSTELLT AM   :  30.10.2009                                   *
     ‚*                                                               *
     ‚* FUNKTION      :  ERSTELLEN PTMPFFD                            *
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

       01  PGM-WRK                       PIC X(10) VALUE "CMP0006".
       01  PGM-TYP                       PIC X(8)  VALUE "COMPILER".

     ‚* PARAMETER FÜR CL-PROGRAMME
       01  DSPFFD-REC                    TYPE DSPFFD-TYPE.
      /
     ‚*-------------------------------------------------------------
       LINKAGE SECTION.
       01  X-LIB                         PIC X(10).
       01  X-FILE                        PIC X(10).
     ‚*-------------------------------------------------------------

     ‚*-------------------------------------------------------------
       PROCEDURE DIVISION USING X-LIB
                                X-FILE.
     ‚*-------------------------------------------------------------
       STEUER SECTION.
       ANFANG.

     ‚* FÜLLEN PGM-WRK UND AUSFÜHREN STANDARDROUTINEN
           MOVE     "CMP0006" TO PGM-WRK.
           PERFORM  COPY-PGM-INIT.

           PERFORM  CREATE-DSPFFD.
           IF       RET-CODE NOT = SPACES
                    GO TO ENDE
           END-IF.

       ENDE.

           PERFORM  COPY-PGM-EXIT.

           GOBACK.
      /
     ‚*--------------------------------------------------------------
     ‚* AUSFÜHREN DES DSPFFD IN DIE QTEMP
     ‚*--------------------------------------------------------------
       CREATE-DSPFFD SECTION.
       CRT-FFD-00.

           INITIALIZE RET-CODE.
     ‚* DSPFFD AUSFÜHREN
           INITIALIZE DSPFFD-REC.
           MOVE     X-FILE          TO FILENAME OF DSPFFD-REC.
           MOVE     X-LIB           TO LIBNAME  OF DSPFFD-REC.
           MOVE     "PTMPFFD"       TO OUTFILE  OF DSPFFD-REC.
           MOVE     "QTEMP"         TO OUTLIB   OF DSPFFD-REC.
           CALL     "CLP0043" USING DSPFFD-REC
                                    RET-CODE
           END-CALL.

       CRT-FFD-90.
           EXIT.
      /
     ‚*---------------------------------------------------------------
     ‚* COPY-PGM-INIT/COPY-PGM-EXIT: STANDARDROUTINEN START UND ENDE
     ‚*---------------------------------------------------------------

           COPY     PGMWRK     OF APG-QCPYSRC.
      /
