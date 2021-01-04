       IDENTIFICATION DIVISION.
     ‚*
         PROGRAM-ID.              XXP9000.
           COPY COPYRIGHT  OF APG-QCPYSRC.
     ‚*
     ‚*---------------------------------------------------------------*
     ‚* FEHLERAFTES PGM FÜR FALSCHEN PARAMETER AUFRUF                 *
     ‚*---------------------------------------------------------------*
     ‚*                                                               *
     ‚* AUTOR         :  A. PIEGER                                    *
     ‚*                                                               *
     ‚* ERSTELLT AM   :  15.10.2009                                   *
     ‚*                                                               *
     ‚* FUNKTION      :  FEHLERHAFTES BEISPIEL-PGMˆ!!!‚               *
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

       01  PARM-WRK1                     PIC X(50).
       01  PARM-WRK2                     PIC X(50).
      /
     ‚*-------------------------------------------------------------
       LINKAGE SECTION.
     ‚*-------------------------------------------------------------
       PROCEDURE DIVISION.
     ‚*-------------------------------------------------------------
       STEUER SECTION.
       ANFANG.

     ‚* IN XXP9001 SIND ALLE PARAMETER 1000 ZEICHEN GROß (=3000)
           MOVE     "HALLO" TO PARM-WRK1.
           MOVE     "#"     TO PARM-WRK1(50:1).
           MOVE     "WELT"  TO PARM-WRK2.
           CALL     "XXP9001" USING PARM-WRK1
                                    PARM-WRK2
           END-CALL.

     ‚* MIT DEM CANCEL WIRD DAFÜR GESORGT, DAS BEIM NÄCHSTEN AUFRUF
     ‚* ALLE WERTE INITIALISIERT SIND
           CANCEL   "XXP9001".

       ENDE.

           GOBACK.
      /
