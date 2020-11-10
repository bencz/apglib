       IDENTIFICATION DIVISION.
     ‚*
         PROGRAM-ID.              CFP0025.
           COPY COPYRIGHT  OF APG-QCPYSRC.
     ‚*
     ‚*---------------------------------------------------------------*
     ‚* WRAPPER FÜR ZITATPROGRAMM                                     *
     ‚*---------------------------------------------------------------*
     ‚*                                                               *
     ‚* AUTOR         :  A. PIEGER                                    *
     ‚*                                                               *
     ‚* ERSTELLT AM   :  20.06.2011                                   *
     ‚*                                                               *
     ‚* FUNKTION      :  ZITAIPROGRAMM CFP0024 AUFRUFEN               *
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

     ‚* INTERNE TABELLEN
           COPY WRKINTTAB  OF APG-QCPYSRC.

       01  PGM-WRK                       PIC X(10) VALUE "CFP0025".

     ‚* PARAMETER FÜR CL-PROGRAMME
       01  MAPDTA-REC                    TYPE MAPDTA-TYPE.
       01  PTRMAP-REC                    TYPE PTRMAP-TYPE.

     ‚* SCHALTER
       01  CLEAR-POINTER                 PIC 9(1).

     ‚* PARAMETER FÜR DAS HLP0021
       01  TEXT-WRK                      PIC X(500).
     ‚*-------------------------------------------------------------
       LINKAGE SECTION.
       01  X-POINTER                     USAGE POINTER.
       01  X-MAXLEN                      PIC 9(4).
       01  X-RESULT-TAB                  TYPE HLP0021-TAB.
       01  X-PGM                         LIKE PGM-WRK.
     ‚*-------------------------------------------------------------

     ‚*-------------------------------------------------------------
       PROCEDURE DIVISION USING X-POINTER
                                X-MAXLEN
                                X-RESULT-TAB
                                X-PGM.
     ‚*-------------------------------------------------------------
       STEUER SECTION.
       ANFANG.

           INITIALIZE CLEAR-POINTER.
     ‚* WURDE EIN POINTER ÜBERGEBEN?
           IF       X-POINTER = NULL
                    MOVE 1 TO CLEAR-POINTER
           END-IF.

     ‚* POINTER FÜLLEN
           EVALUATE X-PGM
                    WHEN "XXP0044"   PERFORM XXP0044-WRAPPER
           END-EVALUATE.

     ‚* ZITATPROGRAMM AUFRUFEN
           CALL     "CFP0026" USING X-POINTER
           END-CALL.

     ‚* AUFTEILEN DES ZITATS IN MEHRERE ZEILEN
           PERFORM  SEPERATE-TEXT.

     ‚* POINTER LEEREN?
           IF       CLEAR-POINTER = 1
                    SET      PTR-MAIN OF PTRMAP-REC TO X-POINTER
                    SET      PTR-CLEAR  OF PTRMAP-REC TO TRUE
                    PERFORM  COPY-PTR-MAPPING
                    SET      X-POINTER TO NULL
           END-IF.

       ENDE.

           GOBACK.
      /
     ‚*--------------------------------------------------------------
     ‚* TEXT AUFTEILEN
     ‚*--------------------------------------------------------------
       SEPERATE-TEXT SECTION.
       SET-TXT-00.

     ‚* ERSTELLEN/LESEN DES KATEGORIE-POINTER
           SET      PTR-MAIN OF PTRMAP-REC TO X-POINTER.
           SET      PTR-READ OF PTRMAP-REC TO TRUE.
           MOVE     "RESULT"               TO PTR-KEY OF PTRMAP-REC.
           PERFORM  COPY-PTR-MAPPING.

           INITIALIZE MAPDTA-REC.
           MOVE     "QUOTE"             TO MAP-KEY OF MAPDTA-REC.
           MOVE     SPACES              TO MAP-REC OF MAPDTA-REC.
           MOVE     MAP-READ            TO MAP-ACT OF MAPDTA-REC.
           SET      MAP-PTR OF MAPDTA-REC TO PTR-RETURN OF PTRMAP-REC.
           PERFORM  COPY-CALL-MAPDTA.
           IF       RET-CODE = SPACES
                    MOVE MAP-REC OF MAPDTA-REC(10:) TO TEXT-WRK
     ‚*             TEXT IN INTERNE TABELLE AUFTEILEN
                    CALL     "HLP0021" USING TEXT-WRK
                                             X-MAXLEN
                                             X-RESULT-TAB
                                             RET-CODE
                    END-CALL
           END-IF.

       SET-TXT-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* XXP0044: GABELZEIT
     ‚*--------------------------------------------------------------
       XXP0044-WRAPPER SECTION.
       WRP-X44-00.

     ‚* ERSTELLEN/LESEN DES KATEGORIE-POINTER
           SET      PTR-MAIN OF PTRMAP-REC TO X-POINTER.
           SET      PTR-CREATE OF PTRMAP-REC TO TRUE.
           MOVE     "CATEGORY" TO PTR-KEY OF PTRMAP-REC.
           PERFORM  COPY-PTR-MAPPING.
           SET      X-POINTER TO PTR-MAIN OF PTRMAP-REC.

           INITIALIZE MAPDTA-REC.
           MOVE     "GABEL"             TO MAP-KEY OF MAPDTA-REC.
           MOVE     SPACES              TO MAP-REC OF MAPDTA-REC.
           MOVE     MAP-WRITE           TO MAP-ACT OF MAPDTA-REC.
           SET      MAP-PTR OF MAPDTA-REC TO PTR-RETURN OF PTRMAP-REC.
           PERFORM  COPY-CALL-MAPDTA.

       WRP-X44-90.
           EXIT.
      /
     ‚*---------------------------------------------------------------
     ‚* COPY-CALL-MAPDTA: MAPDTA AUSFÜHREN
     ‚*---------------------------------------------------------------

           COPY     MAPDTA     OF APG-QCPYSRC.
      /
