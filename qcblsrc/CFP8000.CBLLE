       IDENTIFICATION DIVISION.
     ‚*
         PROGRAM-ID.              CFP8000.
           COPY COPYRIGHT  OF APG-QCPYSRC.
     ‚*
     ‚*---------------------------------------------------------------*
     ‚* LESEN EINES PROGRAMMTEXTES (PCFGTXT)                          *
     ‚*---------------------------------------------------------------*
     ‚*                                                               *
     ‚* AUTOR         :  A. PIEGER                                    *
     ‚*                                                               *
     ‚* ERSTELLT AM   :  19.01.2009                                   *
     ‚*                                                               *
     ‚* FUNKTION      :  LESEN EINES PROGRAMMTEXTES AUS DER PCFGTXT   *
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

     ‚*-------------------------------------------------------------
     ‚* PHYSISCHE DATEI TEXTE
     ‚*-------------------------------------------------------------
           SELECT PCFGTXT-DP
                  ASSIGN       TO  DATABASE-PCFGTXT
                  ORGANIZATION IS  INDEXED
                  RECORD KEY   IS  EXTERNALLY-DESCRIBED-KEY
                  ACCESS       IS  DYNAMIC
                  FILE STATUS  IS  FILE-STATUS.

      /
       DATA DIVISION.
       FILE SECTION.
     ‚*--- TEXTE
       FD  PCFGTXT-DP
           LABEL RECORDS ARE STANDARD.
       01  PCFGTXT-P.
           COPY DDS-ALL-FORMATS OF PCFGTXT.
      /
       WORKING-STORAGE SECTION.

     ‚* STANDARDFELDER
           COPY WRKSTD      OF APG-QCPYSRC.

     ‚* WORKBEREICH ÜBERGABEPARAMETER
           COPY WRKPGMPARM  OF APG-QCPYSRC.

       01  PGM-WRK                       PIC X(10) VALUE "CFP8000".
       01  PGM-TYP                       PIC X(8)  VALUE "CONFIG".
      /
     ‚*-------------------------------------------------------------
       LINKAGE SECTION.
       01  X-TXID                        LIKE TXID   OF PCFGTXT-P.
       01  X-TXSPR                       LIKE TXSPR  OF PCFGTXT-P.
       01  X-GETDEF                      PIC 9(1).
       01  X-TXTEXT                      LIKE TXTEXT OF PCFGTXT-P.
     ‚*-------------------------------------------------------------

     ‚*-------------------------------------------------------------
       PROCEDURE DIVISION USING X-TXID
                                X-TXSPR
                                X-GETDEF
                                X-TXTEXT.
     ‚*-------------------------------------------------------------
       STEUER SECTION.
       ANFANG.

     ‚* FÜLLEN PGM-WRK
           MOVE     "CFP8000" TO PGM-WRK.

     ‚* NUR EINMAL ÖFFNEN
           IF       FIRST-CALL NOT = "1"
                    MOVE "1" TO FIRST-CALL
                    OPEN     I-O   PCFGTXT-DP
           END-IF.

           INITIALIZE X-TXTEXT.
     ‚* ALLE PARAMETER VORHANDEN?
           IF       X-TXID  = SPACES
            OR      X-TXSPR = SPACES
                    GO TO ENDE
           END-IF.

     ‚* LESEN DES ÜBERGEBENEN TEXTES
           PERFORM  LESEN-TEXTID.
           IF       RET-CODE NOT = SPACES
                    PERFORM  ANLEGEN-DUMMY
                    MOVE "?"      TO X-TXTEXT(1:1)
                    MOVE  X-TXID  TO X-TXTEXT(2:)
           END-IF.

       ENDE.

           GOBACK.
      /
     ‚*--------------------------------------------------------------
     ‚* LESEN DES TEXTES IN DER PCFGTXT
     ‚*--------------------------------------------------------------
       LESEN-TEXTID SECTION.
       LES-DAT-00.

           INITIALIZE RET-CODE.
           INITIALIZE PCFGTXTF OF PCFGTXT-P.
           MOVE     X-TXID          TO TXID   OF PCFGTXT-P.
           MOVE     X-TXSPR         TO TXSPR  OF PCFGTXT-P.
           READ     PCFGTXT-DP WITH NO LOCK.
           IF       FILE-STATUS NOT = ZEROES
                    MOVE "1" TO RET-CODE
                    GO TO LES-DAT-90
           END-IF.

     ‚* ÜBERNEHMEN DES TEXTES
           MOVE     TXTEXT OF PCFGTXT-P TO X-TXTEXT.

       LES-DAT-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* ANLEGEN EINES DATENSATZES IN DER PCFGTXT
     ‚*--------------------------------------------------------------
       ANLEGEN-DUMMY SECTION.
       ANL-DUM-00.

           INITIALIZE PCFGTXTF OF PCFGTXT-P.
           MOVE     X-TXSPR         TO TXSPR  OF PCFGTXT-P.
           MOVE     X-TXID          TO TXID   OF PCFGTXT-P.
           MOVE     "?"             TO TXTEXT OF PCFGTXT-P(1:1).
           MOVE     X-TXID          TO TXTEXT OF PCFGTXT-P(2:).
           PERFORM  COPY-GET-TIME.
     ‚* USER/TERMINAL
           MOVE     PGM-WRK  TO TXAPGM OF PCFGTXT-P.
     ‚*€   MOVE     T-USER   TO TXAUSR OF PCFGTXT-P.
     ‚*€   MOVE     T-TERM   TO TXABS  OF PCFGTXT-P.
     ‚* DATUM/UHRZEIT
           MOVE     DATE-8   TO TXADTA OF PCFGTXT-P.
           MOVE     TIME-6   TO TXATIA OF PCFGTXT-P.

           WRITE    PCFGTXT-P.

       ANL-DUM-90.
           EXIT.
      /
     ‚*---------------------------------------------------------------
     ‚* COPY-GET-TIME: DATUM UND UHRZEIT ERMITTELN
     ‚*---------------------------------------------------------------

           COPY     GETTIME    OF APG-QCPYSRC.
      /
