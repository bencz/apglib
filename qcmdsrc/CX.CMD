             CMD        PROMPT('OBJEKT ERSTELLEN')

/*‚          OBJEKTEINSTELLUNGEN                                     */
             PARM       KWD(OPGM) TYPE(*CHAR) LEN(10) MIN(1)         +
                         PASSATR(*YES) CHOICE('Name')                +
                         PROMPT('Programm') KEYPARM(*YES)
             PARM       KWD(OLIB) TYPE(*CHAR) LEN(10) MIN(0)         +
                         PASSATR(*YES) CHOICE('Name, *CURLIB')       +
                         PROMPT('  Bibliothek') DFT(APG) KEYPARM(*YES)
/*‚          SOURCE-EINSTELLUNGEN                                    */
             PARM       KWD(SFILE) TYPE(*CHAR) LEN(10) MIN(0)  +
                         PASSATR(*YES) CHOICE('Name')          +
                         PROMPT('Quellendatei') DFT(QCBLSRC)   +
                         KEYPARM(*YES)
             PARM       KWD(SLIB) TYPE(*CHAR) LEN(10) MIN(0)   +
                         PASSATR(*YES) CHOICE('Name, *CURLIB') +
                         PROMPT('  Bibliothek') DFT(APG)       +
                         KEYPARM(*YES)
             PARM       KWD(SMBR) TYPE(*CHAR) LEN(10) MIN(1)   +
                         PASSATR(*YES) CHOICE('Name')          +
                         PROMPT('  Teildatei')                 +
                         KEYPARM(*YES)
             PARM       KWD(STYP) TYPE(*CHAR) LEN(10) MIN(1)   +
                         PASSATR(*YES) CHOICE('Name')          +
                         PROMPT('Sourcetyp')                   +
                         KEYPARM(*YES)

/*‚          PROGRAMM-EINSTELLUNGEN => PGMCTL(PGM)                   */
             PARM       KWD(TGTRLS) TYPE(*CHAR) LEN(10) RSTD(*NO)    +
                         MIN(0) PASSATR(*YES) PMTCTL(PGM)            +
                         CHOICE('*CURRENT, *PRV, VxRxMx')            +
                         PROMPT('Ziel-Release')
             PARM       KWD(ACTGRP) TYPE(*CHAR) LEN(10) RSTD(*NO)    +
                         MIN(0) MAX(1) PASSATR(*YES)                 +
                         CHOICE('Name, QILE, *NEW, *CALLER')         +
                         PMTCTL(PGM) PROMPT('Aktivierungsgruppe')
             PARM       KWD(BND1) TYPE(*CHAR) LEN(10) RSTD(*NO)      +
                         MIN(0) MAX(1) PASSATR(*YES)                 +
                         CHOICE('Name') PMTCTL(PGM)                  +
                         PROMPT('Binderverzeichnis')
             PARM       KWD(DBG) TYPE(*CHAR) LEN(1) RSTD(*NO)        +
                         DFT(1) MIN(0) PASSATR(*YES) PMTCTL(PGM)     +
                         CHOICE('(0=nein/1=ja)') PROMPT('Debugger')
/*‚          -- OPTIMIERUNGSSTUFE                                    */
             PARM       KWD(OPTIM) TYPE(*CHAR) LEN(6) RSTD(*NO)      +
                         DFT(*NONE) MIN(0) PASSATR(*YES) PMTCTL(PGM) +
                         CHOICE('*NONE, *BASIC, *FULL')              +
                         PROMPT('Optimierungsstufe')
/*‚          -- MODUL, PGM, BEIDES?                                  */
             PARM       KWD(MODUL) TYPE(*CHAR) LEN(1) RSTD(*NO)      +
                         DFT(0) MIN(0) PASSATR(*YES) PMTCTL(PGM)     +
                         CHOICE('(0=PGM/1=Modul/2=Beides')           +
                         PROMPT('Zu erstellendes Objekt')

/*‚          COMMAND-EINSTELLUNGEN => PGMCTL(CMD)                    */
             PARM       KWD(OVRPGM) TYPE(*CHAR) LEN(10) RSTD(*NO)    +
                         DFT(*NONE) MIN(0) PASSATR(*YES) PMTCTL(CMD) +
                         CHOICE('Name, *NONE')                       +
                         PROMPT('Bedienerf.überschreibungsprog.')
             PARM       KWD(OVRLIB) TYPE(*CHAR) LEN(10) RSTD(*NO)    +
                         DFT(*LIBL) MIN(0) PASSATR(*YES) PMTCTL(CMD) +
                         CHOICE('Name, *LIBL, *CURLIB')              +
                         PROMPT('  Bibliothek')
             PARM       KWD(EXCPGM) TYPE(*CHAR) LEN(10) RSTD(*NO)    +
                         MIN(0) PASSATR(*YES) PMTCTL(CMD)            +
                         CHOICE('Name')                              +
                         PROMPT('Befehlsverarbeitungsprogramm')
             PARM       KWD(EXCLIB) TYPE(*CHAR) LEN(10) RSTD(*NO)    +
                         DFT(*LIBL) MIN(0) PASSATR(*YES) PMTCTL(CMD) +
                         CHOICE('Name, *LIBL, *CURLIB')              +
                         PROMPT('  Bibliothek')
             PARM       KWD(PRDLIB) TYPE(*CHAR) LEN(10) RSTD(*NO)    +
                         DFT(*NOCHG) MIN(0) PASSATR(*YES)            +
                         PMTCTL(CMD) CHOICE('Name, *NOCHG')          +
                         PROMPT('Produktbibliothek')

/*‚          DISPLAY-EINSTELLUNGEN => PGMCTL(DSP)                    */
             PARM       KWD(RSTDSP) TYPE(*CHAR) LEN(1) RSTD(*NO)     +
                         DFT(1) MIN(0) PASSATR(*YES) PMTCTL(DSP)     +
                         CHOICE('(0=nein/1=ja)')                     +
                         PROMPT('Anzeige wiederherstellen')
             PARM       KWD(DFRWRT) TYPE(*CHAR) LEN(1) RSTD(*NO)     +
                         DFT(1) MIN(0) PASSATR(*YES) PMTCTL(DSP)     +
                         CHOICE('(0=nein/1=ja)')                     +
                         PROMPT('Schreiben verzögern')

/*‚          PHYS. DATEI-EINSTELLUNGEN => PGMCTL(PF)                 */
             PARM       KWD(PFSIZE) TYPE(*CHAR) LEN(10) RSTD(*NO)    +
                         DFT(8000000) MIN(0) PASSATR(*YES)           +
                         PMTCTL(PF)                                  +
                         CHOICE('1-2147483646, *NOMAX')              +
                         PROMPT('Anfangsanzahl Sätze')
             PARM       KWD(PFEXT) TYPE(*CHAR) LEN(5) RSTD(*NO)      +
                         DFT(10000) MIN(0) PASSATR(*YES) PMTCTL(PF)  +
                         CHOICE('0-32767')                           +
                         PROMPT('Satzanzahl für Erweiterung')
             PARM       KWD(PFMXE) TYPE(*CHAR) LEN(5) RSTD(*NO)      +
                         DFT(3) MIN(0) PASSATR(*YES) PMTCTL(PF)      +
                         CHOICE('0-32767')                           +
                         PROMPT('Maximale Erweiterungen')

/*‚          TBL-EINSTELLUNGEN => PGMCTL(TBL)                        */
             PARM       KWD(TBLTYP) TYPE(*CHAR) LEN(10) RSTD(*NO)    +
                         MIN(0) PASSATR(*YES) PMTCTL(TBL)            +
                         CHOICE('Name')                              +
                         PROMPT('Define Names')

/*‚          C-EINSTELLUNGEN => PGMCTL(C)                            */
             PARM       KWD(DEFINE) TYPE(*CHAR) LEN(80) RSTD(*NO)    +
                         MIN(0) PASSATR(*YES) PMTCTL(C)              +
                         CHOICE('Name')                              +
                         PROMPT('Define Names')

/*‚          SONSTIGE EINSTELLUNGEN => PGMCTL(OTHER)                 */
             PARM       KWD(RECHT) TYPE(*CHAR) LEN(1) RSTD(*NO)      +
                         DFT(0) MIN(0) PASSATR(*YES)                 +
                         CHOICE('(0=nein/1=USE/2=EXCLUDE)')          +
                         PROMPT('Berechtigungen ändern')             +
                         PMTCTL(OTHER)
             PARM       KWD(INTER) TYPE(*CHAR) LEN(1) RSTD(*NO)      +
                         DFT(0) MIN(0) PASSATR(*YES)                 +
                         CHOICE('(0=nein/1=ja)')                     +
                         PROMPT('Interaktiv wandeln')                +
                         PMTCTL(OTHER)
             PARM       KWD(NEWOWN) TYPE(*CHAR) LEN(10) RSTD(*NO)    +
                         MIN(0) PASSATR(*YES) CHOICE('Name')         +
                         PROMPT('Neuer Eigner')                      +
                         PMTCTL(OTHER)
             PARM       KWD(VCPGM) TYPE(*CHAR) LEN(10) RSTD(*NO)     +
                         MIN(0) PASSATR(*YES)                        +
                         CHOICE('Name')                              +
                         PROMPT('Programmaufruf vor wandeln')        +
                         PMTCTL(OTHER)
             PARM       KWD(VCLIB) TYPE(*CHAR) LEN(10) RSTD(*NO)     +
                         MIN(0) PASSATR(*YES)                        +
                         CHOICE('Name, *LIBL, *CURLIB')              +
                         PROMPT('  Bibliothek')                      +
                         PMTCTL(OTHER)
             PARM       KWD(VCPRM) TYPE(*CHAR) LEN(150) RSTD(*NO)    +
                         MIN(0) PASSATR(*YES)                        +
                         CHOICE('Name')                              +
                         PROMPT('  Parameter')                       +
                         PMTCTL(OTHER)
             PARM       KWD(NCPGM) TYPE(*CHAR) LEN(10) RSTD(*NO)     +
                         MIN(0) PASSATR(*YES)                        +
                         CHOICE('Name')                              +
                         PROMPT('Programmaufruf nach wandeln')       +
                         PMTCTL(OTHER)
             PARM       KWD(NCLIB) TYPE(*CHAR) LEN(10) RSTD(*NO)     +
                         MIN(0) PASSATR(*YES)                        +
                         CHOICE('Name, *LIBL, *CURLIB')              +
                         PROMPT('  Bibliothek')                      +
                         PMTCTL(OTHER)
             PARM       KWD(NCPRM) TYPE(*CHAR) LEN(150) RSTD(*NO)    +
                         MIN(0) PASSATR(*YES)                        +
                         CHOICE('Name')                              +
                         PROMPT('  Parameter')                       +
                         PMTCTL(OTHER)
             PARM       KWD(SCRMB) TYPE(*CHAR) LEN(1) RSTD(*NO)      +
                         MIN(0) PASSATR(*YES)                        +
                         CHOICE('(0=nein/1=ja)')                     +
                         PROMPT('  Scramblen')                       +
                         PMTCTL(PGM)
             PARM       KWD(PRMOVR) TYPE(*CHAR) LEN(4) RSTD(*NO)     +
                         MIN(0) CHOICE(*NONE) DFT(*NO) PASSATR(*YES) +
                         DSPINPUT(*NO)

/*‚          ANZEIGE-EINSTELLUNGEN                                   */
 PGM:        PMTCTL     CTL(STYP) COND((*EQ 'CBLLE'))
             PMTCTL     CTL(STYP) COND((*EQ 'CBL'))      LGLREL(*OR)
             PMTCTL     CTL(STYP) COND((*EQ 'CLLE'))     LGLREL(*OR)
             PMTCTL     CTL(STYP) COND((*EQ 'CLP'))      LGLREL(*OR)
             PMTCTL     CTL(STYP) COND((*EQ 'C'))        LGLREL(*OR)
             PMTCTL     CTL(STYP) COND((*EQ 'RPG'))      LGLREL(*OR)
             PMTCTL     CTL(STYP) COND((*EQ 'SQLCBLLE')) LGLREL(*OR)

 CMD:        PMTCTL     CTL(STYP) COND((*EQ 'CMD'))

 DSP:        PMTCTL     CTL(STYP) COND((*EQ 'DSPF'))

 LF:         PMTCTL     CTL(STYP) COND((*EQ 'LF'))

 PF:         PMTCTL     CTL(STYP) COND((*EQ 'PF'))

 C:          PMTCTL     CTL(STYP) COND((*EQ 'C'))

 TBL:        PMTCTL     CTL(STYP) COND((*EQ 'TBL'))

 OTHER:      PMTCTL     CTL(STYP) COND((*EQ 'CBLLE'))
             PMTCTL     CTL(STYP) COND((*EQ 'CBL'))      LGLREL(*OR)
             PMTCTL     CTL(STYP) COND((*EQ 'CLLE'))     LGLREL(*OR)
             PMTCTL     CTL(STYP) COND((*EQ 'CLP'))      LGLREL(*OR)
             PMTCTL     CTL(STYP) COND((*EQ 'C'))        LGLREL(*OR)
             PMTCTL     CTL(STYP) COND((*EQ 'RPG'))      LGLREL(*OR)
             PMTCTL     CTL(STYP) COND((*EQ 'SQLCBLLE')) LGLREL(*OR)
             PMTCTL     CTL(STYP) COND((*EQ 'CMD'))      LGLREL(*OR)
             PMTCTL     CTL(STYP) COND((*EQ 'DSPF'))     LGLREL(*OR)
             PMTCTL     CTL(STYP) COND((*EQ 'PRTF'))     LGLREL(*OR)
             PMTCTL     CTL(STYP) COND((*EQ 'PF'))       LGLREL(*OR)
             PMTCTL     CTL(STYP) COND((*EQ 'LF'))       LGLREL(*OR)
