             CMD        PROMPT('TEILDATEI ÖFFNEN')

/*‚          SOURCE-EINSTELLUNGEN                                    */
             PARM       KWD(SLIB) TYPE(*CHAR) LEN(10) MIN(1)         +
                         CHOICE('Name, *CURLIB')                     +
                         PROMPT('Bibliothek') KEYPARM(*YES)
             PARM       KWD(SFILE) TYPE(*CHAR) LEN(10) MIN(1)        +
                         CHOICE('Name') PROMPT('Quellendatei')       +
                         KEYPARM(*YES)
             PARM       KWD(SMBR) TYPE(*CHAR) LEN(10)                +
                         MIN(1) CHOICE('Name') KEYPARM(*YES)         +
                         PROMPT('  Teildatei')

/*‚          ZEILE / OPTION                                          */
             PARM       KWD(SECNBR) TYPE(*CHAR) LEN(6) RSTD(*NO)     +
                         MIN(0) CHOICE(*NONE) DFT(000000)            +
                         DSPINPUT(*NO)
             PARM       KWD(OPTION) TYPE(*CHAR) LEN(1) RSTD(*NO)     +
                         MIN(0) MAX(1) DFT(5)                        +
                         CHOICE('(2=Ändern/5=Anzeigen)')             +
                         PROMPT('Auswahl')
/*‚          F3/F12                                                  */
             PARM       KWD(XF3) TYPE(*CHAR) LEN(1) RSTD(*NO)        +
                         MIN(0) DSPINPUT(*NO) CHOICE(*NONE)
             PARM       KWD(XF12) TYPE(*CHAR) LEN(1) RSTD(*NO)       +
                         MIN(0) DSPINPUT(*NO) CHOICE(*NONE)
