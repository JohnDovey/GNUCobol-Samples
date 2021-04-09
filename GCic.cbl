       *> CONFIGURATION SETTINGS: Set these switches before compiling:
       *>
       *> LINEDRAW Set to:
       *>    0   To use spaces (no lines)
       *>    1   To use the line-drawing characterset (PC codepage 437)
       *>    2   To use conventional ASCII characters (+, -, |)
       *>
       *>          OSX USERS - To use the linedrawing characterset,
       *>                      set your 'terminal' font to 'Lucida Console'
       *>
       *> OS       Set to one of the following:
       *>          'CYGWIN'   For a Windows/Cygwin version
       *>          'MINGW'    For a Windows/MinGW version
       *>          'OSX'      For a Macintosh OSX version
       *>          'UNIX'     For a Unix/Linux version
       *>          'WINDOWS'  For a Native Windows version
       *>
       *> SELCHAR  Set to the desired single character to be used as the red
       *>          'feature selected' character on the screen.
       *>          SUGGESTIONS: '>', '*', '=', '+'
       *>
       *> LPP      Set to maximum printable lines per page when the listing
       *>          should be generated for LANDSCAPE orientation (can be over-
       *>          ridden at execution time using the GCXREF_LINES environment
       *>          variable.
       *>
       *> LPPP     Set to maximum printable lines per page when the listing
       *>          should be generated for PORTRAIT orientation (can be over-
       *>          ridden at execution time using the GCXREF_LINES_PORT
       *>          environment variable.
       *>
       *>*******************************************************************
       *>**  Change the settings in the copy book const-set-1.cpy to reflect
       *>    your system set up. This notes are also present there.
       *>    This is to reduce the need to amend these every time a update
       *>    for GCic occurs.
       *>*******************************************************************
       *>
       copy "const-set-1.cpy".
       *>
       *> --------------------------------------------------------------
       *> END CONFIGURATION SETTINGS
        IDENTIFICATION DIVISION.
        PROGRAM-ID. GCic.
       *>***************************************************************
       *>     >NOTE<   >NOTE<   >NOTE<   >NOTE<   >NOTE<   >NOTE<     **
       *>                                                             **
       *> If this program is compiled with '-fdebugging-line', you    **
       *> will need to pipe SYSERR to a text file when executing GCic **
       *> (by adding the text '2> filename' to the end of the GCic    **
       *> command).  You may also need to press the ENTER key when    **
       *> GCic is finished.                                           **
       *>***************************************************************
       *> This program provides a Textual User Interface (TUI) to the **
       *> process of compiling and (optionally) executing a GNU COBOL **
       *> program.                                                    **
       *>                                                             **
       *> This programs execution syntax is as follows:               **
       *>                                                             **
       *> GCic <program-path-and-filename> [ <switch>... ]            **
       *>                                                             **
       *> Once executed, a display screen will be presented showing   **
       *> the compilation options that will be used.  The user will   **
       *> have the opportunity to change options, specify new ones    **
       *> and specify any program execution arguments to be used if   **
       *> you select the 'Execute' option.  When you press the Enter  **
       *> key the program will be compiled.                           **
       *>                                                             **
       *> The SCREEN SECTION contains an image of the screen.         **
       *>                                                             **
       *> The '010-Parse-Args' section in the PROCEDURE DIVISION has  **
       *> documentation on switches and their function.               **
       *>***************************************************************
       *>                                                             **
       *> AUTHOR:       GARY L. CUTLER                                **
       *>               Copyright (C) 2009-2017, Gary L. Cutler, GPL  **
       *>                                                             **
       *> DATE-WRITTEN: June 14, 2009                                 **
       *>                                                             **
       *>***************************************************************
       *>  DATE  CHANGE DESCRIPTION                                   **
       *> ====== ==================================================== **
       *> GC0609 Don't display compiler messages file if compilation  **
       *>  GLC   Is successful.  Also don't display messages if the   **
       *>        output file is busy (just put a message on the       **
       *>        screen, leave the OC screen up & let the user fix    **
       *>        the problem & resubmit.                              **
       *> GC0709 When 'EXECUTE' is selected, a 'FILE BUSY' error will **
       *>  CLC   still cause the (old) executable to be launched.     **
       *>        Also, the 'EXTRA SWITCHES' field is being ignored.   **
       *>        Changed the title bar to lowlighted reverse video &  **
       *>        the message area to highlighted reverse-video.       **
       *> GC0809 Add a SPACE in front of command-line args when       **
       *>  GLC   executing users program.  Add a SPACE after the      **
       *>        -ftraceall switch when building cobc command.        **
       *> GC0909 Convert to work on Cygwin/Linux as well as MinGW     **
       *> GC0310 Virtualized the key codes for S-F1 thru S-F7 as they **
       *>  GLC   differ depending upon whether PDCurses or NCurses is **
       *>        being used.                                          **
       *> GC0410 Introduced the cross-reference and source listing    **
       *>  GLC   features.  Also fixed a bug in EXTRA switch proces-  **
       *>        sing where garbage will result if more than the      **
       *>        EXTRA switch is specified.                           **
       *> GC1010 Corrected several problems reported by Vince Coen:   **
       *>  GLC   1) Listing/Xref wouldn't work if '-I' additional     **
       *>           cobc switch specified.                            **
       *>        2) Programs coded with lowercase reserved words did  **
       *>           not get parsed properly when generating listing   **
       *>           and/or xref reports.                              **
       *>        3) Reliance on a TEMP environment variable caused    **
       *>           non-recoverable errors when generating listing    **
       *>           and/or xref reports in a session that lacks a     **
       *>           TEMP variable.                                    **
       *>        As a result of this change, GCic no longer runs a    **
       *>        second 'cobc' when generating listing and/or xref    **
       *>        reports.  A '-save-temps' (without '=dir') specified **
       *>        in the EXTRA options field will be ignored.  A       **
       *>        '-save-temps=dir' specified in the EXTRA options     **
       *>        field will negate both the XREF and SOURCE opts,     **
       *>        if specified.                                        **
       *> GC0711 Tailored for 29APR2011 version of GNU COBOL 2.0      **
       *> GC0712 Replaced all switches with configuration settings;   **
       *>  GLC   Tailored for 11FEB2012 version of GNU COBOL 2.0;     **
       *>        Reformatted screen layout to fit a 24x80 screen      **
       *>        rather than a 25x81 screen and to accommodate shell  **
       *>        environments having only F1-F12 (like 'terminal' in  **
       *>        OSX); Fully tested under OSX (required a few altera- **
       *>        tions); Expanded both extra-options and runtime-     **
       *>        arguments areas to TWO lines (152 chars total) each; **
       *>        Added support for MF/IBM/BS2000 listing-control      **
       *>        directives EJECT,SKIP1,SKIP2,SKIP3 (any of these in  **
       *>        copybooks will be ignored)                           **
       *> GC0313 Expand the source code record from 80 chars to 256   **
       *>  GLC   to facilitate looking for "LINKAGE SECTION" in a     **
       *>        free-format file.                                    **
       *> GC1113 Edited to support the change of "OpenCOBOL" to "GNU  **
       *>  GLC   COBOL"                                               **
       *> GC1213 Updated for 23NOV2013 version of GNU COBOL 2.1       **
       *> GC0114 Introduce a "Press ENTER to Close" action after run- **
       *>  GLC   ning the compiled program in the compiler window (F4)**
       *> VC0617 Remove the Blinking in meny screen as uncomfortable  **
       *>  VBC   Update version printed to 2.2 30JUN2017.             **
       *>        Move the system constant settings to a copy file     **
       *>        const-set-1.cpy  in case GCic is updated.            **
       *>        Added  SET ENVIRONMENT "COB_EXIT_WAIT" TO "0" to     **
       *>        100-Initialization section.                          **
       *> VC0717 Replaced compile param instrinsic=all with           **
       *>  VBC   intrinstics=ALL. Changed mod detail inits for Gary   **
       *>        from GCL to GLC.                                     **
       *>        Update version printed to 2.2 20JUL2017.             **
       *>        Should really get this from the compiler if avail?   **
       *> VC1217 Update compiler version to v3.0 24DEC2017.           **
       *>        and copyright to 2018 (in 3 places).                 **
       *>***************************************************************
       *>
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       REPOSITORY.
           FUNCTION ALL INTRINSIC.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
GC1010     SELECT F-Cobc-Output-FILE   ASSIGN TO WS-Listing-Filename-TXT
                                       ORGANIZATION IS LINE SEQUENTIAL.

           SELECT F-Source-Code-FILE   ASSIGN TO WS-File-Name-TXT
                                       ORGANIZATION IS LINE SEQUENTIAL
                                       FILE STATUS IS WS-FSM-Status-CD.
       DATA DIVISION.
       FILE SECTION.
       FD  F-Cobc-Output-FILE.
       01  F-Cobc-Output-REC                     PIC X(256).
       FD  F-Source-Code-FILE.
GC0313 01  F-Source-Code-REC                     PIC X(256).
       WORKING-STORAGE SECTION.
       COPY screenio. *> Included in Distribution
GC0712 01  WS-Compilation-Switches-TXT.
GC0712     05 WS-CS-Args-TXT VALUE SPACES.
GC0712        10 WS-CS-Arg-H1-TXT                PIC X(76).
GC0712        10 WS-CS-Arg-H2-TXT                PIC X(76).
GC0712     05 WS-CS-Filenames-TXT.
GC0712        10 VALUE 'BS2000'                  PIC X(9).
GC0712        10 VALUE 'COBOL85'                 PIC X(9).
GC0712        10 VALUE 'COBOL2002'               PIC X(9).
GC0712        10 VALUE 'DEFAULT'                 PIC X(9).
GC0712        10 VALUE 'IBM'                     PIC X(9).
GC0712        10 VALUE 'MF'                      PIC X(9).
GC0712        10 VALUE 'MVS'                     PIC X(9).
GC0712     05 WS-CS-Filenames-Table-TXT REDEFINES WS-CS-Filenames-TXT.
GC0712        10 WS-CS-Filename-TXT              OCCURS 7 TIMES
GC0712                                           PIC X(9).
GC0712 >>IF F12 < 1
GC0712     05 WS-CS-Config-NUM     VALUE 4       PIC 9(1).
GC0712 >>ELIF F12 > 7
GC0712     05 WS-CS-Config-NUM     VALUE 4       PIC 9(1).
GC0712 >>ELSE
GC0712     05 WS-CS-Config-NUM     VALUE F12     PIC 9(1).
GC0712 >>END-IF
GC0712     05 WS-CS-Extra-TXT VALUE SPACES.
GC0712        10 WS-CS-Extra-H1-TXT              PIC X(76).
GC0712        10 WS-CS-Extra-H2-TXT              PIC X(76).
GC0712     05 WS-CS-Switch-Defaults-TXT.
GC0712        10 VALUE F1                        PIC 9(1). *> WS-CS-DEBUG-CHR
GC0712        10 VALUE F4                        PIC 9(1). *> WS-CS-EXECUTE-CHR
GC0712        10 VALUE F8                        PIC 9(1). *> WS-CS-FREE-CHR
GC0712        10 VALUE F3                        PIC 9(1). *> WS-CS-LIBRARY-CHR
GC0712        10 VALUE F5                        PIC 9(1). *> WS-CS-LISTING-CHR
GC0712        10 VALUE F6                        PIC 9(1). *> WS-CS-NOFUNC-CHR
GC0712        10 VALUE F9                        PIC 9(1). *> WS-CS-NOTRUNC-CHR
GC0712        10 VALUE F2                        PIC 9(1). *> WS-CS-TRACEALL-CHR
GC0712        10 VALUE F7                        PIC 9(1). *> WS-CS-WARNALL-CHR
GC0712     05 WS-CS-All-Switches-TXT REDEFINES
GC0712                               WS-CS-Switch-Defaults-TXT.
GC0712        10 WS-CS-DEBUG-CHR                 PIC X(1).
GC0712        10 WS-CS-EXECUTE-CHR               PIC X(1).
GC0712        10 WS-CS-FREE-CHR                  PIC X(1).
GC0712        10 WS-CS-LIBRARY-CHR               PIC X(1).
GC0712        10 WS-CS-LISTING-CHR               PIC X(1).
GC0712        10 WS-CS-NOFUNC-CHR                PIC X(1).
GC0712        10 WS-CS-NOTRUNC-CHR               PIC X(1).
GC0712        10 WS-CS-TRACEALL-CHR              PIC X(1).
GC0712        10 WS-CS-WARNALL-CHR               PIC X(1).
GC0909 01  WS-Cmd-TXT                            PIC X(512).
GC0712 01  WS-Cmd-Args-TXT                       PIC X(256).
GC0712 01  WS-Cmd-End-Quote-CHR                  PIC X(1).
GC0712 01  WS-Cmd-SUB                            USAGE BINARY-LONG.
       01  WS-Cobc-Cmd-TXT                       PIC X(256).
       01  WS-Config-Fn-TXT                      PIC X(12).
GC1113 01  WS-Delete-Fn-TXT                      PIC X(256).
       01  WS-File-Name-TXT.
           05 WS-FN-CHR                          OCCURS 256 TIMES
                                                 PIC X(1).
       01  WS-File-Status-Message-TXT.
           05 VALUE 'Status Code: '              PIC X(13).
           05 WS-FSM-Status-CD                   PIC 9(2).
           05 VALUE ', Meaning: '                PIC X(11).
           05 WS-FSM-Msg-TXT                     PIC X(25).
GC0909 01  WS-Horizontal-Line-TXT                PIC X(80).
GC0909
       01  WS-I-SUB                              USAGE BINARY-LONG.
       01  WS-J-SUB                              USAGE BINARY-LONG.
GC1213 01  WS-Listing-CD VALUE F5                PIC 9(1).
GC0712 01  WS-Listing-Filename-TXT               PIC X(256).
GC1213 01  WS-Listing-TXT VALUE SPACES           PIC X(27).
       01  WS-OC-Compile-DT                      PIC XXXX/XX/XXBXX/XX.
GC0712 >>IF OS = 'CYGWIN'
GC0712 01  WS-OS-Dir-CHR         VALUE '/'       PIC X(1).
GC0712 78  WS-OS-Exe-Ext-CONST   VALUE '.exe'.
GC0712 78  WS-OS-Lib-Ext-CONST   VALUE '.dll'.
GC0712 78  WS-OS-Lib-Type-CONST  VALUE 'DLL)'.
GC0712 01  WS-OS-Type-CD         VALUE 2         PIC 9(1).
GC0712 >>ELIF OS = 'MINGW'
GC0712 01  WS-OS-Dir-CHR         VALUE '\'       PIC X(1).
GC0712 78  WS-OS-Exe-Ext-CONST   VALUE '.exe'.
GC0712 78  WS-OS-Lib-Ext-CONST   VALUE '.dll'.
GC0712 78  WS-OS-Lib-Type-CONST  VALUE 'DLL)'.
GC0712 01  WS-OS-Type-CD         VALUE 5         PIC 9(1).
GC0712 >>ELIF OS = 'OSX'
GC0712 01  WS-OS-Dir-CHR         VALUE '/'       PIC X(1).
GC0712 78  WS-OS-Exe-Ext-CONST   VALUE ' '.
GC0712 78  WS-OS-Lib-Ext-CONST   VALUE '.dylib'.
GC0712 78  WS-OS-Lib-Type-CONST  VALUE 'DYLIB)'.
GC0712 01  WS-OS-Type-CD         VALUE 4         PIC 9(1).
GC0712 >>ELIF OS = 'UNIX'
GC0712 01  WS-OS-Dir-CHR         VALUE '/'       PIC X(1).
GC0712 78  WS-OS-Exe-Ext-CONST   VALUE ' '.
GC0712 78  WS-OS-Lib-Ext-CONST   VALUE '.so'.
GC0712 78  WS-OS-Lib-Type-CONST  VALUE 'SO)'.
GC0712 01  WS-OS-Type-CD         VALUE 3         PIC 9(1).
GC0712 >>ELIF OS = 'WINDOWS'
GC0712 01  WS-OS-Dir-CHR         VALUE '\'       PIC X(1).
GC0712 78  WS-OS-Exe-Ext-CONST   VALUE '.exe'.
GC0712 78  WS-OS-Lib-Ext-CONST   VALUE '.dll'.
GC0712 78  WS-OS-Lib-Type-CONST  VALUE 'DLL)'.
GC0712 01  WS-OS-Type-CD         VALUE 1         PIC 9(1).
GC0712 >>END-IF
GC0909     88 WS-OS-Windows-BOOL VALUE 1, 5.
GC0909     88 WS-OS-Cygwin-BOOL  VALUE 2.
GC0712     88 WS-OS-UNIX-BOOL    VALUE 3, 4.
GC0712     88 WS-OS-OSX-BOOL     VALUE 4.
       01  WS-OS-Type-FILLER-TXT.
           05 VALUE 'Windows'                    PIC X(14).
           05 VALUE 'Windows/Cygwin'             PIC X(14).
           05 VALUE 'UNIX/Linux'                 PIC X(14).
           05 VALUE 'OSX'                        PIC X(14).
           05 VALUE 'Windows/MinGW'              PIC X(14).
       01  WS-OS-Types-TXT REDEFINES WS-OS-Type-FILLER-TXT.
           05 WS-OS-Type-TXT                     OCCURS 5 TIMES
                                                 PIC X(14).
       01  WS-Output-Msg-TXT                     PIC X(80).
       01  WS-Path-Delimiter-CHR                 PIC X(1).
       01  WS-Prog-Extension-TXT                 PIC X(256).
       01  WS-Prog-Folder-TXT                    PIC X(256).
GC0712 01  WS-Prog-File-Name-TXT.
GC0712     05 WS-PFN-CHR                         OCCURS 256 TIMES
GC0712                                           PIC X(1).
GC0712 01  WS-Pgm-Nm-TXT                         PIC X(31).
       01  WS-Runtime-Switches-TXT.
           05 WS-RS-Compile-OK-CHR               PIC X(1).
              88 WS-RS-Compile-OK-BOOL           VALUE 'Y'.
GC0909        88 WS-RS-Compile-OK-Warn-BOOL      VALUE 'W'.
              88 WS-RS-Compile-Failed-BOOL       VALUE 'N'.
GC0609     05 WS-RS-Complete-CHR                 PIC X(1).
GC0609        88 WS-RS-Complete-BOOL             VALUE 'Y'.
GC0609        88 WS-RS-Not-Complete-BOOL         VALUE 'N'.
GC0712     05 WS-RS-Quote-CHR                    PIC X(1).
GC0712        88 WS-RS-Double-Quote-Used-BOOL    VALUE 'Y' FALSE 'N'.
GC0809     05 WS-RS-IDENT-DIV-CHR                PIC X(1).
GC0809        88 WS-RS-1st-Prog-Complete-BOOL    VALUE 'Y'.
GC0809        88 WS-RS-More-To-1st-Prog-BOOL     VALUE 'N'.
           05 WS-RS-No-Switch-Chgs-CHR           PIC X(1).
              88 WS-RS-No-Switch-Changes-BOOL    VALUE 'Y'.
              88 WS-RS-Switch-Changes-BOOL       VALUE 'N'.
GC0709     05 WS-RS-Output-File-Busy-CHR         PIC X(1).
GC0709        88 WS-RS-Output-File-Busy-BOOL     VALUE 'Y'.
GC0709        88 WS-RS-Output-File-Avail-BOOL    VALUE 'N'.
GC0809     05 WS-RS-Source-Record-Type-CHR       PIC X(1).
GC0809        88 WS-RS-Source-Rec-Linkage-BOOL   VALUE 'L'.
GC0809        88 WS-RS-Source-Rec-Ident-BOOL     VALUE 'I'.
GC0712        88 WS-RS-Source-Rec-Ignored-BOOL   VALUE ' '.
           05 WS-RS-Switch-Error-CHR             PIC X(1).
              88 WS-RS-Switch-Is-Bad-BOOL        VALUE 'Y'.
              88 WS-RS-Switch-Is-Good-BOOL       VALUE 'N'.
       01  WS-Tally-QTY                          USAGE BINARY-LONG.
        SCREEN SECTION.
       *>
       *> Here is the layout of the GCic screen.
       *>
       *> The sample screen below shows how the screen would look if the LINEDRAW
       *> configuration setting is set to a value of 2
       *>
       *> The following sample screen layout shows how the screen looks with line-drawing
       *> characters disabled.
       *>
       *>         1         2         3         4         5         6         7         8
       *>12345678901234567890123456789012345678901234567890123456789012345678901234567890
       *>================================================================================
    01 *> GCic (2017/12/24 08:52) - GNU COBOL V3.0 24DEC2017 Interactive Compilation
    02 *>+------------------------------------------------------------------------------+
    03 *>| Folder:   E:\GNU COBOL\Samples                                               |
    04 *>| Filename: GCic.cbl                                                           |
    05 *>+------------------------------------------------------------------------------+
    06 *> Set/Clr Switches Via F1-F9; Set Config Via F12; ENTER Key Compiles; ESC Quits
    07 *>+-----------------------------------------------------------------+------------+
    08 *>| F1  Assume WITH DEBUGGING MODE  F6  "FUNCTION" Is Optional      | Current    |
    09 *>| F2  Procedure+Statement Trace   F7  Enable All Warnings         | Config:    |
    10 *>| F3  Make A Library (DLL)        F8  Source Is Free-Format       | XXXXXXXXXX |
    11 *>| F4  Execute If Compilation OK   F9  No COMP/BINARY Truncation   |            |
    12 *>| F5 >Produce Listing (Landscape)                                 |            |
    13 *>+-----------------------------------------------------------------+------------+
    14 *> Extra "cobc" Switches, If Any ("-save-temps=xxx" Prevents Listings):
    15 *>+------------------------------------------------------------------------------+
    16 *>| ____________________________________________________________________________ |
    17 *>| ____________________________________________________________________________ |
    18 *>+------------------------------------------------------------------------------+
    19 *> Program Execution Arguments, If Any:
    20 *>+------------------------------------------------------------------------------+
    21 *>| ____________________________________________________________________________ |
    22 *>| ____________________________________________________________________________ |
    23 *>+------------------------------------------------------------------------------+
    24 *> GCic Copyright (C) 2009-2018, Gary L. Cutler, GPL
       *>================================================================================
       *>12345678901234567890123456789012345678901234567890123456789012345678901234567890
       *>         1         2         3         4         5         6         7         8
       *>
       *> If this program is run on Windows, it must run with codepage 437 activated to
       *> display the line-drawing characters.  With a native Windows build or a
       *> Windows/MinGW build, one could use the command 'chcp 437' to set that codepage
       *> for display within a Windows console window (that should be the default though).
       *> With a Windows/Cygwin build, set the environment variable CYGWIN to a value of
       *> 'codepage:oem' (this cannot be done from within the program though - you will
       *> have to use the 'Computer/Advanced System Settings/Environment Variables' (Vista
       *> or Windows 7) function to define the variable.  XP Users: use 'My Computer/
       *> Properties/Advanced/Environment Variables'.
       *>
       *> OSX users may use line drawing characters in this and any GNU COBOL program
      *> simply by setting their 'terminal' application's font to "Lucida Console".
      *>
       >>IF LINEDRAW IS EQUAL TO 0
       78 LD-UL-Corner                 VALUE ' '.
       78 LD-LL-Corner                 VALUE ' '.
       78 LD-UR-Corner                 VALUE ' '.
       78 LD-LR-Corner                 VALUE ' '.
       78 LD-Upper-T                   VALUE ' '.
       78 LD-Lower-T                   VALUE ' '.
       78 LD-Horiz-Line                VALUE ' '.
       78 LD-Vert-Line                 VALUE ' '.
       >>ELIF LINEDRAW IS EQUAL TO 1
       78 LD-UL-Corner                 VALUE X'DA'.
       78 LD-LL-Corner                 VALUE X'C0'.
       78 LD-UR-Corner                 VALUE X'BF'.
       78 LD-LR-Corner                 VALUE X'D9'.
       78 LD-Upper-T                   VALUE X'C2'.
       78 LD-Lower-T                   VALUE X'C1'.
       78 LD-Horiz-Line                VALUE X'C4'.
       78 LD-Vert-Line                 VALUE X'B3'.
       >>ELSE
       78 LD-UL-Corner                 VALUE '+'.
       78 LD-LL-Corner                 VALUE '+'.
       78 LD-UR-Corner                 VALUE '+'.
       78 LD-LR-Corner                 VALUE '+'.
       78 LD-Upper-T                   VALUE '+'.
       78 LD-Lower-T                   VALUE '+'.
       78 LD-Horiz-Line                VALUE '-'.
       78 LD-Vert-Line                 VALUE '|'.
       >>END-IF
       01 S-Blank-SCR LINE 1 COLUMN 1 BLANK SCREEN.
       01 S-Switches-SCR BACKGROUND-COLOR COB-COLOR-BLACK
                         FOREGROUND-COLOR COB-COLOR-WHITE AUTO.
      *>
      *> GENERAL SCREEN FRAMEWORK
      *>
          03 BACKGROUND-COLOR COB-COLOR-BLACK
             FOREGROUND-COLOR COB-COLOR-GREEN HIGHLIGHT.
GC0712       05 LINE 02 COL 01           VALUE LD-UL-Corner.
GC0712       05         COL 02 PIC X(78) FROM  WS-Horizontal-Line-TXT.
             05         COL 80           VALUE LD-UR-Corner.
GC0712       05 LINE 03 COL 01           VALUE LD-Vert-Line.
             05         COL 80           VALUE LD-Vert-Line.
GC0712       05 LINE 04 COL 01           VALUE LD-Vert-Line.
             05         COL 80           VALUE LD-Vert-Line.
GC0712       05 LINE 05 COL 01           VALUE LD-LL-Corner.
GC0712       05         COL 02 PIC X(78) FROM  WS-Horizontal-Line-TXT.
             05         COL 80           VALUE LD-LR-Corner.
GC0712       05 LINE 07 COL 01           VALUE LD-UL-Corner.
GC0712       05         COL 02 PIC X(65) FROM  WS-Horizontal-Line-TXT.
GC0712       05         COL 67           VALUE LD-Upper-T.
GC0712       05         COL 68 PIC X(12) FROM  WS-Horizontal-Line-TXT.
             05         COL 80           VALUE LD-UR-Corner.
GC0712       05 LINE 08 COL 01           VALUE LD-Vert-Line.
GC0712       05         COL 67           VALUE LD-Vert-Line.
             05         COL 80           VALUE LD-Vert-Line.
GC0712       05 LINE 09 COL 01           VALUE LD-Vert-Line.
GC0712       05         COL 67           VALUE LD-Vert-Line.
             05         COL 80           VALUE LD-Vert-Line.
GC0712       05 LINE 10 COL 01           VALUE LD-Vert-Line.
GC0712       05         COL 67           VALUE LD-Vert-Line.
             05         COL 80           VALUE LD-Vert-Line.
GC0712       05 LINE 11 COL 01           VALUE LD-Vert-Line.
GC0712       05         COL 67           VALUE LD-Vert-Line.
             05         COL 80           VALUE LD-Vert-Line.
GC0712       05 LINE 12 COL 01           VALUE LD-Vert-Line.
GC0712       05         COL 67           VALUE LD-Vert-Line.
             05         COL 80           VALUE LD-Vert-Line.
GC0712       05 LINE 13 COL 01           VALUE LD-LL-Corner.
GC0712       05         COL 02 PIC X(65) FROM  WS-Horizontal-Line-TXT.
GC0712       05         COL 67           VALUE LD-Lower-T.
GC0712       05         COL 68 PIC X(12) FROM  WS-Horizontal-Line-TXT.
             05         COL 80           VALUE LD-LR-Corner.
GC0712       05 LINE 15 COL 01           VALUE LD-UL-Corner.
GC0712       05         COL 02 PIC X(78) FROM  WS-Horizontal-Line-TXT.
             05         COL 80           VALUE LD-UR-Corner.
GC0712       05 LINE 16 COL 01           VALUE LD-Vert-Line.
             05         COL 80           VALUE LD-Vert-Line.
GC0712       05 LINE 17 COL 01           VALUE LD-Vert-Line.
             05         COL 80           VALUE LD-Vert-Line.
GC0712       05 LINE 18 COL 01           VALUE LD-LL-Corner.
GC0712       05         COL 02 PIC X(78) FROM  WS-Horizontal-Line-TXT.
             05         COL 80           VALUE LD-LR-Corner.
GC0712       05 LINE 20 COL 01           VALUE LD-UL-Corner.
GC0712       05         COL 02 PIC X(78) FROM  WS-Horizontal-Line-TXT.
             05         COL 80           VALUE LD-UR-Corner.
GC0712       05 LINE 21 COL 01           VALUE LD-Vert-Line.
             05         COL 80           VALUE LD-Vert-Line.
GC0712       05 LINE 22 COL 01           VALUE LD-Vert-Line.
             05         COL 80           VALUE LD-Vert-Line.
GC0712       05 LINE 23 COL 01           VALUE LD-LL-Corner.
GC0712       05         COL 02 PIC X(78) FROM  WS-Horizontal-Line-TXT.
             05         COL 80           VALUE LD-LR-Corner.
      *>
      *> TOP AND BOTTOM LINES
      *>
GC0712    03 BACKGROUND-COLOR COB-COLOR-BLUE
             FOREGROUND-COLOR COB-COLOR-WHITE HIGHLIGHT.
GC0410       05 LINE 01 COL 01 VALUE ' GCic ('.
GC0410       05         COL 08 PIC X(16) FROM WS-OC-Compile-DT.
GC1213       05         COL 24 VALUE ') GNUCOBOL 3.0 24DEC2017 ' &
GC0410                               'Interactive Compilation        '.
VC0617    03 BACKGROUND-COLOR COB-COLOR-RED
GC0712       FOREGROUND-COLOR COB-COLOR-WHITE HIGHLIGHT.
GC0712       05 LINE 24 COL 01 PIC X(80) FROM WS-Output-Msg-TXT.
      *>
      *> LABELS
      *>
          03 BACKGROUND-COLOR COB-COLOR-BLACK
             FOREGROUND-COLOR COB-COLOR-CYAN  HIGHLIGHT.
GC0712       05 LINE 06 COL 02 VALUE 'Set/Clr Switches Via F1-F9; ' &
GC0712                               'Set Config Via F12; Enter Key ' &
GC0712                               'Compiles; Esc Quits'.
GC0712       05 LINE 14 COL 02 VALUE 'Extra "cobc" Switches, If Any ' &
GC0712                               '("-save-temps=xxx" Prevents ' &
GC0712                               'Listings):'.
GC0712       05 LINE 19 COL 02 VALUE 'Program Execution Arguments, ' &
GC0712                               'If Any:'.
GC0712    03 BACKGROUND-COLOR COB-COLOR-BLACK
GC0712       FOREGROUND-COLOR COB-COLOR-WHITE HIGHLIGHT.
GC0712       05 LINE 06 COL 23 VALUE 'F1'.
GC0712       05         COL 26 VALUE 'F9'.
GC0712       05         COL 45 VALUE 'F12'.
GC0712       05         COL 50 VALUE 'ENTER'.
GC0712       05         COL 70 VALUE 'ESC'.
      *>
      *> TOP SECTION BACKGROUND
      *>
          03 BACKGROUND-COLOR COB-COLOR-BLACK
             FOREGROUND-COLOR COB-COLOR-WHITE HIGHLIGHT.
GC0712       05 LINE 03 COL 62 VALUE 'Enter'.
GC0712       05 LINE 04 COL 62 VALUE 'Esc'.
          03 BACKGROUND-COLOR COB-COLOR-BLACK
             FOREGROUND-COLOR COB-COLOR-GREEN HIGHLIGHT.
GC0712       05 LINE 04 COL 03 VALUE 'Folder:   '.
GC0712       05 LINE 03 COL 03 VALUE 'Filename: '.
GC0712       05 LINE 03 COL 67 VALUE ': Compile   '.
GC0712       05 LINE 04 COL 65 VALUE ':   Quit      '.
      *>
      *> TOP SECTION PROGRAM INFO
      *>
          03 BACKGROUND-COLOR COB-COLOR-BLACK
             FOREGROUND-COLOR COB-COLOR-WHITE HIGHLIGHT.
GC0712       05 LINE 03 COL 13 PIC X(66) FROM WS-Prog-File-Name-TXT.
GC0712       05 LINE 04 COL 13 PIC X(66) FROM WS-Prog-Folder-TXT.
      *>
      *> MIDDLE LEFT SECTION F-KEYS
      *>
          03 BACKGROUND-COLOR COB-COLOR-BLACK
             FOREGROUND-COLOR COB-COLOR-WHITE HIGHLIGHT.
GC0712       05 LINE 08 COL 03 VALUE 'F1'.
GC0712       05 LINE 09 COL 03 VALUE 'F2'.
GC0712       05 LINE 10 COL 03 VALUE 'F3'.
GC0712       05 LINE 11 COL 03 VALUE 'F4'.
GC0712       05 LINE 12 COL 03 VALUE 'F5'.
GC0712       05 LINE 08 COL 35 VALUE 'F6'.
GC0712       05 LINE 09 COL 35 VALUE 'F7'.
GC0712       05 LINE 10 COL 35 VALUE 'F8'.
GC0712       05 LINE 11 COL 35 VALUE 'F9'.
      *>
      *> MIDDLE LEFT SECTION SWITCHES
      *>
          03 BACKGROUND-COLOR COB-COLOR-BLACK
             FOREGROUND-COLOR COB-COLOR-RED   HIGHLIGHT.
GC0712       05 LINE 08 COL 06 PIC X(1) FROM WS-CS-DEBUG-CHR.
GC0712       05 LINE 09 COL 06 PIC X(1) FROM WS-CS-TRACEALL-CHR.
GC0712       05 LINE 10 COL 06 PIC X(1) FROM WS-CS-LIBRARY-CHR.
GC0712       05 LINE 11 COL 06 PIC X(1) FROM WS-CS-EXECUTE-CHR.
GC0712       05 LINE 12 COL 06 PIC X(1) FROM WS-CS-LISTING-CHR.
GC0712       05 LINE 08 COL 38 PIC X(1) FROM WS-CS-NOFUNC-CHR.
GC0712       05 LINE 09 COL 38 PIC X(1) FROM WS-CS-WARNALL-CHR.
GC0712       05 LINE 10 COL 38 PIC X(1) FROM WS-CS-FREE-CHR.
GC0712       05 LINE 11 COL 38 PIC X(1) FROM WS-CS-NOTRUNC-CHR.
      *>
      *> MIDDLE LEFT SECTION BACKGROUND
      *>
          03 BACKGROUND-COLOR COB-COLOR-BLACK
             FOREGROUND-COLOR COB-COLOR-GREEN HIGHLIGHT.
GC0712       05 LINE 08 COL 07 VALUE 'Assume WITH DEBUGGING MODE'.
GC0712       05 LINE 09 COL 07 VALUE 'Procedure+Statement Trace '.
GC0712       05 LINE 10 COL 07 VALUE 'Make a Library ('.
GC0712       05         COL 23 VALUE WS-OS-Lib-Type-CONST.
GC0712       05 LINE 11 COL 07 VALUE 'Execute If Compilation OK '.
GC1213       05 LINE 12 COL 07 FROM  WS-Listing-TXT.
GC0712       05 LINE 08 COL 39 VALUE '"FUNCTION" Is Optional    '.
GC0712       05 LINE 09 COL 39 VALUE 'Enable All Warnings       '.
GC0712       05 LINE 10 COL 39 VALUE 'Source Is Free-Format     '.
GC0712       05 LINE 11 COL 39 VALUE 'No COMP/BINARY Truncation '.
      *>
      *> MIDDLE RIGHT SECTION Text
      *>
          03 BACKGROUND-COLOR COB-COLOR-BLACK
             FOREGROUND-COLOR COB-COLOR-GREEN HIGHLIGHT.
GC0712       05 LINE 08 COL 69 VALUE 'Current'.
GC0712       05 LINE 09 COL 69 VALUE 'Config:'.
      *>
      *> MIDDLE RIGHT SECTION CONFIG FILE
      *>
          03 BACKGROUND-COLOR COB-COLOR-BLACK
             FOREGROUND-COLOR COB-COLOR-WHITE HIGHLIGHT.
GC0712       05 LINE 10 COL 69 PIC X(10)
GC0712          FROM WS-CS-Filename-TXT (WS-CS-Config-NUM).
      *>
      *> FREE-FORM OPTIONS FIELDS
      *>
          03 BACKGROUND-COLOR COB-COLOR-BLACK
             FOREGROUND-COLOR COB-COLOR-WHITE HIGHLIGHT.
GC0712       05 LINE 16 COL 03 PIC X(76) USING WS-CS-Extra-H1-TXT.
GC0712       05 LINE 17 COL 03 PIC X(76) USING WS-CS-Extra-H2-TXT.
GC0712       05 LINE 21 COL 03 PIC X(76) USING WS-CS-Arg-H1-TXT.
GC0712       05 LINE 22 COL 03 PIC X(76) USING WS-CS-Arg-H2-TXT.
       PROCEDURE DIVISION.
       *>***************************************************************
       *> Legend to procedure names:                                  **
       *>                                                             **
       *> 00x-xxx   All MAIN driver procedures                        **
       *> 0xx-xxx   All GLOBAL UTILITY procedures                     **
       *> 1xx-xxx   All INITIALIZATION procedures                     **
       *> 2xx-xxx   All CORE PROCESSING procedures                    **
       *> 9xx-xxx   All TERMINATION procedures                        **
       *>***************************************************************
       DECLARATIVES.
       000-File-Error SECTION.
           USE AFTER STANDARD ERROR PROCEDURE ON F-Source-Code-FILE.
           COPY FileStat-Msgs.cpy
               REPLACING STATUS BY WS-FSM-Status-CD
                         MSG    BY WS-FSM-Msg-TXT.
           MOVE SPACES TO WS-Output-Msg-TXT
           IF WS-FSM-Status-CD = 35
               DISPLAY
                   'File not found: "'
                   TRIM(WS-File-Name-TXT,TRAILING)
                   '"'
           ELSE
               DISPLAY
                   'Error accessing file: "'
                   TRIM(WS-File-Name-TXT,TRAILING)
                   '"'
           END-IF
           GOBACK
           .
       END DECLARATIVES.
       000-Main SECTION.
           PERFORM 100-Initialization
GC0609     SET WS-RS-Not-Complete-BOOL TO TRUE
GC0609     PERFORM UNTIL WS-RS-Complete-BOOL
GC0609         PERFORM 200-Let-User-Set-Switches
GC0609         PERFORM 210-Run-Compiler
GC0410         IF (WS-RS-Compile-OK-BOOL OR WS-RS-Compile-OK-Warn-BOOL)
GC0712         AND (WS-CS-LISTING-CHR > SPACE)
GC0712             DISPLAY S-Blank-SCR
GC0410             PERFORM 220-Make-Listing
GC0410         END-IF
GC0709         IF  (WS-CS-EXECUTE-CHR NOT = SPACES)
GC0709         AND (WS-RS-Output-File-Avail-BOOL)
GC0609             PERFORM 230-Run-Program
GC0609         END-IF
GC0712         PERFORM 250-Autoload-Listing
GC0609     END-PERFORM
           PERFORM 900-Terminate
      * -- Control will NOT return
           .
      *>***************************************************************
      *> Perform all program-wide initialization operations          **
      *>***************************************************************
       100-Initialization SECTION.
      *>***************************************************************
      *> Make sure full screen-handling is in effect                 **
      *>***************************************************************
           SET ENVIRONMENT 'COB_SCREEN_EXCEPTIONS' TO 'Y'
           SET ENVIRONMENT 'COB_SCREEN_ESC'        TO 'Y'
VC0617     SET ENVIRONMENT "COB_EXIT_WAIT"         TO "0".
      *>***************************************************************
      *> Get GCic Compilation Date/Time                              **
      *>***************************************************************
           MOVE WHEN-COMPILED (1:12) TO WS-OC-Compile-DT
           INSPECT WS-OC-Compile-DT
               REPLACING ALL '/' BY ':'
               AFTER INITIAL SPACE
      *>***************************************************************
      *> Convert WS-CS-All-Switches-TXT to Needed Alphanumeric Values**
      *>***************************************************************
           INSPECT WS-CS-All-Switches-TXT
               REPLACING ALL '0' BY SPACE
                         ALL '1' BY SELCHAR
GC1213                   ALL '2' BY SELCHAR
      *>***************************************************************
      *> Process filename (the only command-line argument)           **
      *>***************************************************************
GC0712     ACCEPT WS-Cmd-Args-TXT FROM COMMAND-LINE
GC0712     MOVE 1 TO WS-Cmd-SUB
GC0712     IF WS-Cmd-Args-TXT(WS-Cmd-SUB:1) = '"' OR "'"
GC0712         MOVE WS-Cmd-Args-TXT(WS-Cmd-SUB:1)
GC0712           TO WS-Cmd-End-Quote-CHR
GC0712         ADD 1 TO WS-Cmd-SUB
GC0712         UNSTRING WS-Cmd-Args-TXT
GC0712             DELIMITED BY WS-Cmd-End-Quote-CHR
GC0712             INTO WS-File-Name-TXT
GC0712             WITH POINTER WS-Cmd-SUB
GC0712     ELSE
GC0712         UNSTRING WS-Cmd-Args-TXT
GC0712             DELIMITED BY ALL SPACES
GC0712             INTO WS-File-Name-TXT
GC0712             WITH POINTER WS-Cmd-SUB
GC0712     END-IF
           IF WS-File-Name-TXT = SPACES
GC0712         DISPLAY 'No program filename was specified'
               PERFORM 900-Terminate
      * ------ Control will NOT return
           END-IF
      *>***************************************************************
      *> Determine if 'Make A Library' feature should be forced 'ON' **
      *>***************************************************************
           PERFORM 240-Find-LINKAGE-SECTION
      *>***************************************************************
      *> Split 'WS-File-Name-TXT' into 'WS-Prog-Folder-TXT' and      **
      *> 'WS-Prog-File-Name-TXT'                                     **
      *>***************************************************************
GC0909     IF WS-OS-Cygwin-BOOL AND WS-File-Name-TXT (2:1) = ':'
GC0712         MOVE '\' TO WS-OS-Dir-CHR
GC0909     END-IF
GC0712     MOVE LENGTH(WS-File-Name-TXT) TO WS-I-SUB
GC0712     PERFORM UNTIL WS-I-SUB = 0
GC0712     OR WS-FN-CHR (WS-I-SUB) = WS-OS-Dir-CHR
               SUBTRACT 1 FROM WS-I-SUB
           END-PERFORM
           IF WS-I-SUB = 0
               MOVE SPACES    TO WS-Prog-Folder-TXT
               MOVE WS-File-Name-TXT TO WS-Prog-File-Name-TXT
           ELSE
               MOVE '*' TO WS-FN-CHR (WS-I-SUB)
               UNSTRING WS-File-Name-TXT DELIMITED BY '*'
                   INTO WS-Prog-Folder-TXT
                        WS-Prog-File-Name-TXT
GC0712         MOVE WS-OS-Dir-CHR TO WS-FN-CHR (WS-I-SUB)
           END-IF
           IF WS-Prog-Folder-TXT = SPACES
               ACCEPT WS-Prog-Folder-TXT FROM ENVIRONMENT 'CD'
GC0909     ELSE
GC0909         CALL 'CBL_CHANGE_DIR'
GC0909             USING TRIM(WS-Prog-Folder-TXT,TRAILING)
           END-IF
GC0909     IF WS-OS-Cygwin-BOOL AND WS-File-Name-TXT (2:1) = ':'
GC0712         MOVE '/' TO WS-OS-Dir-CHR
GC0909     END-IF
      *>***************************************************************
      *> Split 'WS-Prog-File-Name-TXT' into 'WS-Pgm-Nm-TXT' &        **
      *> 'WS-Prog-Extension-TXT'                                     **
      *>***************************************************************
GC0712     MOVE LENGTH(WS-Prog-File-Name-TXT) TO WS-I-SUB
GC0712     PERFORM UNTIL WS-I-SUB = 0
GC0712     OR WS-PFN-CHR (WS-I-SUB) = '.'
GC0712         SUBTRACT 1 FROM WS-I-SUB
GC0712     END-PERFORM
GC0712     IF WS-I-SUB = 0
GC0712         MOVE WS-Prog-File-Name-TXT TO WS-Pgm-Nm-TXT
GC0712         MOVE SPACES         TO WS-Prog-Extension-TXT
GC0712     ELSE
GC0712         MOVE '*' TO WS-PFN-CHR (WS-I-SUB)
GC0712         UNSTRING WS-Prog-File-Name-TXT DELIMITED BY '*'
GC0712             INTO WS-Pgm-Nm-TXT
GC0712                  WS-Prog-Extension-TXT
GC0712         MOVE '.' TO WS-PFN-CHR (WS-I-SUB)
GC0712     END-IF
      *>***************************************************************
      *> Build initial Line 24 Message                               **
      *>***************************************************************
GC0909     MOVE ALL LD-Horiz-Line TO WS-Horizontal-Line-TXT.
GC0410     MOVE CONCATENATE(' GCic for '
GC0410                      TRIM(WS-OS-Type-TXT(WS-OS-Type-CD),Trailing)
GC1213                      ' Copyright (C) 2009-2018, Gary L. '
GC0410                      'Cutler, GPL')
GC0410       TO WS-Output-Msg-TXT.
GC0909
      *>***************************************************************
      *> Show the user the current switch settings and allow them to **
      *> be changed.                                                 **
      *>***************************************************************
       200-Let-User-Set-Switches SECTION.
           SET WS-RS-Switch-Changes-BOOL TO TRUE
           PERFORM UNTIL WS-RS-No-Switch-Changes-BOOL
GC1213         EVALUATE WS-Listing-CD
GC1213         WHEN 0
GC1213             MOVE 'Listing Off'            TO WS-Listing-TXT
GC1213             MOVE SPACE                    TO WS-CS-LISTING-CHR
GC1213         WHEN 1
GC1213             MOVE 'Listing On (Landscape)' TO WS-Listing-TXT
GC1213             MOVE SELCHAR                  TO WS-CS-LISTING-CHR
GC1213         WHEN 2
GC1213             MOVE 'Listing On (Portrait)' TO WS-Listing-TXT
GC1213             MOVE SELCHAR                  TO WS-CS-LISTING-CHR
GC1213         END-EVALUATE
               ACCEPT S-Switches-SCR
               IF COB-CRT-STATUS > 0
                   EVALUATE COB-CRT-STATUS
                       WHEN COB-SCR-F1
                           IF WS-CS-DEBUG-CHR = SPACE
GC0712                         MOVE SELCHAR TO WS-CS-DEBUG-CHR
                           ELSE
                               MOVE ' ' TO WS-CS-DEBUG-CHR
                           END-IF
GC0712                 WHEN COB-SCR-F2
GC0712                     IF  WS-CS-TRACEALL-CHR = SPACE
GC0712                         MOVE SELCHAR TO WS-CS-TRACEALL-CHR
GC0712                     ELSE
GC0712                         MOVE ' ' TO WS-CS-TRACEALL-CHR
GC0712                     END-IF
                       WHEN COB-SCR-F3
GC0712                     IF WS-CS-LIBRARY-CHR = SPACE
GC0712                         MOVE SELCHAR TO WS-CS-LIBRARY-CHR
                           ELSE
GC0712                         MOVE ' ' TO WS-CS-LIBRARY-CHR
                           END-IF
                       WHEN COB-SCR-F4
                           IF  WS-CS-EXECUTE-CHR = SPACE
GC0712                         MOVE SELCHAR TO WS-CS-EXECUTE-CHR
                           ELSE
                               MOVE ' ' TO WS-CS-EXECUTE-CHR
                           END-IF
GC0712                 WHEN COB-SCR-F5
GC1213                     ADD 1 TO WS-Listing-CD
GC1213                     IF WS-Listing-CD > 2
GC1213                         MOVE 0 TO WS-Listing-CD
GC1213                     END-IF
GC0712                 WHEN COB-SCR-F6
GC0712                     IF WS-CS-NOFUNC-CHR = SPACE
GC0712                         MOVE SELCHAR TO WS-CS-NOFUNC-CHR
GC0712                     ELSE
GC0712                         MOVE ' ' TO WS-CS-NOFUNC-CHR
GC0712                     END-IF
GC0712                 WHEN COB-SCR-F7
GC0712                     IF WS-CS-WARNALL-CHR = SPACE
GC0712                         MOVE SELCHAR TO WS-CS-WARNALL-CHR
GC0712                     ELSE
GC0712                         MOVE ' ' TO WS-CS-WARNALL-CHR
GC0712                     END-IF
GC0712                 WHEN COB-SCR-F8
GC0712                     IF WS-CS-FREE-CHR = SPACE
GC0712                         MOVE SELCHAR TO WS-CS-FREE-CHR
GC0712                     ELSE
GC0712                         MOVE ' ' TO WS-CS-FREE-CHR
GC0712                     END-IF
GC0712                 WHEN COB-SCR-F9
GC0712                     IF  WS-CS-NOTRUNC-CHR = SPACE
GC0712                         MOVE SELCHAR TO WS-CS-NOTRUNC-CHR
GC0712                     ELSE
GC0712                         MOVE ' ' TO WS-CS-NOTRUNC-CHR
GC0712                     END-IF
                       WHEN COB-SCR-ESC
                           PERFORM 900-Terminate
      * ------------------ Control will NOT return
GC0712                 WHEN COB-SCR-F12
GC0712                     ADD 1 TO WS-CS-Config-NUM
GC0712                     IF WS-CS-Config-NUM > 7
GC0712                         MOVE 1 TO WS-CS-Config-NUM
GC0712                     END-IF
                       WHEN OTHER
                           MOVE 'An unsupported key was pressed'
                             TO WS-Output-Msg-TXT
                   END-EVALUATE
               ELSE
                   SET WS-RS-No-Switch-Changes-BOOL TO TRUE
               END-IF
           END-PERFORM
           .
      *>***************************************************************
      *> Run the compiler using the switch settings we've prepared.  **
      *>***************************************************************
       210-Run-Compiler SECTION.
           MOVE SPACES TO WS-Cmd-TXT
                          WS-Cobc-Cmd-TXT
                          WS-Output-Msg-TXT
           DISPLAY S-Switches-SCR
           MOVE 1 TO WS-I-SUB
GC0712     MOVE LOWER-CASE(WS-CS-Filename-TXT (WS-CS-Config-NUM))
GC0712       TO WS-Config-Fn-TXT
      *>***************************************************************
      *> Build the 'cobc' command                                    **
      *>***************************************************************
GC0909     MOVE SPACES TO WS-Cobc-Cmd-TXT
GC0909     STRING 'cobc -v -std='
GC0909         TRIM(WS-Config-Fn-TXT,TRAILING)
GC0909         ' '
GC0909         INTO WS-Cobc-Cmd-TXT
GC0909         WITH POINTER WS-I-SUB
           IF WS-CS-LIBRARY-CHR NOT = ' '
               STRING '-m '
                   DELIMITED SIZE INTO WS-Cobc-Cmd-TXT
                   WITH POINTER WS-I-SUB
           ELSE
               STRING '-x '
                   DELIMITED SIZE INTO WS-Cobc-Cmd-TXT
                   WITH POINTER WS-I-SUB
           END-IF
           IF WS-CS-DEBUG-CHR NOT = ' '
               STRING '-fdebugging-line '
                   DELIMITED SIZE INTO WS-Cobc-Cmd-TXT
                   WITH POINTER WS-I-SUB
           END-IF
           IF WS-CS-NOTRUNC-CHR NOT = ' '
               STRING '-fnotrunc '
                   DELIMITED SIZE INTO WS-Cobc-Cmd-TXT
                   WITH POINTER WS-I-SUB
           END-IF
           IF WS-CS-TRACEALL-CHR NOT = ' '
GC0809         STRING '-ftraceall '
                   DELIMITED SIZE INTO WS-Cobc-Cmd-TXT
                   WITH POINTER WS-I-SUB
           END-IF
GC0712     IF WS-CS-NOFUNC-CHR NOT = ' '
VC0717         STRING '-fintrinsics=ALL '
GC0712             DELIMITED SIZE INTO WS-Cobc-Cmd-TXT
GC0712             WITH POINTER WS-I-SUB
GC0712     END-IF
GC0712     IF WS-CS-WARNALL-CHR NOT = ' '
GC0712         STRING '-Wall '
GC0712             DELIMITED SIZE INTO WS-Cobc-Cmd-TXT
GC0712             WITH POINTER WS-I-SUB
GC0712     END-IF
GC0712     IF WS-CS-FREE-CHR NOT = ' '
GC0712         STRING '-free '
GC0712             DELIMITED SIZE INTO WS-Cobc-Cmd-TXT
GC0712             WITH POINTER WS-I-SUB
GC0712     ELSE
GC0712         STRING '-fixed '
GC0712             DELIMITED SIZE INTO WS-Cobc-Cmd-TXT
GC0712             WITH POINTER WS-I-SUB
GC0712     END-IF
GC0712     MOVE 0 TO WS-Tally-QTY
GC0712     INSPECT WS-CS-Extra-TXT
GC0712         TALLYING WS-Tally-QTY FOR ALL '-save-temps'
GC0712     IF WS-CS-LISTING-CHR > SPACE
GC0712     AND WS-Tally-QTY > 0
GC0712         MOVE SPACE TO WS-CS-LISTING-CHR *> Can't generate listing
                                               *> if -save-temps used
GC0712     END-IF
GC0712     IF WS-CS-LISTING-CHR > SPACE
GC1010         STRING '-save-temps '
GC1010             DELIMITED SIZE INTO WS-Cobc-Cmd-TXT
GC1010             WITH POINTER WS-I-SUB
GC1010     END-IF
GC0709     IF WS-CS-Extra-TXT > SPACES
GC0709         STRING ' '
GC0709                TRIM(WS-CS-Extra-TXT,TRAILING)
GC0709                ' '
GC0709                DELIMITED SIZE INTO WS-Cobc-Cmd-TXT
GC0709                WITH POINTER WS-I-SUB
GC0709     END-IF
GC0909     STRING TRIM(WS-Prog-File-Name-TXT,TRAILING)
GC0909         DELIMITED SIZE INTO WS-Cobc-Cmd-TXT
GC0909         WITH POINTER WS-I-SUB
      *>***************************************************************
      *> Prepare the compilation listing file                        **
      *>***************************************************************
GC1113     MOVE CONCATENATE(TRIM(WS-Pgm-Nm-TXT,Trailing),'.gclst')
GC0712       TO WS-Listing-Filename-TXT
GC0712     CALL 'CBL_DELETE_FILE' USING WS-Listing-Filename-TXT
      *>***************************************************************
      *> Now execute the 'cobc' command                              **
      *>***************************************************************
GC0410     MOVE ' Compiling...' TO WS-Output-Msg-TXT
GC0410     DISPLAY S-Switches-SCR
GC0609     SET WS-RS-Output-File-Avail-BOOL TO TRUE
           MOVE SPACES TO WS-Cmd-TXT
           STRING TRIM(WS-Cobc-Cmd-TXT,TRAILING)
GC0712            ' >' WS-Listing-Filename-TXT
GC0712            ' 2>&1'
                  DELIMITED SIZE
                  INTO WS-Cmd-TXT
DEBUG D    DISPLAY WS-Cmd-TXT UPON SYSERR
           CALL 'SYSTEM' USING TRIM(WS-Cmd-TXT,TRAILING)
GC0712     OPEN EXTEND F-Cobc-Output-FILE
GC0712     WRITE F-Cobc-Output-REC FROM SPACES
GC0712     IF RETURN-CODE = 0
GC0712         SET WS-RS-Compile-OK-BOOL TO TRUE
GC0712         MOVE ' Compilation Was Successful' TO WS-Output-Msg-TXT
GC0712         MOVE CONCATENATE('GNU COBOL',WS-Output-Msg-TXT)
GC0712           TO F-Cobc-Output-REC
GC0712         WRITE F-Cobc-Output-REC
GC0712         SET WS-RS-Complete-BOOL TO TRUE
GC0712     ELSE
GC0712         SET WS-RS-Compile-Failed-BOOL TO TRUE
GC0712         MOVE CONCATENATE(' Compilation Failed - See ',
GC0712                     TRIM(WS-Listing-Filename-TXT,Trailing))
GC0712           TO WS-Output-Msg-TXT
GC0712         MOVE 'GNU COBOL Compilation HAS FAILED - See Above'
GC0712           TO F-Cobc-Output-REC
GC0712         WRITE F-Cobc-Output-REC
GC0712     END-IF
GC0712     CLOSE F-Cobc-Output-FILE
GC0712     DISPLAY S-Switches-SCR
GC0712     CALL 'C$SLEEP' USING 2
GC0712     MOVE SPACES TO WS-Output-Msg-TXT
           IF WS-RS-Compile-Failed-BOOL
GC0712         PERFORM 250-Autoload-Listing
               PERFORM 900-Terminate
      *> ----- Control will not return
           END-IF
           .
      *>***************************************************************
      *> Generate a source + xref listing using 'LISTING' subroutine **
      *>***************************************************************
GC0410 220-Make-Listing SECTION.
GC0410     MOVE ' Generating listing...' TO WS-Output-Msg-TXT
GC0410     DISPLAY S-Switches-SCR
GC0410     MOVE 0 TO RETURN-CODE
      *>***************************************************************
      *> Create the listing                                          **
      *>***************************************************************
GC0410     MOVE SPACES TO WS-Output-Msg-TXT
GC0410     CALL 'LISTING' USING WS-Listing-Filename-TXT
GC0712                          WS-File-Name-TXT
GC0712                          WS-OS-Type-CD
GC1213                          LPP
GC1213                          LPPP
GC1213                          WS-Listing-CD
GC0410     ON EXCEPTION
GC0410         MOVE ' LISTING module is not available'
GC0410           TO WS-Output-Msg-TXT
GC0410         MOVE 1 TO RETURN-CODE
GC0410     END-CALL
GC0410     IF RETURN-CODE = 0
GC0712         MOVE ' Source+Xref listing generated '
GC0712           TO WS-Output-Msg-TXT
GC0410     END-IF
GC0712     DISPLAY S-Switches-SCR
GC0712     CALL 'C$SLEEP' USING 2
GC0712     PERFORM 250-Autoload-Listing
GC0410     .
      *>***************************************************************
      *> Run the compiled program                                    **
      *>***************************************************************
       230-Run-Program SECTION.
GC0114     MOVE ' Preparing to run program ... press ENTER to close '
GC0114       TO WS-Output-Msg-TXT
GC0114     DISPLAY S-Switches-SCR
GC0114     CALL 'C$SLEEP' USING 3
GC0909     MOVE SPACES TO WS-Cmd-TXT
GC0909     MOVE 1 TO WS-I-SUB
      *>***************************************************************
      *> If necessary, start with 'cobcrun' command                  **
      *>***************************************************************
GC0712     IF WS-CS-LIBRARY-CHR NOT = ' '
               STRING 'cobcrun ' DELIMITED SIZE
                      INTO WS-Cmd-TXT
                      WITH POINTER WS-I-SUB
           END-IF
      *>***************************************************************
      *> Add any necessary path prefix                               **
      *>***************************************************************
GC0712     SET WS-RS-Double-Quote-Used-BOOL TO FALSE
           IF WS-Prog-Folder-TXT NOT = SPACES
GC0909         IF WS-OS-Cygwin-BOOL AND WS-Prog-Folder-TXT (2:1) = ':'
GC0909             STRING '/cygdrive/'
GC0909                 INTO WS-Cmd-TXT
GC0909                 WITH POINTER WS-I-SUB
GC0909             STRING LOWER-CASE(WS-Prog-Folder-TXT (1:1))
GC0909                 INTO WS-Cmd-TXT
GC0909                 WITH POINTER WS-I-SUB
GC0909             PERFORM
GC0909                 VARYING WS-J-SUB FROM 3 BY 1
GC0909                 UNTIL WS-J-SUB > LENGTH(TRIM(WS-Prog-Folder-TXT))
GC0909                 IF WS-Prog-Folder-TXT (WS-J-SUB:1) = '\'
GC0909                     STRING '/'
GC0909                         INTO WS-Cmd-TXT
GC0909                         WITH POINTER WS-I-SUB
GC0909                 ELSE
GC0909                     STRING WS-Prog-Folder-TXT (WS-J-SUB:1)
GC0909                         INTO WS-Cmd-TXT
GC0909                         WITH POINTER WS-I-SUB
GC0909                 END-IF
GC0909             END-PERFORM
GC0909         ELSE
GC0410             STRING '"' TRIM(WS-Prog-Folder-TXT,TRAILING)
GC0909                 INTO WS-Cmd-TXT
GC0909                 WITH POINTER WS-I-SUB
GC0712             SET WS-RS-Double-Quote-Used-BOOL TO TRUE
GC0909         END-IF
GC0712         STRING WS-OS-Dir-CHR
GC0909             INTO WS-Cmd-TXT
GC0909             WITH POINTER WS-I-SUB
GC0909     ELSE
GC0909         IF WS-OS-Cygwin-BOOL OR WS-OS-UNIX-BOOL
GC0909             STRING './'
GC0909                 INTO WS-Cmd-TXT
GC0909                 WITH POINTER WS-I-SUB
GC0909         END-IF
           END-IF
      *>***************************************************************
      *> Insert program filename                                     **
      *>***************************************************************
GC0909     STRING TRIM(WS-Pgm-Nm-TXT,TRAILING)
GC0909         INTO WS-Cmd-TXT
GC0909         WITH POINTER WS-I-SUB
      *>***************************************************************
      *> Insert proper extension                                     **
      *>***************************************************************
GC0712     IF WS-CS-LIBRARY-CHR = ' '
GC0712         IF WS-OS-Exe-Ext-CONST > ' '
GC0712             STRING WS-OS-Exe-Ext-CONST DELIMITED SPACE
GC0712                 INTO WS-Cmd-TXT
GC0712                 WITH POINTER WS-I-SUB
GC0712         END-IF
GC0712     ELSE
GC0712         IF WS-OS-Lib-Ext-CONST > ' '
GC0712             STRING WS-OS-Lib-Ext-CONST DELIMITED SPACE
GC0712                 INTO WS-Cmd-TXT
GC0712                 WITH POINTER WS-I-SUB
GC0712         END-IF
GC0712     END-IF
GC0712     IF WS-RS-Double-Quote-Used-BOOL
GC0712         STRING '"' DELIMITED SIZE
GC0712             INTO WS-Cmd-TXT
GC0712             WITH POINTER WS-I-SUB
GC0712     END-IF
           IF WS-CS-Args-TXT NOT = SPACES
GC0809         STRING ' ' TRIM(WS-CS-Args-TXT,TRAILING)
                   INTO WS-Cmd-TXT
                   WITH POINTER WS-I-SUB
           END-IF
      *>***************************************************************
      *> Run the program                                             **
      *>***************************************************************
GC0114     CALL X'E4'
           CALL 'SYSTEM' USING TRIM(WS-Cmd-TXT,TRAILING)
GC0712     MOVE SPACES TO WS-Output-Msg-TXT
GC0114     ACCEPT WS-Output-Msg-TXT(1:1) AT 0101
           PERFORM 900-Terminate
      * -- Control will NOT return
           .
      *>***************************************************************
      *> Determine if the program being compiled is a MAIN program   **
      *>***************************************************************
       240-Find-LINKAGE-SECTION SECTION.
           OPEN INPUT F-Source-Code-FILE
GC0712     MOVE ' ' TO WS-CS-LIBRARY-CHR
           SET WS-RS-More-To-1st-Prog-BOOL   TO TRUE
           PERFORM UNTIL WS-RS-1st-Prog-Complete-BOOL
               READ F-Source-Code-FILE AT END
                   CLOSE F-Source-Code-FILE
                   EXIT SECTION
               END-READ
GC0712         CALL 'CHECKSRC'
GC0712             USING BY CONTENT   F-Source-Code-REC
GC0712                   BY REFERENCE WS-RS-Source-Record-Type-CHR
               IF WS-RS-Source-Rec-Ident-BOOL
                   SET WS-RS-1st-Prog-Complete-BOOL TO TRUE
               END-IF
           END-PERFORM
GC0712     SET WS-RS-Source-Rec-Ignored-BOOL TO TRUE
           PERFORM UNTIL WS-RS-Source-Rec-Linkage-BOOL
                      OR WS-RS-Source-Rec-Ident-BOOL
               READ F-Source-Code-FILE AT END
                   CLOSE F-Source-Code-FILE
                   EXIT SECTION
               END-READ
GC0712         CALL 'CHECKSRC'
GC0712             USING BY CONTENT   F-Source-Code-REC
GC0712                   BY REFERENCE WS-RS-Source-Record-Type-CHR
           END-PERFORM
           CLOSE F-Source-Code-FILE
           IF WS-RS-Source-Rec-Linkage-BOOL
GC0712         MOVE SELCHAR TO WS-CS-LIBRARY-CHR
           END-IF
           .
GC0712*>***************************************************************
GC0712*> Attempt to open the listing file as a command.  This will - **
GC1113*> if the user has associated filetype/extension 'gclst' with  **
GC0712*> an application - invoke the appropriate application to      **
GC0712*> allow the user to view the listing.                         **
GC0712*>***************************************************************'
GC0712 250-Autoload-Listing SECTION.
GC0712     EVALUATE TRUE
GC0712         WHEN WS-OS-Windows-BOOL OR WS-OS-Cygwin-BOOL
GC0712             MOVE SPACES TO WS-Cmd-TXT
GC0712             STRING
GC0712                 'cmd /c '
GC0712                 TRIM(WS-Listing-Filename-TXT,TRAILING)
GC0712                 DELIMITED SIZE INTO WS-Cmd-TXT
GC0712             CALL 'SYSTEM' USING TRIM(WS-Cmd-TXT,TRAILING)
GC0712         WHEN WS-OS-OSX-BOOL
GC0712             MOVE SPACES TO WS-Cmd-TXT
GC0712             STRING
GC0712                 'open -t '
GC0712                 TRIM(WS-Listing-Filename-TXT,TRAILING)
GC0712                 DELIMITED SIZE INTO WS-Cmd-TXT
GC0712             CALL 'SYSTEM' USING TRIM(WS-Cmd-TXT,TRAILING)
GC0712     END-EVALUATE
GC0712*>   ************************************************************
GC0712*>   ** Since we had to do our own '-save-temps' when we       **
GC0712*>   ** compiled (in order to generate the cross-reference     **
GC0712*>   ** listing) we now need to clean up after ourselves.      **
GC0712*>   ************************************************************
GC1112     DISPLAY S-Blank-SCR
GC0712     IF WS-OS-Windows-BOOL
GC0712         MOVE CONCATENATE('del ',TRIM(WS-Pgm-Nm-TXT,TRAILING))
GC0712           TO WS-Cmd-TXT
GC0712     ELSE
GC0712         MOVE CONCATENATE('rm ',TRIM(WS-Pgm-Nm-TXT,TRAILING))
GC0712           TO WS-Cmd-TXT
GC0712     END-IF
GC0712     CALL 'SYSTEM'
GC0712         USING CONCATENATE(TRIM(WS-Cmd-TXT,TRAILING),'.c')
GC0712     CALL 'SYSTEM'
GC0712         USING CONCATENATE(TRIM(WS-Cmd-TXT,TRAILING),'.c.h')
GC0712     CALL 'SYSTEM'
GC0712         USING CONCATENATE(TRIM(WS-Cmd-TXT,TRAILING),'.c.l*.h')
GC0712     CALL 'SYSTEM'
GC0712         USING CONCATENATE(TRIM(WS-Cmd-TXT,TRAILING),'.i')
GC0712     CALL 'SYSTEM'
GC0712         USING CONCATENATE(TRIM(WS-Cmd-TXT,TRAILING),'.o')
GC0712     .
      *> Display a message and halt the program                      **
      *>***************************************************************
       900-Terminate SECTION.
GC0909     IF WS-Output-Msg-TXT > SPACES
GC0909         DISPLAY S-Switches-SCR
GC0909         CALL 'C$SLEEP' USING 2
GC0909     END-IF
           DISPLAY S-Blank-SCR
           STOP RUN
           .
       END PROGRAM GCic.
