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
       78  LINEDRAW VALUE 1.
       78  OS VALUE 'WINDOWS'.
       78  SELCHAR VALUE '>'.
       78  LPP VALUE '25'.
       78  LPPP VALUE '25'.
