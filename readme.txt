Cribbage Source Code

-------------------------------------------------------------------------------------

NOTE: This code relies on the QCardU source available through the QCard2 link.

-------------------------------------------------------------------------------------

A few comments:
- The project file (*.spr) may need to be edited for your path.
- The program was originally a port from an Amiga A-basic program (into DOS).  The
  original port source (turbo pascal) is available as Cribbage_org.pas.  It will not
  compile because of custom units needed.  The OS/2 version has a reworked analyze
  engine (and as of 2001-07-07, no more GOTOs).  The newer engine is better than the
  original.  The original OS/2 version (with GOTOs) is included as 'OldAnalyze.txt'.
- Logging functionality is available for the analyze hand's function.  To turn on,
  define LOGGININGON.  To turn off, undefine LOGGINGON.
- To compile using the QCard2 DLL, change the QCardU references to QCard2 (make sure
  the QCard2 unit is available and the QCard2.dll is on the path/current directory).
  The dll works fine as long as the configuration dialog or about dialog are not
  accessed.  If they are, then the program starts throwing exceptions.  I have no clue
  right now why this occurs.
- Included are the resources needed to compile this into Windows.  This is largely
  untested.
- Enjoy the code...

Note: Removed all forms (except the main) from the auto-create list in the projects
 settings dialog (none should have been there anyway).  2001-06-19.

 Recompiled with the card images set to 4-bit instead of 24-bit to overcome and issue
 with Sibyl's TBitmap drawing and SDD/Snap drivers (maybe others...).  2003-01-17.

P.S. If you can make money with this, more power to you.  But remember, your conscious
     will kill you if you don't give credit where credit is due :).

       Michael G. Slack (slack@attglobal.net)