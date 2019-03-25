UNIT CribSup;

(*                                                                      *)
(* AUTHOR: Michael G. Slack                    DATE WRITTEN: 2001/03/09 *)
(* ENVIRONMENT: Sibyl                                                   *)
(*                                                                      *)
(* Unit defines the global constants and variables used by the cribbage *)
(* program.                                                             *)
(* All messages are defined here to allow easy conversion to other      *)
(* languages if wanted.                                                 *)
(*                                                                      *)
(* -------------------------------------------------------------------- *)
(*                                                                      *)
(* REVISED: 2001/07/07 - Added version string constant for about dialog.*)
(*          2001/07/08 - Changed version number.                        *)
(*          2003-01-17 -  " " " " " " " " " " "                         *)
(*                                                                      *)

INTERFACE

 USES Graphics, ExtCtrls, Messages, QCardU;

 TYPE TCRIBBOARD = ARRAY[0..4, 0..39] OF INTEGER;
      TCARDHAND  = ARRAY[1..6] OF LONGWORD;

 CONST wm_StartUp     = wm_User + 101;
       wm_AboutSUp    = wm_User + 102;
       wm_CompsPlay   = wm_User + 104;
       wm_PlayersPlay = wm_User + 105;
       wm_ScoreHand   = wm_User + 106;
       {ini string constants (application/key)}
       Ini_Pos      : STRING[14] = 'WindowPosition';
       Ini_PTop     : STRING[3]  = 'Top';
       Ini_PLeft    : STRING[4]  = 'Left';
       Ini_Opt      : STRING[7]  = 'Options';
       Ini_OCPic    : STRING[11] = 'CardBackPic';
       Ini_OPrmt    : STRING[9]  = 'PromptsOn';
       Ini_OAlways  : STRING[11] = 'AlwaysStart';
       Ini_OUseSnd  : STRING[9]  = 'UseSounds';
       Ini_OVerbose : STRING[11] = 'VerboseMode';
       {global options/default values}
       CardBackPic  : LONGWORD = Min_CardBackNum;
       PromptMe     : BOOLEAN  = FALSE; // verbose mode also
       AlwaysStarts : BOOLEAN  = FALSE;
       UseSounds    : BOOLEAN  = TRUE;
       VerboseMode  : BOOLEAN  = TRUE;
       {other text}
       bDiscard : STRING[7]  = 'Discard';
       bPlay    : STRING[4]  = 'Play';
       bKeep    : STRING[4]  = 'Keep';
       bPlayGm  : STRING[10] = '~Play Game';
       bNewGm   : STRING[9]  = '~New Game';
       {prompt and message strings}
       mVersion    : STRING[28] = 'Version 1.08 - January 2003';
       mQuitM      : STRING[12] = 'Really quit?';
       mYourCrib   : STRING[19] = 'Your crib to start.';
       mMyCrib     : STRING[17] = 'My crib to start.';
       mAreSure    : STRING[39] = 'Are you sure you wish start a new game?';
       mYouWon     : STRING[28] = 'You''ve won, congratulations.';
       mCompWon    : STRING[9]  = 'I''ve won.';
       mUpMsg      : STRING[45] = 'Up card is a Jack, two points to crib holder.';
       mPointErr   : STRING[30] = 'Too many points for that hand!';
       mMuggins    : STRING[44] = 'MUGGINS! You missed %d points (which I get).';
       mPDlgTit1   : STRING[11] = 'Crib Points';
       mCompPlay   : STRING[10] = 'My play...';
       mPlayPlay   : STRING[12] = 'Your play...';
       mCompFirst  : STRING[16] = 'I score first...';
       mPlayFirst  : STRING[18] = 'You score first...';
       mCompPts    : STRING[17] = 'I have %d points.';
       mCompCPts   : STRING[32] = 'There are %d points in the crib.';
       mShame      : STRING[30] = 'Shame on you, you have a play!';
       mYou        : STRING[3]  = 'You';
       mMe         : STRING[1]  = 'I';
       mTwo15      : STRING[28] = ' get 2 points for making 15.';
       mPair       : STRING[25] = ' get 2 points for a pair.';
       mThreeKind  : STRING[34] = ' get 6 points for three of a kind.';
       mFourKind   : STRING[34] = ' get 12 points for four of a kind.';
       mRun        : STRING[35] = '%s get %d points for a %d card run.';
       mTwo31      : STRING[28] = ' get 2 points for making 31.';
       mMoreThan31 : STRING[36] = 'That totals more than 31, try again.';
       mLastCard   : STRING[27] = ' get 1 point for last card.';
       mGo         : STRING[24] = 'I don''t have a play, Go.';
       mGivePt     : STRING[35] = 'I have no play, you get last point.';
       {cribbage board constants}
       cb_Blank = 0;
       cb_Empty = 1;
       cb_Blue  = 2;
       cb_Red   = 3;
       cb_Arrow = 4;
       DefBoard : TCRIBBOARD = ((0,2,0,4,1,1,1,1,1,0,1,1,1,1,1,0,1,1,1,1,1,0,1,1,1,1,1,0,1,1,1,1,1,0,1,1,1,1,1,0),
                                (0,2,0,0,1,1,1,1,1,0,1,1,1,1,1,0,1,1,1,1,1,0,1,1,1,1,1,0,1,1,1,1,1,0,1,1,1,1,1,0),
                                (0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0),
                                (0,3,0,0,1,1,1,1,1,0,1,1,1,1,1,0,1,1,1,1,1,0,1,1,1,1,1,0,1,1,1,1,1,0,1,1,1,1,1,0),
                                (0,3,0,4,1,1,1,1,1,0,1,1,1,1,1,0,1,1,1,1,1,0,1,1,1,1,1,0,1,1,1,1,1,0,1,1,1,1,1,0));

 VAR CardDeck  : Q_CARDDECK;
     CribBoard : TCRIBBOARD;

(************************************************************************)

 PROCEDURE PutCardInImage(VAR Img : TIMAGE; Card : LONGWORD);
     (* procedure used to put a specific card into a timage instance *)

 PROCEDURE SortHand(VAR Hnd : TCARDHAND; NC : INTEGER);
     (* procedure to sort the dealt hand (6 cards) *)

(************************************************************************)

IMPLEMENTATION

(************************************************************************)

 PROCEDURE PutCardInImage(VAR Img : TIMAGE; Card : LONGWORD);
     (* procedure used to put a specific card into a timage object *)

    VAR T : TBITMAP;

  BEGIN (*putcardinimage*)
   // get card image from DLL (based on what kind it is)
   IF (Card = Red_Joker) OR (Card = Black_Joker)
    THEN T := QC_GetJoker(Card)
   ELSE IF (Card >= Min_SymbolNum) AND (Card <= Max_SymbolNum)
         THEN T := QC_GetCardSymbol(Card)
        ELSE IF (Card >= Min_CardBackNum) AND (Card <= Max_CardBackNum)
              THEN T := QC_GetCardBack(Card)
             ELSE T := QC_GetCard(Card);
   Img.Bitmap := T; // assign to image
   T.Free; // free reference returned from DLL
  END; (*putcardinimage*)

(************************************************************************)

 PROCEDURE SortHand(VAR Hnd : TCARDHAND; NC : INTEGER);
     (* procedure to sort the dealt hand (6 cards) *)

    VAR I, J, V1, V2 : INTEGER;
        T            : LONGWORD;

  BEGIN (*sorthand*)
   // bubble sort it
   FOR I := 1 TO (NC-1) DO
    FOR J := (I+1) TO NC DO
     BEGIN {inner loop}
      V1 := QC_GetCardValue(Hnd[I]);
      V2 := QC_GetCardValue(Hnd[J]);
      IF V1 < V2
       THEN BEGIN {swap}
             T := Hnd[I]; Hnd[I] := Hnd[J]; Hnd[J] := T;
            END; {then}
     END; {j for}
  END; (*sorthand*)

(************************************************************************)

INITIALIZATION

END. (*of unit*)
