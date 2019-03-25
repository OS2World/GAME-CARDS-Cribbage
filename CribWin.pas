UNIT CribWin;

(*                                                                      *)
(* AUTHOR: Michael G. Slack                    DATE WRITTEN: 2001/03/05 *)
(* ENVIRONMENT: Sibyl                                                   *)
(*                                                                      *)
(* Unit defines the main window used by the cribbage program.           *)
(*                                                                      *)
(* -------------------------------------------------------------------- *)
(*                                                                      *)
(* REVISED: 2001/06/17 - Added in code to fix 'last card' problem.      *)
(*          2001/06/24 - Fixed where comp was not getting 'last card'   *)
(*                       after comp played out hand.                    *)
(*          2001/07/08 - Fixed a bug in the start-hand routine in which *)
(*                       not all 'discard' buttons were enabled.        *)
(*                                                                      *)

INTERFACE

 USES Classes, Forms, Graphics, Grids, ExtCtrls, Buttons, StdCtrls, Messages,
      Menus, CribSup;

 TYPE TCRIBBAGEWIN = CLASS(TForm)
                      DG_CribbageBoard : TDrawGrid;
                      Bevel1           : TBevel;
                      ImgCard1         : TImage;
                      ImgCard2         : TImage;
                      ImgCard3         : TImage;
                      ImgCard4         : TImage;
                      ImgCard5         : TImage;
                      ImgCard6         : TImage;
                      BtnCard1         : TButton; // discard/play
                      BtnCard2         : TButton;
                      BtnCard3         : TButton;
                      BtnCard4         : TButton;
                      BtnCard5         : TButton;
                      BtnCard6         : TButton;
                      PB_Play          : TPaintBox; // card play area
                      BtnPlay          : TBitBtn;
                      BtnExit          : TBitBtn;
                      BtnSettings      : TBitBtn;
                      BtnGo            : TBitBtn;
                      BtnContinue      : TBitBtn;
                      Label1           : TLabel; // comp cards left title
                      LblCLeft         : TLabel;
                      LblCrib          : TLabel;
                      Label2           : TLabel; // current tally (round score)
                      LblTally         : TLabel;
                      Label3           : TLabel; // current game score title
                      LblPlayer        : TLabel;
                      LblComp          : TLabel;
                      Label4           : TLabel; // games won title
                      LblGPlayer       : TLabel;
                      LblGComp         : TLabel;
                      PopupAbout       : TPopupMenu;
                      MI_About         : TMenuItem;
                      DelayTimer       : TTimer;
                      ImgCardDeck      : TImage; // card back/up card
                      Procedure CribbageWinOnCreate(Sender : TObject);
                      Procedure CribbageWinOnShow(Sender : TObject);
                      Procedure CribbageWinOnDestroy(Sender : TObject);
                      Procedure CribbageWinOnCloseQuery(Sender : TObject;
                                                        Var CanClose : Boolean);
                      Procedure BtnPlayOnClick(Sender : TObject);
                      Procedure BtnExitOnClick(Sender : TObject);
                      Procedure BtnSettingsOnClick(Sender : TObject);
                      Procedure BtnGoOnClick(Sender : TObject);
                      Procedure BtnContinueOnClick(Sender : TObject);
                      Procedure BtnCardOnClick(Sender : TObject);
                      Procedure DG_CribbageBoardOnDrawCell(Sender : TObject;
                                                           ACol : LongInt;
                                                           ARow : LongInt;
                                                           rc : TRect;
                                                           State : TGridDrawState);
                      Procedure PB_PlayOnPaint(Sender : TObject; Const rec : TRect);
                      Procedure MI_AboutOnClick(Sender : TObject);
                      Procedure DelayTimerOnTimer(Sender : TObject);
                     PRIVATE
                      OldTop, OldLeft, // saved window position
                      P2PegP, C2PegP,  // back peg position (index value)
                      TempTally,  // used to count number of player discards
                      CardsLeft,  // number of cards left in comps hand
                      PCardsLeft, // number of cards left in players hand
                      PlayerGamesWon,
                      CompGamesWon,
                      PlayersScore,
                      CompScore  : INTEGER;
                      BrdImgs    : TBITMAPLIST; // board and peg images
                      Ini_Fn     : STRING[160];
                      GameOver,    // game is over (someone reached 121)
                      DelayDone,   // used by the delay proc and timer
                      PPlayedLast, // who played last card
                      GoFlagP,     // player 'go' flag
                      GoFlagC,     // comp 'go' flag
                      ScoreLastPt, // do we score a last point?
                      X31,         // 31 points reached
                      CribFlag   : BOOLEAN; // true = players crib, false = comps
                      PPly, CPly : ARRAY[1..4] OF LONGWORD; // played cards
                      CribHand,
                      PHnd, CHnd,
                      CHnd2      : TCARDHAND;
                      BtnCards   : ARRAY[1..6] OF TBUTTON; // BtnCardx array
                      ImgCards   : ARRAY[1..6] OF TIMAGE;  // ImgCardx array
                      Discards   : ARRAY[1..6] OF BOOLEAN;
                      PROCEDURE ProcessDiscarding(Sender : TObject; S : STRING);
                      PROCEDURE ProcessPlaying(Sender : TOBJECT);
                     PROTECTED
                      PROCEDURE WMStartUp(VAR Msg : TMESSAGE);
                       MESSAGE wm_StartUp;
                      PROCEDURE WMCompsPlay(VAR Msg : TMESSAGE);
                       MESSAGE wm_CompsPlay;
                      PROCEDURE WMPlayersPlay(VAR Msg : TMESSAGE);
                       MESSAGE wm_PlayersPlay;
                      PROCEDURE WMScoreHand(VAR Msg : TMESSAGE);
                       MESSAGE wm_ScoreHand;
                     PUBLIC
                      PROCEDURE Init_Game;
                      PROCEDURE FinishGame(Player : BOOLEAN);
                      PROCEDURE Read_INI;
                      PROCEDURE Write_INI;
                      PROCEDURE PaintBoard;
                      PROCEDURE DisplayCell(C, R : INTEGER);
                      PROCEDURE StartHand;
                      PROCEDURE GetUpCard;
                      PROCEDURE Delay100;
                      PROCEDURE PegMoveSound;
                      PROCEDURE MovePeg(Amount : INTEGER; Player : BOOLEAN);
                      PROCEDURE ClearPlayedCards;
                      PROCEDURE Play_Card(Player : BOOLEAN; Hnd : TCARDHAND;
                                          CardIdx : INTEGER);
                      PROCEDURE Get_Points(Crib : BOOLEAN);
                      PROCEDURE Show_Cards(Hnd : TCARDHAND);
                      PROCEDURE Clear31(Player : BOOLEAN);
                     END;

 VAR CribbageWin : TCRIBBAGEWIN;

(************************************************************************)

IMPLEMENTATION

 USES SysUtils, IniFiles, Dialogs, QCardU,
      CribMsg, CribPDlg, CribConf, CribAbout, CribEngine;

 TYPE TPEGPOS = ARRAY[1..121,1..2] OF INTEGER;

 CONST CPegPos : TPEGPOS = ((4,0),(5,0),(6,0),(7,0),(8,0),
       {comp peg pos}       (10,0),(11,0),(12,0),(13,0),(14,0),
                            (16,0),(17,0),(18,0),(19,0),(20,0),
                            (22,0),(23,0),(24,0),(25,0),(26,0),
                            (28,0),(29,0),(30,0),(31,0),(32,0),
                            (34,0),(35,0),(36,0),(37,0),(38,0),
                            (38,1),(37,1),(36,1),(35,1),(34,1),
                            (32,1),(31,1),(30,1),(29,1),(28,1),
                            (26,1),(25,1),(24,1),(23,1),(22,1),
                            (20,1),(19,1),(18,1),(17,1),(16,1),
                            (14,1),(13,1),(12,1),(11,1),(10,1),
                            (8,1),(7,1),(6,1),(5,1),(4,1),
                            (4,0),(5,0),(6,0),(7,0),(8,0),
                            (10,0),(11,0),(12,0),(13,0),(14,0),
                            (16,0),(17,0),(18,0),(19,0),(20,0),
                            (22,0),(23,0),(24,0),(25,0),(26,0),
                            (28,0),(29,0),(30,0),(31,0),(32,0),
                            (34,0),(35,0),(36,0),(37,0),(38,0),
                            (38,1),(37,1),(36,1),(35,1),(34,1),
                            (32,1),(31,1),(30,1),(29,1),(28,1),
                            (26,1),(25,1),(24,1),(23,1),(22,1),
                            (20,1),(19,1),(18,1),(17,1),(16,1),
                            (14,1),(13,1),(12,1),(11,1),(10,1),
                            (8,1),(7,1),(6,1),(5,1),(4,1),(1,1));
       PPegPos : TPEGPOS = ((4,4),(5,4),(6,4),(7,4),(8,4),
       {players peg pos}    (10,4),(11,4),(12,4),(13,4),(14,4),
                            (16,4),(17,4),(18,4),(19,4),(20,4),
                            (22,4),(23,4),(24,4),(25,4),(26,4),
                            (28,4),(29,4),(30,4),(31,4),(32,4),
                            (34,4),(35,4),(36,4),(37,4),(38,4),
                            (38,3),(37,3),(36,3),(35,3),(34,3),
                            (32,3),(31,3),(30,3),(29,3),(28,3),
                            (26,3),(25,3),(24,3),(23,3),(22,3),
                            (20,3),(19,3),(18,3),(17,3),(16,3),
                            (14,3),(13,3),(12,3),(11,3),(10,3),
                            (8,3),(7,3),(6,3),(5,3),(4,3),
                            (4,4),(5,4),(6,4),(7,4),(8,4),
                            (10,4),(11,4),(12,4),(13,4),(14,4),
                            (16,4),(17,4),(18,4),(19,4),(20,4),
                            (22,4),(23,4),(24,4),(25,4),(26,4),
                            (28,4),(29,4),(30,4),(31,4),(32,4),
                            (34,4),(35,4),(36,4),(37,4),(38,4),
                            (38,3),(37,3),(36,3),(35,3),(34,3),
                            (32,3),(31,3),(30,3),(29,3),(28,3),
                            (26,3),(25,3),(24,3),(23,3),(22,3),
                            (20,3),(19,3),(18,3),(17,3),(16,3),
                            (14,3),(13,3),(12,3),(11,3),(10,3),
                            (8,3),(7,3),(6,3),(5,3),(4,3),(1,3));

(************************************************************************)

 Procedure TCribbageWin.CribbageWinOnCreate(Sender : TObject);

    VAR I : INTEGER;

  Begin
   PlayerGamesWon := 0; CompGamesWon := 0;
   Init_Game;
   {load cribbage board images (by id from resource)}
   BrdImgs := TBITMAPLIST.Create;
   FOR I := 1000 TO 1004 DO
    BrdImgs.AddResourceID(I); // 0 = blank, 1 = empty, 2 = blue, 3 = red, 4 = arw
  End;

(************************************************************************)

 Procedure TCribbageWin.CribbageWinOnShow(Sender : TObject);
  Begin
   Top := (Screen.Height DIV 2) - (Height DIV 2); {default position}
   Left := (Screen.Width DIV 2) - (Width DIV 2);
   {read and setup options}
   Ini_Fn := GetDefaultINI;
   Read_INI;
   OldTop := Top; OldLeft := Left; {save old position}
   {set up button and image arrays - easier to handle like this}
   BtnCards[1] := BtnCard1;
   BtnCards[2] := BtnCard2;
   BtnCards[3] := BtnCard3;
   BtnCards[4] := BtnCard4;
   BtnCards[5] := BtnCard5;
   BtnCards[6] := BtnCard6;
   ImgCards[1] := ImgCard1;
   ImgCards[2] := ImgCard2;
   ImgCards[3] := ImgCard3;
   ImgCards[4] := ImgCard4;
   ImgCards[5] := ImgCard5;
   ImgCards[6] := ImgCard6;
   {do the final setup now}
   PostMsg(Handle,wm_StartUp,0,0);
  End;

(************************************************************************)

 Procedure TCribbageWin.CribbageWinOnDestroy(Sender : TObject);

    VAR Ini : TINIFILE;

  Begin
   BrdImgs.Free;
   {check if position needs to be saved or not}
   IF (Top <> OldTop) OR (Left <> OldLeft)
    THEN BEGIN {only save if changed}
          Ini := TINIFILE.Create(Ini_Fn);
          Ini.WriteInteger(Ini_Pos,Ini_PTop,Top);
          Ini.WriteInteger(Ini_Pos,Ini_PLeft,Left);
          Ini.Free;
         END; {then}
  End;

(************************************************************************)

 Procedure TCribbageWin.CribbageWinOnCloseQuery(Sender : TObject;
                                                Var CanClose : Boolean);

    VAR Ret : TMSGDLGRETURN;

  Begin
   Ret := mrYes; {close if prompting is not on}
   IF PromptMe
    THEN BEGIN {prompt user}
          Ret := MessageBox(mQuitM, mtConfirmation, [mbYes,mbNo]);
         END; {then}
   CanClose := Ret = mrYes;
  End;

(************************************************************************)

 Procedure TCribbageWin.BtnPlayOnClick(Sender : TObject);

    VAR S   : STRING[10];
        Ret : TMSGDLGRETURN;

  Begin
   S := BtnPlay.Caption;
   IF (S = bPlayGm)
    THEN BtnPlay.Caption := bNewGm
   ELSE BEGIN {prompt them if on - maybe start new game...}
         Ret := mrYes;
         IF PromptMe
          THEN Ret := MessageBox(mAreSure, mtConfirmation, [mbYes,mbNo]);
         IF Ret <> mrYes THEN Exit;
         Init_Game;
         PaintBoard;
        END; {else}
   // starting crib
   IF AlwaysStarts
    THEN CribFlag := TRUE
   ELSE CribFlag := (Random(2) = 0);
   IF CribFlag
    THEN BEGIN {players crib first}
          Show_Message(Self, mYourCrib);
         END {then}
   ELSE BEGIN {comps crib first}
         Show_Message(Self, mMyCrib);
        END; {else}
   StartHand;
  End;

(************************************************************************)

 Procedure TCribbageWin.BtnExitOnClick(Sender : TObject);
  Begin
   Close;
  End;

(************************************************************************)

 Procedure TCribbageWin.BtnSettingsOnClick(Sender : TObject);

    VAR Ret : TMSGDLGRETURN;

  Begin
   ConfDlg := TCONFDLG.Create(Self);
   Ret := ConfDlg.ShowModal;
   ConfDlg.Free;
   IF (Ret = mrOK)
    THEN BEGIN {save settings}
          Write_INI;
         END; {then}
  End;

(************************************************************************)

 Procedure TCribbageWin.BtnGoOnClick(Sender : TObject);

    VAR I : INTEGER;

  Begin
   // player has no play?
   FOR I := 1 TO 4 DO
    IF (BtnCards[I].Visible) AND ((PHnd[I] + HandTally) <= 31)
     THEN BEGIN {player has a play - what gives}
           Show_Message(Self, mShame); Exit;
          END; {then}
   // process 'go'
   IF GoFlagC
    THEN BEGIN {comp has already gone - clear board give player a point}
          Show_Message(Self, mYou + mLastCard);
          MovePeg(1, TRUE);
          ClearPlayedCards;
         END {then}
   ELSE BEGIN {maybe move on}
         IF CardsLeft <> 0
          THEN GoFlagP := TRUE
         ELSE BEGIN {need to give last point to comp}
               ScoreLastPt := FALSE;
               Show_Message(Self, mMe + mLastCard);
               MovePeg(1, FALSE);
               ClearPlayedCards;
              END; {else}
        END; {else}
   PostMsg(Handle, wm_CompsPlay, 0, 0);
  End;

(************************************************************************)

 Procedure TCribbageWin.BtnContinueOnClick(Sender : TObject);

    VAR I, J, C1, C2 : INTEGER;

  Begin
   AnalyzeHandForDiscards(CHnd); // get computers discards for crib
   CribHand[1] := CHnd[RecDiscardIdx1]; CribHand[2] := CHnd[RecDiscardIdx2];
   CardsLeft := 4; LblCLeft.Caption := IntToStr(CardsLeft); PCardsLeft := 4;
   // add players discards to crib
   J := 3;
   FOR I := 1 TO 6 DO
    IF Discards[I]
     THEN BEGIN {add to crib}
           IF J = 3 THEN C1 := I ELSE C2 := I;
           CribHand[J] := PHnd[I]; Inc(J);
           IF J = 5 THEN Break;
          END; {then}
   // move cards down to first 4 (player and comp)
   FOR I := C2 TO 5 DO
    PHnd[I] := PHnd[I+1];
   FOR I := C1 TO 5 DO
    PHnd[I] := PHnd[I+1];
   FOR I := RecDiscardIdx2 TO 5 DO
    CHnd[I] := CHnd[I+1];
   FOR I := RecDiscardIdx1 TO 5 DO
    CHnd[I] := CHnd[I+1];
   CHnd2 := CHnd; // get hand we can mess around with
   // sort crib hand
   SortHand(CribHand, 4);
   // reset display
   FOR I := 1 TO 6 DO
    BEGIN {show players cards - enable play buttons}
     BtnCards[I].Caption := bPlay;
     IF I > 4
      THEN BEGIN {only need first 4}
            BtnCards[I].Visible := FALSE;
            PutCardInImage(ImgCards[I], 0); // clear card image
           END {then}
     ELSE PutCardInImage(ImgCards[I], PHnd[I]);
    END; {i for}
   // turn up card now
   GetUpCard;
   BtnContinue.Enabled := FALSE; // disable the button again
   IF CribFlag
    THEN PostMsg(Handle, wm_PlayersPlay, 0, 0)
   ELSE PostMsg(Handle, wm_CompsPlay, 0, 0);
  End;

(************************************************************************)

 Procedure TCribbageWin.BtnCardOnClick(Sender : TObject);

    VAR S : STRING[7];

  Begin
   S := TBUTTON(Sender).Caption;
   IF (S = bDiscard) OR (S = bKeep)
    THEN BEGIN {discard/keep processing}
          ProcessDiscarding(Sender, S);
         END {discard/keep}
   ELSE BEGIN {play processing}
         ProcessPlaying(Sender);
        END; {play}
  End;

(************************************************************************)

 Procedure TCribbageWin.DG_CribbageBoardOnDrawCell(Sender : TObject;
                                                   ACol : LongInt; ARow : LongInt;
                                                   rc : TRect;
                                                   State : TGridDrawState);

    VAR ImgNum : INTEGER;

  Begin
   DG_CribbageBoard.Canvas.FillRect(rc, clBlack);
   ImgNum := CribBoard[ARow, ACol];
   BrdImgs.Bitmaps[ImgNum].Draw(DG_CribbageBoard.Canvas, rc);
  End;

(************************************************************************)

 Procedure TCribbageWin.PB_PlayOnPaint(Sender: TObject; Const rec : TRect);

    VAR I, CX, PX : INTEGER;
        Tmp       : TBITMAP;
        rc        : TRECT;

  Begin
   // erase paintbox
   PB_Play.Canvas.FillRect(rec, PB_Play.Color);
   // draw cards (if any played)
   CX := 5; PX := 5;
   FOR I := 1 TO 4 DO
    BEGIN {draw cards in paint box}
      // draw computers played card
      Tmp := QC_GetCard(CPly[I]);
      IF Tmp <> NIL
       THEN BEGIN {draw it}
             rc.Left := PX; rc.Bottom := 107;
             rc.Top := rc.Bottom + qsz_CardHeight;
             rc.Right := rc.Left + qsz_CardWidth;
             Tmp.Draw(PB_Play.Canvas, rc);
             Tmp.Free;
             PX := PX + qsz_ImageOffset;
            END; {then}
      // draw players played card
      Tmp := QC_GetCard(PPly[I]);
      IF Tmp <> NIL
       THEN BEGIN {draw it}
             rc.Left := CX; rc.Bottom := 6;
             rc.Top := rc.Bottom + qsz_CardHeight;
             rc.Right := rc.Left + qsz_CardWidth;
             Tmp.Draw(PB_Play.Canvas, rc);
             Tmp.Free;
             CX := CX + qsz_ImageOffset;
            END; {then}
    END; {i for}
  End;

(************************************************************************)

 Procedure TCribbageWin.MI_AboutOnClick(Sender : TObject);
  Begin
   AboutBox := TABOUTBOX.Create(Self);
   AboutBox.ShowModal;
   AboutBox.Free;
  End;

(************************************************************************)

 Procedure TCribbageWin.DelayTimerOnTimer(Sender : TObject);
  Begin
   DelayTimer.Stop;
   DelayDone := TRUE;
  End;

(************************************************************************)

 PROCEDURE TCribbageWin.ProcessDiscarding(Sender : TObject; S : STRING);
     (* procedure to process discard/keep button presses *)

    VAR I : INTEGER;

  BEGIN (*tcribbagewin.processdiscarding*)
   IF S = bDiscard
    THEN BEGIN {discard}
          Inc(TempTally);
          TBUTTON(Sender).Caption := bKeep;
         END {then}
   ELSE BEGIN {keep}
         Dec(TempTally);
         TBUTTON(Sender).Caption := bDiscard;
        END; {else}
   // discarded 2, we can continue
   IF TempTally = 2
    THEN BtnContinue.Enabled := TRUE
   ELSE BtnContinue.Enabled := FALSE;
   // which button (card) to discard/keep
   FOR I := 1 TO 6 DO
    IF Sender = BtnCards[I]
     THEN Discards[I] := (S = bDiscard);
  END; (*tcribbagewin.processdiscarding*)

(************************************************************************)

 PROCEDURE TCribbageWin.ProcessPlaying(Sender : TOBJECT);
     (* procedure used to process play button presses *)

    VAR I, J, P : INTEGER;

  BEGIN (*tcribbagewin.processplaying*)
   FOR I := 1 TO 4 DO
    IF BtnCards[I] = Sender THEN J := I;
   IF (HandTally + QC_GetCV_Face10(PHnd[J])) > 31
    THEN BEGIN {nope!}
          Show_Message(Self, mMoreThan31); Exit;
         END; {then}

   PutCardInImage(ImgCards[J], 0);   // erase card from hand
   TBUTTON(Sender).Visible := FALSE; // turn off button now that card is played
   Application.ProcessMessages;

   // do the play now
   Dec(PCardsLeft);
   Play_Card(TRUE, PHnd, J);
   PointsInGame(1, X31, P);
   IF X31 THEN Clear31(TRUE);
   MovePeg(P, TRUE);

   IF NOT(GameOver) AND NOT(GoFlagC) THEN PostMsg(Handle, wm_CompsPlay, 0, 0);
  END; (*tcribbagewin.processplaying*)

(************************************************************************)

 PROCEDURE TCribbageWin.WMStartUp(VAR Msg : TMESSAGE);
     (* procedure used to do final setup during program startup *)

  BEGIN (*tcribbagewin.wmstartup*)
   // this is needed when using the DLL to store cards (timing issue I think)
   PutCardInImage(ImgCardDeck, CardBackPic);
  END; (*tcribbagewin.wmstartup*)

(************************************************************************)

 PROCEDURE TCribbageWin.WMCompsPlay(VAR Msg : TMESSAGE);
     (* procedure used to handle computers turn (when it needs to) *)

    VAR C, P   : INTEGER;
        ImDone : BOOLEAN;

  BEGIN (*tcribbagewin.wmcompsplay*)
   IF GameOver THEN Exit;
   BtnGo.Enabled := FALSE; // disable 'go' button
   FOR C := 1 TO 4 DO // disable 'play' buttons
    IF BtnCards[C].Visible THEN BtnCards[C].Enabled := FALSE;
   IF (CardsLeft = 0) AND (PCardsLeft = 0)
    THEN BEGIN {finish off hand}
          PostMsg(Handle, wm_ScoreHand, 0, 0); Exit;
         END; {then}
   IF (CardsLeft > 0)
    THEN BEGIN // my turn
          IF VerboseMode THEN Show_Message(Self, mCompPlay);
          REPEAT // may have multiple moves
           Application.ProcessMessages;
           // get and play card
           C := GetCompCardToPlay(CHnd2);
           IF C <> -1
            THEN BEGIN {play the card and take the points}
                  Dec(CardsLeft); LblCLeft.Caption := IntToStr(CardsLeft);
                  Play_Card(FALSE, CHnd, C);
                  PointsInGame(2, X31, P);
                  IF X31 THEN Clear31(FALSE);
                  MovePeg(P, FALSE);
                 END {then}
           ELSE BEGIN {process 'go'}
                 IF GoFlagP
                  THEN BEGIN {player has already gone - clear board give me a point}
                        Show_Message(Self, mMe + mLastCard);
                        MovePeg(1, FALSE);
                        ClearPlayedCards;
                       END {then}
                 ELSE BEGIN {computer pushes go}
                       IF PCardsLeft <> 0
                        THEN BEGIN {show message and move on}
                              Show_Message(Self, mGo);
                              GoFlagC := TRUE;
                             END {then}
                       ELSE BEGIN {need to give last point to player}
                             ScoreLastPt := FALSE;
                             Show_Message(Self, mGivePt);
                             MovePeg(1, TRUE);
                             ClearPlayedCards;
                            END; {else}
                      END; {else}
                END; {else}
           ImDone := NOT(GoFlagP) OR (CardsLeft = 0) OR GameOver;
          UNTIL ImDone;
          // fix so that 'last card' point is correct (I think)
          IF (CardsLeft = 0) AND (PCardsLeft <> 0) AND (GoFlagP)
           THEN BEGIN {player has gone, comp played out cards}
                 Show_Message(Self, mMe + mLastCard);
                 MovePeg(1, FALSE);
                 ClearPlayedCards; {clear cards and reset so player can play out}
                 GoFlagP := TRUE; {reset this just in case}
                END; {then}
         END; {then}
   // if the game ain't over, give turn to the player
   IF NOT(GameOver) THEN PostMsg(Handle, wm_PlayersPlay, 0, 0);
  END; (*tcribbagewin.wmcompsplay*)

(************************************************************************)

  PROCEDURE TCribbageWin.WMPlayersPlay(VAR Msg : TMESSAGE);
      (* procedure used to handle giving player the turn *)

     VAR I : INTEGER;

   BEGIN (*tcribbagewin.wmplayersplay*)
    IF NOT(GameOver)
     THEN BEGIN {set up for player to play}
           IF (PCardsLeft = 0) AND (CardsLeft = 0)
            THEN BEGIN {score the hand/crib then}
                  PostMsg(Handle, wm_ScoreHand, 0, 0); Exit;
                 END; {then}
           IF (PCardsLeft > 0)
            THEN BEGIN // is it really our turn?
                  // enable buttons for the user to play with
                  BtnGo.Enabled := TRUE; // give the player a 'go' button
                  FOR I := 1 TO 4 DO // enable the 'play' buttons
                   IF BtnCards[I].Visible THEN BtnCards[I].Enabled := TRUE;
                  // show whos turn it is
                  IF VerboseMode THEN Show_Message(Self, mPlayPlay);
                 END {then}
           ELSE PostMsg(Handle, wm_CompsPlay, 0, 0);
          END; {then}
   END; (*tcribbagewin.wmplayersplay*)

(************************************************************************)

  PROCEDURE TCribbageWin.WMScoreHand(VAR Msg : TMESSAGE);
      (* procedure used to handle the hand scoring event *)

     VAR Pts : INTEGER;

   BEGIN (*tcribbagewin.wmscorehand*)
    IF GameOver THEN Exit;
    // give out point for last card
    IF NOT(X31) AND (ScoreLastPt)
     THEN BEGIN {don't get last card if score 31 with last card or go}
           IF PPlayedLast
            THEN BEGIN {player}
                  Show_Message(Self, mYou + mLastCard);
                  MovePeg(1, TRUE);
                 END {then}
           ELSE BEGIN {comp}
                 Show_Message(Self, mMe + mLastCard);
                 MovePeg(1, FALSE);
                END; {else}
          END; {then}
    IF GameOver THEN Exit;
    ClearPlayedCards;
    IF VerboseMode
     THEN BEGIN {display who scores first}
           IF CribFlag
            THEN Show_Message(Self, mCompFirst)
           ELSE Show_Message(Self, mPlayFirst);
          END; {then}
    // determine points and total scores to this point
    Figure_Points(CHnd, FALSE, Pts);
    IF CribFlag
     THEN BEGIN {comp scoring first}
           Show_Cards(CHnd);
           Show_Message(Self, Format(mCompPts, [Pts]));
           MovePeg(Pts, FALSE);
           IF NOT(GameOver)
            THEN BEGIN {get points from user now}
                  Show_Cards(PHnd);
                  Get_Points(FALSE);
                  IF NOT(GameOver)
                   THEN BEGIN {get crib points}
                         Show_Cards(CribHand);
                         Get_Points(TRUE);
                        END; {then}
                 END; {then}
          END {then}
    ELSE BEGIN {player scores first}
          Show_Cards(PHnd);
          Get_Points(FALSE);
          IF NOT(GameOver)
           THEN BEGIN {get computers score now}
                 Show_Cards(CHnd);
                 Show_Message(Self, Format(mCompPts, [Pts]));
                 MovePeg(Pts, FALSE);
                 IF NOT(GameOver)
                  THEN BEGIN {show computers crib score}
                        Show_Cards(CribHand);
                        Figure_Points(CribHand, TRUE, Pts);
                        Show_Message(Self, Format(mCompCPts, [Pts]));
                        MovePeg(Pts, FALSE);
                       END; {then}
                END; {then}
         END; {else}
    IF NOT(GameOver)
     THEN BEGIN {setup for next hand}
           CribFlag := NOT(CribFlag);
           StartHand;
          END; {then}
   END; (*tcribbagewin.wmscorehand*)

(************************************************************************)

 PROCEDURE TCribbageWin.Init_Game;
     (* procedure used to initialize the game (start new) *)

  BEGIN (*tcribbagewin.init_game*)
   PlayersScore := 0; CompScore := 0; // set scores to zero
   P2PegP := 121; C2PegP := 121; GameOver := FALSE; UpCard := 0;
   LblPlayer.Caption := IntToStr(PlayersScore);
   LblComp.Caption := IntToStr(CompScore);
   CribBoard := DefBoard;
   LblCrib.Color := clSilver;
  END; (*tcribbagewin.init_game*)

(************************************************************************)

 PROCEDURE TCribbageWin.FinishGame(Player : BOOLEAN);
     (* procedure to run the game finish code *)

    VAR SSS : STRING[80];
        I   : INTEGER;

  BEGIN (*tcribbagewin.finishgame*)
   GameOver := TRUE;
   // display won message
   IF Player THEN SSS := mYouWon ELSE SSS := mCompWon;
   Show_Message(Self, SSS);
   // increment total games won for winner
   IF Player
    THEN BEGIN {player won}
          Inc(PlayerGamesWon);
          LblGPlayer.Caption := IntToStr(PlayerGamesWon);
         END {then}
   ELSE BEGIN {comp won}
         Inc(CompGamesWon);
         LblGComp.Caption := IntToStr(CompGamesWon);
        END; {else}
   // reset some of the game now
   BtnContinue.Enabled := FALSE;
   BtnGo.Enabled := FALSE;
   FOR I := 1 TO 6 DO
    BtnCards[I].Visible := FALSE;
   FOR I := 1 TO 4 DO
    PutCardInImage(ImgCards[I], 0); // clear card images
  END; (*tcribbagewin.finishgame*)

(************************************************************************)

 PROCEDURE TCribbageWin.Read_INI;
     (* procedure used to read the ini file and pull program options out *)

    VAR Ini : TINIFILE;

  BEGIN (*tcribbagewin.read_ini*)
   Ini := TINIFILE.Create(Ini_Fn);
   {read in window position (just top and left needed)}
   Top := Ini.ReadInteger(Ini_Pos,Ini_PTop,Top);
   Left := Ini.ReadInteger(Ini_Pos,Ini_PLeft,Left);
   {read in options now}
   CardBackPic := Ini.ReadInteger(Ini_Opt,Ini_OCPic,CardBackPic);
   PromptMe := Ini.ReadBool(Ini_Opt,Ini_OPrmt,PromptMe);
   AlwaysStarts := Ini.ReadBool(Ini_Opt,Ini_OAlways,AlwaysStarts);
   UseSounds := Ini.ReadBool(Ini_Opt,Ini_OUseSnd,UseSounds);
   VerboseMode := Ini.ReadBool(Ini_Opt,Ini_OVerbose,VerboseMode);
   {close it}
   Ini.Free;
  END; (*tcribbagewin.read_ini*)

(************************************************************************)

 PROCEDURE TCribbageWin.Write_INI;
     (* procedure to write out to the ini file the options *)

    VAR Ini : TINIFILE;

  BEGIN (*tcribbagewin.writeini*)
   Ini := TINIFILE.Create(Ini_Fn);
   {write out options}
   Ini.WriteInteger(Ini_Opt,Ini_OCPic,CardBackPic);
   Ini.WriteBool(Ini_Opt,Ini_OPrmt,PromptMe);
   Ini.WriteBool(Ini_Opt,Ini_OAlways,AlwaysStarts);
   Ini.WriteBool(Ini_Opt,Ini_OUseSnd,UseSounds);
   Ini.WriteBool(Ini_Opt,Ini_OVerbose,VerboseMode);
   {close it down}
   Ini.Free;
  END; (*tcribbagewin.writeini*)

(************************************************************************)

 PROCEDURE TCribbageWin.PaintBoard;
     (* procedure to paint all cribbage board cells *)

  BEGIN (*tcribbagewin.paintboard*)
   DG_CribbageBoard.BeginUpdate;
   DG_CribbageBoard.Repaint;
   DG_CribbageBoard.EndUpdate;
  END; (*tcribbagewin.paintboard*)

(************************************************************************)

 PROCEDURE TCribbageWin.DisplayCell(C, R : INTEGER);
     (* procedure to redraw a particular cell *)

    VAR rc : TRECT;

  BEGIN (*tcribbagewin.displaycell*)
   rc := DG_CribbageBoard.GridRects[C,R];
   DG_CribbageBoard.Redraw(rc);
  END; (*tcribbagewin.displaycell*)

(************************************************************************)

 PROCEDURE TCribbageWin.StartHand;
     (* procedure to initialize the start of a hand *)

    VAR I : INTEGER;

  BEGIN (*tcribbagewin.starthand*)
   ClearPlayedCards; // clear out played cards/area
   UpCard := 0; // reset to card back image (clear upcard)
   PutCardInImage(ImgCardDeck, CardBackPic);
   QC_ShuffleDeck(CardDeck, qcn_1Deck); // shuffle card deck (1 deck)
   IF CribFlag
    THEN LblCrib.Color := clRed
   ELSE LblCrib.Color := clBlue;
   ScoreLastPt := TRUE;
   // deal cards and reset buttons
   TempTally := 0; {no discards from player yet}
   FOR I := 1 TO 6 DO
    BEGIN {do it}
     PHnd[I] := QC_GetNextCard(CardDeck);
     CHnd[I] := QC_GetNextCard(CardDeck);
     Discards[I] := FALSE;
     BtnCards[I].Caption := bDiscard;
     BtnCards[I].Visible := TRUE;
     BtnCards[I].Enabled := TRUE; // just in case
    END; {for i}
   SortHand(PHnd, 6);
   SortHand(CHnd, 6);
   // show players cards now
   FOR I := 1 TO 6 DO
    PutCardInImage(ImgCards[I], PHnd[I]);
  END; (*tcribbagewin.starthand*)

(************************************************************************)

 PROCEDURE TCribbageWin.GetUpCard;
     (* procedure to deal the 'up' card *)

  BEGIN (*tcribbagewin.getupcard*)
   UpCard := QC_GetNextCard(CardDeck);
   PutCardInImage(ImgCardDeck, UpCard);
   IF QC_GetCardValue(UpCard) = 11
    THEN BEGIN {score 2 points for crib holder}
          IF VerboseMode
           THEN Show_Message(Self, mUpMsg);
          MovePeg(2, CribFlag);
         END; {then}
  END; (*tcribbagewin.getupcard*)

(************************************************************************)

 PROCEDURE TCribbageWin.Delay100;
     (* procedure used to do a 100 ms delay *)

  BEGIN (*tcribbagewin.delay100*)
   DelayDone := FALSE;
   DelayTimer.Start;
   WHILE NOT(DelayDone) DO
    Application.ProcessMessages;
  END; (*tcribbagewin.delay100*)

(************************************************************************)

 PROCEDURE TCribbageWin.PegMoveSound;
     (* procedure used to make 'sound' when the pegs are moved on the bouad *)

  BEGIN (*tcribbagewin.pegmovesound*)
   IF UseSounds THEN Beep(50,50);
   Delay100; // pause between peg moves
  END; (*tcribbagewin.pegmovesound*)

(************************************************************************)

 PROCEDURE TCribbageWin.MovePeg(Amount : INTEGER; Player : BOOLEAN);
     (* procedure to move the peg (from backpeg) to amount ahead of frontpeg *)

    VAR FrontPegP, BackPegP, S : INTEGER;
        BC, BR, NC, NR, I, C   : INTEGER;
        TempPos                : TPEGPOS;

  BEGIN (*tcribbagewin.movepeg*)
   IF Amount = 0 THEN Exit;
   IF Player
    THEN BEGIN {moving players peg}
          FrontPegP := PlayersScore; BackPegP := P2PegP; C := cb_Red;
          TempPos := PPegPos;
         END {then}
   ELSE BEGIN {moving comps peg}
         FrontPegP := CompScore; BackPegP := C2PegP; C := cb_Blue;
         TempPos := CPegPos;
        END; {else}
   // get back peg position (to clear)
   IF FrontPegP <> 0
    THEN BEGIN {get position to clear}
          BC := TempPos[BackPegP,1];
          BR := TempPos[BackPegP,2];
          IF Player THEN P2PegP := FrontPegP ELSE C2PegP := FrontPegP;
         END {then}
   ELSE BEGIN {need to clear front peg pos}
         BC := 1;
         IF Player THEN BR := 4 ELSE BR := 0;
        END; {else}
   // now move the peg (one point at a time)
   FOR I := 1 TO Amount DO
    BEGIN {move the peg}
     IF Player
      THEN BEGIN
            Inc(PlayersScore); FrontPegP := PlayersScore; S := PlayersScore;
            LblPlayer.Caption := IntToStr(PlayersScore);
           END {then}
     ELSE BEGIN
           Inc(CompScore); FrontPegP := CompScore; S := CompScore;
           LblComp.Caption := IntToStr(CompScore);
          END; {else}
     NC := TempPos[FrontPegP,1];
     NR := TempPos[FrontPegP,2];
     // erase back peg now
     CribBoard[BR,BC] := cb_Empty;
     DisplayCell(BC, BR);
     // show front peg now
     CribBoard[NR,NC] := C;
     DisplayCell(NC, NR); BC := NC; BR := NR;
     PegMoveSound;
     IF S = 121
      THEN BEGIN {game done}
            FinishGame(Player);
            Break; // leave - game is over
           END; {then}
    END; {i for}
  END; (*tcribbagewin.movepeg*)

(************************************************************************)

 PROCEDURE TCribbageWin.ClearPlayedCards;
     (* procedure used to clear out played cards and clean up play area *)

    VAR I : INTEGER;

  BEGIN (*tcribbagewin.clearplayedcards*)
   // reset globals associated with played cards
   CardsPlayed := 0; HandTally := 0; GoFlagP := FALSE; GoFlagC := FALSE;
   LblTally.Caption := IntToStr(HandTally);
   FOR I := 1 TO 4 DO
    BEGIN {reset played cards}
     PPly[I] := 0; CPly[I] := 0;
    END; {for i}
   PB_Play.Repaint; // redraw play area
  END; (*tcribbagewin.clearplayedcards*)

(************************************************************************)

 PROCEDURE TCribbageWin.Play_Card(Player : BOOLEAN; Hnd : TCARDHAND;
                                  CardIdx : INTEGER);
     (* procedure used to play a card from a hand and display it *)

    VAR T : INTEGER;

  BEGIN (*tcribbagewin.play_card*)
   PPlayedLast := Player; // set flag on who played last card
   IF Player
    THEN PPly[4-PCardsLeft] := Hnd[CardIdx]
   ELSE CPly[4-CardsLeft] := Hnd[CardIdx];
   T := QC_GetCV_Face10(Hnd[CardIdx]); // face cards are 10's
   HandTally := HandTally + T;
   LblTally.Caption := IntToStr(HandTally);
   PB_Play.Repaint;
   Inc(CardsPlayed); PlayedCards[CardsPlayed] := Hnd[CardIdx];
   Application.ProcessMessages;
  END; (*tcribbagewin.play_card*)

(************************************************************************)

 PROCEDURE TCribbageWin.Get_Points(Crib : BOOLEAN);
     (* procedure to get points for hand or crib from player *)

    VAR APoints, Points, MPoints : INTEGER;

  BEGIN (*tcribbagewin.get_points*)
   IF Crib
    THEN Figure_Points(CribHand, TRUE, APoints) // actual points in crib
   ELSE Figure_Points(PHnd, FALSE, APoints); // actual points in hand
   // get points now
   PointDlg := TPOINTDLG.Create(Self);
   PointDlg.Left := Left + 190; PointDlg.Top := Top + 50;
   PointDlg.CribPoints := Crib; PointDlg.ActualPoints := APoints;
   PointDlg.ShowModal;
   Points := PointDlg.EnteredPoints;
   PointDlg.Free;
   // score it
   MovePeg(Points, TRUE); MPoints := APoints - Points;
   IF NOT(GameOver) AND (MPoints <> 0)
    THEN BEGIN {muggins :)}
          Show_Message(Self, Format(mMuggins,[MPoints]));
          MovePeg(MPoints, FALSE);
         END; {then}
  END; (*tcribbagewin.get_points*)

(************************************************************************)

 PROCEDURE TCribbageWin.Show_Cards(Hnd : TCARDHAND);
     (* procedure used to show the cards in a hand *)

    VAR I : INTEGER;

  BEGIN (*tcribbagewin.show_cards*)
   FOR I := 1 TO 4 DO
    PutCardInImage(ImgCards[I], Hnd[I]);
  END; (*tcribbagewin.show_cards*)

(************************************************************************)

 PROCEDURE TCribbageWin.Clear31(Player : BOOLEAN);
      (* procedure to clear the board once 31 is reached *)
      (* - displays message here so 31 score comes last  *)

     VAR sWho : STRING[10];

  BEGIN (*tcribbagewin.clear31*)
   IF Player THEN sWho := mYou ELSE sWho := mMe;
   Show_Message(Self, sWho + mTwo31);
   ClearPlayedCards;
  END; (*tcribbagewin.clear31*)

(************************************************************************)

INITIALIZATION
  RegisterClasses ([TCribbageWin, TDrawGrid, TImage, TBevel, TButton, TPaintBox,
    TBitBtn, TLabel, TPopupMenu, TMenuItem, TTimer]);
END. (*of unit*)
