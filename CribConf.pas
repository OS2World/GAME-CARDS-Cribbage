UNIT CribConf;

(*                                                                      *)
(* AUTHOR: Michael G. Slack                    DATE WRITTEN: 2001/03/10 *)
(* ENVIRONMENT: Sibyl                                                   *)
(*                                                                      *)
(* Unit defines the configuration dialog used by the cribbage program.  *)
(*                                                                      *)
(* -------------------------------------------------------------------- *)
(*                                                                      *)
(* REVISED: 2001/05/22 - Fixed a display problem with the initial       *)
(*                       card back.                                     *)
(*                                                                      *)

INTERFACE

 USES Classes, Forms, Graphics, TabCtrls, Buttons, StdCtrls, ExtCtrls;

 TYPE TConfDlg = CLASS(TForm)
                  ConfNoteBook   : TTabbedNotebook;
                  OkButton       : TBitBtn;
                  CancelButton   : TBitBtn;
                  CB_UseSound    : TCheckBox;
                  CB_Prompts     : TCheckBox;
                  CB_PlayerStart : TCheckBox;
                  CB_Verbose     : TCheckBox;
                  LB_Choice      : TListBox;
                  Label1         : TLabel;
                  ImgBack        : TImage;
                  Procedure ConfDlgOnShow(Sender : TObject);
                  Procedure ConfNoteBookOnSetupShow(Sender : TObject);
                  Procedure OkButtonOnClick(Sender : TObject);
                  Procedure LB_ChoiceOnItemSelect(Sender : TObject; Index : LongInt);
                 PRIVATE
                  CurCardBack : LONGWORD;
                  AutoSelect  : BOOLEAN;
                 PUBLIC
                  {Insert public declarations here}
                 END;

 VAR ConfDlg : TConfDlg;

(************************************************************************)

IMPLEMENTATION

 USES SysUtils, QCardU, CribSup;

(************************************************************************)

 Procedure TConfDlg.ConfDlgOnShow(Sender : TObject);

    VAR I : INTEGER;

  Begin
   CB_Prompts.Checked := PromptMe;
   CB_PlayerStart.Checked := AlwaysStarts;
   CB_UseSound.Checked := UseSounds;
   CB_Verbose.Checked := VerboseMode;
   CurCardBack := CardBackPic;
   AutoSelect := TRUE; // need this so select statement won't try to set image
   // load list box
   FOR I := Min_CardBackNum TO Max_CardBackNum DO
    LB_Choice.Items.Add(IntToStr(I));
   I := LB_Choice.Items.IndexOf(IntToStr(CurCardBack));
   IF I <> -1 THEN LB_Choice.ItemIndex := I; // select current choice
  End;

(************************************************************************)

 Procedure TConfDlg.ConfNoteBookOnSetupShow(Sender : TObject);
  Begin
   PutCardInImage(ImgBack, CardBackPic);
  End;

(************************************************************************)

 Procedure TConfDlg.OkButtonOnClick(Sender : TObject);
  Begin
   PromptMe := CB_Prompts.Checked;
   AlwaysStarts := CB_PlayerStart.Checked;
   UseSounds := CB_UseSound.Checked;
   VerboseMode := CB_Verbose.Checked;
   CardBackPic := CurCardBack;
   DismissDlg(mrOK);
  End;

(************************************************************************)

 Procedure TConfDlg.LB_ChoiceOnItemSelect(Sender : TObject; Index : LongInt);
  Begin
   IF (Index <> -1) AND NOT(AutoSelect)
    THEN BEGIN {get new image}
          CurCardBack := StrToInt(LB_Choice.Items[Index]); // save it
          PutCardInImage(ImgBack, CurCardBack);
        END; {then}
   AutoSelect := FALSE;
  End;

(************************************************************************)

INITIALIZATION
  RegisterClasses ([TConfDlg, TTabbedNotebook, TBitBtn, TCheckBox, TListBox,
    TLabel, TImage]);
END. (*of unit*)
