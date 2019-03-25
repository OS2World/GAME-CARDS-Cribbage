UNIT CribPDlg;

(*                                                                      *)
(* AUTHOR: Michael G. Slack                    DATE WRITTEN: 2001/03/17 *)
(* ENVIRONMENT: Sibyl                                                   *)
(*                                                                      *)
(* Unit defines the dialog to get players points for hand and crib.     *)
(*                                                                      *)
(* -------------------------------------------------------------------- *)
(*                                                                      *)
(* REVISED: yyyy/mm/dd -                                                *)
(*                                                                      *)

INTERFACE

 USES Classes, Forms, Graphics, Buttons, StdCtrls, ExtCtrls, Spin;

 TYPE TPointDlg = CLASS(TForm)
                   Bevel1    : TBevel;
                   Label1    : TLabel;
                   SE_Points : TSpinEdit;
                   BtnDone   : TBitBtn;
                   Procedure PointDlgOnCreate(Sender : TObject);
                   Procedure PointDlgOnShow(Sender : TObject);
                   Procedure BtnDoneOnClick(Sender : TObject);
                  PRIVATE
                   {Insert private declarations here}
                  PUBLIC
                   CribPoints : BOOLEAN;
                   ActualPoints,
                   EnteredPoints : INTEGER;
                  END;

 VAR PointDlg : TPointDlg;

(************************************************************************)

IMPLEMENTATION

 USES Dialogs, CribSup, CribMsg;

(************************************************************************)

 Procedure TPointDlg.PointDlgOnCreate(Sender : TObject);
  Begin
   ActualPoints := 0; EnteredPoints := 0; CribPoints := FALSE;
  End;

(************************************************************************)

 Procedure TPointDlg.PointDlgOnShow(Sender : TObject);
  Begin
   IF CribPoints THEN Caption := mPDlgTit1;
   // focus the edit control
   SE_Points.Edit.Focus;
  End;

(************************************************************************)

 Procedure TPointDlg.BtnDoneOnClick(Sender : TObject);
  Begin
   EnteredPoints := SE_Points.Value;
   // check it
   IF EnteredPoints > ActualPoints
    THEN BEGIN {nope - too many points for hand shown}
          Show_Message_Pos(Self, mPointErr, 15, 30);
         END {then}
   ELSE DismissDlg(mrOK);
  End;

(************************************************************************)

INITIALIZATION
  RegisterClasses ([TPointDlg, TBitBtn, TBevel, TSpinEdit, TLabel]);
END. (*of unit*)
