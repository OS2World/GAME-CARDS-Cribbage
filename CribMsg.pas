UNIT CribMsg;

(*                                                                      *)
(* AUTHOR: Michael G. Slack                    DATE WRITTEN: 2001/03/21 *)
(* ENVIRONMENT: Sibyl                                                   *)
(*                                                                      *)
(* Unit defines a dialog to use as a message dialog.                    *)
(*                                                                      *)
(* -------------------------------------------------------------------- *)
(*                                                                      *)
(* REVISED: yyyy/mm/dd -                                                *)
(*                                                                      *)

INTERFACE

 USES Classes, Forms, Graphics, Buttons, StdCtrls, ExtCtrls;

 TYPE TDlgMessage = CLASS(TForm)
                     Bevel1     : TBevel;
                     Image1     : TImage;
                     LblMessage : TLabel;
                     OkButton   : TBitBtn;
                     Procedure DlgMessageOnCreate(Sender : TObject);
                     Procedure DlgMessageOnShow(Sender : TObject);
                    PRIVATE
                     {Insert private declarations here}
                    PUBLIC
                     Msg : STRING[200];
                    END;

 VAR DlgMessage : TDlgMessage;

(************************************************************************)

 PROCEDURE Show_Message(Par : TFORM; S : STRING);
     (* procedure to display message dialog with given message string *)
     (*  - show message dialog at parent left + 100, parent top + 50  *)

 PROCEDURE Show_Message_Pos(Par : TFORM; S : STRING; DL, DT : INTEGER);
     (* procedure to display message dialog with given message string *)
     (* at a given delta (left/top) to the parent window              *)

(************************************************************************)

IMPLEMENTATION

(************************************************************************)

 Procedure TDlgMessage.DlgMessageOnCreate (Sender: TObject);
  Begin
   Msg := 'Set Your Message Now!';
   // set default position
   Top := (Screen.Height DIV 2) - (Height DIV 2);
   Left := (Screen.Width DIV 2) - (Width DIV 2);
  End;

(************************************************************************)

 Procedure TDlgMessage.DlgMessageOnShow(Sender : TObject);
  Begin
   LblMessage.Caption := Msg;
  End;

(************************************************************************)

 PROCEDURE Show_Message(Par : TFORM; S : STRING);
     (* procedure to display message dialog with given message string *)

  BEGIN (*show_message*)
   Show_Message_Pos(Par, S, 100, 50);
  END; (*show_message*)

(************************************************************************)

 PROCEDURE Show_Message_Pos(Par : TFORM; S : STRING; DL, DT : INTEGER);
     (* procedure to display message dialog with given message string *)
     (* at a given delta (left/top) to the parent window              *)

  BEGIN (*show_message*)
   DlgMessage := TDLGMESSAGE.Create(Par);
   DlgMessage.Left := Par.Left + DL; DlgMessage.Top := Par.Top + DT;
   DlgMessage.Msg := S;
   DlgMessage.ShowModal;
   DlgMessage.Free;
  END; (*show_message*)

(************************************************************************)

INITIALIZATION
  RegisterClasses ([TDlgMessage, TBitBtn, TBevel, TImage, TLabel]);
END. (*of unit*)
