unit Room;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ExtCtrls, AGS.Room;

type
  TRoomForm = class(TForm)
    Backcb: TComboBox;
    Label1: TLabel;
    BackImg: TImage;
    ORbtn: TButton;
    ScrollBar1: TScrollBar;
    odCRM: TOpenDialog;
    procedure ORbtnClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  RoomForm: TRoomForm;
  ERoom: TRoom;

implementation

{$R *.dfm}

procedure TRoomForm.ORbtnClick(Sender: TObject);
begin
  if odCRM.Execute then
  begin
    ERoom := TRoom.Create(odCRM.FileName);
    ERoom.Free;
  end;
end;

end.
