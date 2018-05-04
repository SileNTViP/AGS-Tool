program AGSTool;

uses
  Vcl.Forms,
  Main in 'Main.pas' {MainForm},
  Room in 'Room.pas' {RoomForm};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TMainForm, MainForm);
  Application.CreateForm(TRoomForm, RoomForm);
  Application.Run;
end.
