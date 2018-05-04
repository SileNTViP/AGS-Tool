unit Main;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, AGS.Clib32, AGS.Translation, Xml.xmldom,
  Xml.XMLIntf, Xml.Win.msxmldom, Xml.XMLDoc, AGS.GameDta, AGS.Script, AGS.Room, AGS.Font, stdActns,
  AGS.Sprite, Room;

type
  TMainForm = class(TForm)
    ExtractEXE: TButton;
    CompileTRS: TButton;
    DecompileTRA: TButton;
    odGame: TOpenDialog;
    odTRS: TOpenDialog;
    odTRA: TOpenDialog;
    Label1: TLabel;
    TRSAdder: TButton;
    SaveTXT: TButton;
    ImportTXT: TButton;
    sdTRS: TSaveDialog;
    GroupBox1: TGroupBox;
    GroupBox2: TGroupBox;
    ExtrLIB: TButton;
    odLIB: TOpenDialog;
    Button1: TButton;
    GroupBox3: TGroupBox;
    GroupBox4: TGroupBox;
    CreateTXT2TRS: TButton;
    odTXT: TOpenDialog;
    ExtrEXE: TButton;
    CreateFrom2TXT: TButton;
    GroupBox5: TGroupBox;
    wfn2bdf: TButton;
    odWFN: TOpenDialog;
    odBDF: TOpenDialog;
    bdf2wfn: TButton;
    MakeEXE: TButton;
    sdEXE: TSaveDialog;
    GroupBox6: TGroupBox;
    ExtrSpr: TButton;
    GenSpr: TButton;
    odSPR: TOpenDialog;
    sdSPR: TSaveDialog;
    RoomBtn: TButton;
    btn1: TButton;
    btn2: TButton;
    CreateFromDTA: TButton;
    odDTACRM: TOpenDialog;
    sdDTACRM: TSaveDialog;
    chkTRAV2: TCheckBox;
    procedure ExtractEXEClick(Sender: TObject);
    function ReadGameProj(FileName: string): Boolean;
    procedure WriteGameProj(FileName: string);
    procedure CompileTRSClick(Sender: TObject);
    procedure DecompileTRAClick(Sender: TObject);
    procedure TRSAdderClick(Sender: TObject);
    procedure SaveTXTClick(Sender: TObject);
    procedure ImportTXTClick(Sender: TObject);
    procedure ExtrLIBClick(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure CreateTXT2TRSClick(Sender: TObject);
    procedure ExtrEXEClick(Sender: TObject);
    procedure CreateFrom2TXTClick(Sender: TObject);
    procedure wfn2bdfClick(Sender: TObject);
    procedure bdf2wfnClick(Sender: TObject);
    procedure MakeEXEClick(Sender: TObject);
    procedure ExtrSprClick(Sender: TObject);
    procedure GenSprClick(Sender: TObject);
    procedure RoomBtnClick(Sender: TObject);
    procedure btn1Click(Sender: TObject);
    procedure btn2Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure WriteLog(Text: String);
    procedure CreateFromDTAClick(Sender: TObject);
  private
    function GetTempDirectory: AnsiString;
    { Private declarations }
  public
    { Public declarations }
  end;

var
  MainForm: TMainForm;
  RoomForm: TRoomForm;
  clib: TClib32;
  trans: TTranslation;
  dta: TGameDta;
  scr: TScript;
  room: TRoom;
  gamepath: AnsiString;
  GameName: AnsiString; GameID: uInt;
  log: TextFile;

implementation

{$R *.dfm}

procedure TMainForm.GenSprClick(Sender: TObject);
var
  sprd: TBrowseForFolder;
  spr: TSprite;
  newspr: AnsiString;
begin
  if sdSPR.Execute then
  begin
    newspr := sdSPR.FileName;
    sprd := TBrowseForFolder.Create(nil);
    sprd.Folder := ExtractFilePath(newspr);
    if sprd.Execute then
    begin
      spr := TSprite.Create;
      spr.GenerateSpriteFromDir(sprd.Folder, newspr);
      spr.Free;
      Application.MessageBox('Buildig sprite library done!', 'Info', MB_OK);
    end;
    sprd.Free;
  end;
end;

function TMainForm.GetTempDirectory: AnsiString;
var
  tempFolder: array[0..MAX_PATH] of AnsiChar;
begin
  GetTempPathA(MAX_PATH, @tempFolder);
  Result := tempFolder;
end;

procedure TMainForm.ImportTXTClick(Sender: TObject);
var
  newf: AnsiString;
begin
  if sdTRS.Execute then
  begin
    gamepath := ExtractFilePath(sdTRS.FileName);
    trans := TTranslation.Create;
    trans.ImportTextFromDir(gamepath);
    trans.WriteToDecompiled(sdTRS.FileName);
    trans.Free;
    Application.MessageBox('Creating TRS from Inbox done!', 'Info', MB_OK);
  end;
end;

procedure TMainForm.MakeEXEClick(Sender: TObject);
var
  exed: TBrowseForFolder;
  execli: TClib32;
  sRec: TSearchRec;
  isFound: Integer;
begin
  if sdEXE.Execute then
  begin
    gamepath := ExtractFilePath(sdEXE.FileName);
    exed := TBrowseForFolder.Create(nil);
    exed.Folder := gamepath;
    if exed.Execute then
    begin
      execli := TClib32.Create;
      execli.WriteToFile(sdEXE.FileName);
      execli.WriteGameEngine(exed.Folder + '\acwin.exe');
      isFound := FindFirst(exed.Folder + '\*.*', faAnyFile, sRec);
      while isFound = 0 do
      begin
        if (sRec.Name <> '.') and (sRec.Name <> '..') and (LowerCase(sRec.Name) <> 'acwin.exe') then
          execli.AddFile(exed.Folder + '\' + sRec.Name);
        isFound := FindNext(sRec);
      end;
      execli.WriteFiles;
      execli.Close;
      execli.Free;
      Application.MessageBox('Bulding EXE done!', 'Info', MB_OK);
    end;
    exed.Free;
  end;
end;

procedure TMainForm.ExtractEXEClick(Sender: TObject);
var
  i, m, strcnt, fullstrcount: Integer;
begin
  if odGame.Execute then
  begin
    fullstrcount := 0;
    WriteLog('Begin extracting from game library.');
    WriteLog('Try to open "' + ExtractFileName(odGame.FileName) + '" library.');
    clib := TClib32.Create(odGame.FileName);
    WriteLog('Found ' + IntToStr(clib.GetNumFiles) + ' files in library.');
    gamepath := ExtractFilePath(odGame.FileName);

//    DeleteFile(gamepath + 'Extracted.txt');
    trans := TTranslation.Create;
    WriteLog('Search file: ac2game.dta or game28.dta');
    if clib.OpenFile('ac2game.dta') then
    begin
      WriteLog('Try to open file: ac2game.dta');
      dta := TGameDta.Create(clib.GetFileStream);
      if dta.GetDtaErr = DTA_ERROR_INC_VER then
      begin
        Application.MessageBox('Unsupported DTA version!', 'Error', MB_OK);
        WriteLog('Unsupported DTA ' + dta.GetDtaVersionString + ' version!');
        dta.Free;
        clib.Close;
        Exit;
      end;
      WriteLog('Game engine version: ' + dta.GetDtaVersionString);
      GameName := dta.GetGameName;
      WriteLog('Found game name: ' + GameName);
      GameID := dta.GetGameID;
      WriteLog('Found game id: ' + IntToStr(GameID));
      strcnt := dta.GetExtractedStringsCount;
      Inc(fullstrcount, strcnt);
      for i := 0 to strcnt - 1 do
        trans.Add(dta.GetExtractedString(i), dta.GetExtractedString(i));
      dta.Free;
      WriteLog('Extracting done. Found strings: ' + IntToStr(strcnt));
      clib.Close;
    end else if clib.OpenFile('game28.dta') then
    begin
      WriteLog('Try to open file: game28.dta');
      dta := TGameDta.Create(clib.GetFileStream);
      if dta.GetDtaErr = DTA_ERROR_INC_VER then
      begin
        Application.MessageBox('Unsupported DTA version!', 'Error', MB_OK);
        dta.Free;
        clib.Close;
        WriteLog('Unsupported DTA ' + dta.GetDtaVersionString + ' version!');
        Exit;
      end;
      WriteLog('Game engine version: ' + dta.GetDtaVersionString);
      GameName := dta.GetGameName;
      WriteLog('Found game name: ' + GameName);
      GameID := dta.GetGameID;
      WriteLog('Found game id: ' + IntToStr(GameID));
      strcnt := dta.GetExtractedStringsCount;
      Inc(fullstrcount, strcnt);
      for i := 0 to strcnt - 1 do
        trans.Add(dta.GetExtractedString(i), dta.GetExtractedString(i));
      dta.Free;
      WriteLog('Extracting done. Found strings: ' + IntToStr(strcnt));
      clib.Close;
    end;
    for i := 1 to 999 do
      if clib.OpenFile('room' + IntToStr(i) + '.crm') then
      begin
        WriteLog('Try to open file: room' + IntToStr(i) + '.crm');
        room := TRoom.Create(clib.GetFileStream);
        strcnt := room.GetExtractedStringsCount;
        Inc(fullstrcount, strcnt);
        for m := 0 to strcnt - 1 do
          trans.Add(room.GetExtractedString(m), room.GetExtractedString(m));
        room.Free;
        WriteLog('Extracting done. Found strings: ' + IntToStr(strcnt));
        clib.Close;
      end;
    if FileExists(gamepath + '\game.id') then
      DeleteFile(gamepath + '\game.id');
    WriteLog('Writing game Name and ID into game.id file.');
    WriteGameProj(gamepath + '\game.id');
    WriteLog('Extracted ' + IntToStr(fullstrcount) + ' strings.');
    clib.Free;
    trans.WriteToFullText(gamepath + 'Extracted.txt');
    trans.Free;
    Application.MessageBox('Extracting done!', 'Info', MB_OK);
  end;
end;

procedure TMainForm.CreateFromDTAClick(Sender: TObject);
var
  fext: String;
  fullstrcount, strcnt, i: Integer;
begin
 if odDTACRM.Execute then
 begin
   if sdDTACRM.Execute then
   begin
     fext := ExtractFileExt(odDTACRM.FileName);
     trans := TTranslation.Create;
     if fext = '.crm' then
     begin
       WriteLog('Try to open file: ' + ExtractFileName(odDTACRM.FileName));
       room := TRoom.Create(odDTACRM.FileName);
       strcnt := room.GetExtractedStringsCount;
       Inc(fullstrcount, strcnt);
       for i := 0 to strcnt - 1 do
         trans.Add(room.GetExtractedString(i), room.GetExtractedString(i));
       room.Free;
       WriteLog('Extracting done. Found strings: ' + IntToStr(strcnt));
     end;
     if fext = '.dta' then
     begin
       WriteLog('Try to open file: ' + ExtractFileName(odDTACRM.FileName));
       dta := TGameDta.Create(odDTACRM.FileName);
       if dta.GetDtaErr = DTA_ERROR_INC_VER then
       begin
         Application.MessageBox('Unsupported DTA version!', 'Error', MB_OK);
         WriteLog('Unsupported DTA ' + dta.GetDtaVersionString + ' version!');
         dta.Free;
         clib.Close;
         Exit;
       end;
       WriteLog('Game engine version: ' + dta.GetDtaVersionString);
       GameName := dta.GetGameName;
       WriteLog('Found game name: ' + GameName);
       GameID := dta.GetGameID;
       WriteLog('Found game id: ' + IntToStr(GameID));
       strcnt := dta.GetExtractedStringsCount;
       Inc(fullstrcount, strcnt);
       for i := 0 to strcnt - 1 do
         trans.Add(dta.GetExtractedString(i), dta.GetExtractedString(i));
       dta.Free;
       WriteLog('Extracting done. Found strings: ' + IntToStr(strcnt));
     end;
     trans.WriteToFullText(sdDTACRM.FileName);
     trans.Free;
     Application.MessageBox('Extracting done!', 'Info', MB_OK);
   end;
 end;
end;

procedure TMainForm.WriteGameProj(FileName: string);
var
  uid: TFileStream;
  names: Integer;
begin
  uid := TFileStream.Create(FileName, fmCreate);
  names := Length(GameName);
  uid.Write(names, 1);
  uid.Write(PAnsiChar(GameName)^, names);
  uid.Write(GameID, 4);
  uid.Free;
end;

procedure TMainForm.ExtrEXEClick(Sender: TObject);
var
  fName, buf: AnsiString;
  i: Integer;
begin
  if odGame.Execute then
  begin
    gamepath := ExtractFilePath(odGame.FileName);
    fName := ExtractFileName(odGame.FileName);
    for i := 1 to Length(fName) do
      if fName[i] = '.' then Break
      else buf := buf + fName[i];
    ForceDirectories(gamepath + '\' + buf);
    clib := TClib32.Create(odGame.FileName);
    clib.ExtractGameEngine(gamepath + buf + '\acwin.exe');
    clib.ExtractAllFiles(gamepath + buf + '\');
    clib.Free;
    Application.MessageBox('Extracting from EXE done!', 'Info', MB_OK);
  end;
end;

procedure TMainForm.ExtrLIBClick(Sender: TObject);
var
  fName, buf: AnsiString;
  i: Integer;
begin
  if odLIB.Execute then
  begin
    gamepath := ExtractFilePath(odLIB.FileName);
    fName := ExtractFileName(odLIB.FileName);
    for i := 1 to Length(fName) do
      if fName[i] = '.' then Break
      else buf := buf + fName[i];
    ForceDirectories(gamepath + '\' + buf);
    clib := TClib32.Create(odLIB.FileName);
    clib.ExtractAllFiles(gamepath + buf + '\');
    clib.Free;
    Application.MessageBox('Extracting files done!', 'Info', MB_OK);
  end;
end;

procedure TMainForm.ExtrSprClick(Sender: TObject);
var
  spr: TSprite;
  aa: Integer;
begin
  if odSPR.Execute then
  begin
    gamepath := ExtractFilePath(odSPR.FileName);
    spr := TSprite.Create(odSPR.FileName);
    spr.ExtractAllSprites(gamepath + 'acsprset\');
//    spr.CheckExtract(1);
    spr.Free;
    Application.MessageBox('Extracting sprites done!', 'Info', MB_OK);
  end;
end;

procedure TMainForm.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  WriteLog('Exit from tool.');
  WriteLog('');
end;

procedure TMainForm.FormCreate(Sender: TObject);
begin
  WriteLog('Tool started.');
end;

procedure TMainForm.WriteLog(Text: String);
begin
  AssignFile(log, 'agstool.log');
  if not FileExists('agstool.log') then
    ReWrite(log)
  else
    Append(log);
  if Text <> '' then
    Writeln(log, DateToStr(Date) + ' ' + FormatDateTime('hh:nn:ss :', Now) + ' ' + Text)
  else
    Writeln(log, '');
  CloseFile(log);
end;

procedure TMainForm.bdf2wfnClick(Sender: TObject);
var
  wfn: TWFNFont;
  newf: AnsiString;
begin
  if odBDF.Execute then
  begin
    wfn := TWFNFont.Create(odBDF.FileName);
    newf := ChangeFileExt(odBDF.FileName, '.wfn');
    wfn.WriteToWFN(newf);
    wfn.Free;
    Application.MessageBox('Font file created!', 'Info', MB_OK);
  end;
end;

procedure TMainForm.btn1Click(Sender: TObject);
var
  strcnt, m: Integer;
begin
  trans := TTranslation.Create;
  room := TRoom.Create('room1.crm');
  strcnt := room.GetExtractedStringsCount;
  for m := 0 to strcnt - 1 do
    trans.Add(room.GetExtractedString(m), room.GetExtractedString(m));
  room.Free;
//  clib.Close;
  trans.WriteToFullText('Extracted.txt');
  trans.Free;
  Application.MessageBox('Extracting done!', 'Info', MB_OK);
end;

procedure TMainForm.btn2Click(Sender: TObject);
var
  dta : TGameDta;
  i, strcnt: Integer;
begin
  trans := TTranslation.Create;
  dta := TGameDta.Create('game28.dta');
  strcnt := dta.GetExtractedStringsCount;
  for i := 0 to strcnt - 1 do
    trans.Add(dta.GetExtractedString(i), dta.GetExtractedString(i));
  dta.Free;
  trans.WriteToFullText('Extracted.txt');
  trans.Free;
  Application.MessageBox('Extracting done!', 'Info', MB_OK);
end;

procedure TMainForm.Button1Click(Sender: TObject);
var
  tmpscr: TScript;
  tmptrs: TTranslation;
  i: Integer;
begin
  tmpscr := TScript.Create('Kiosk.asc');
//  tmpscr.Dump('dump.txt');
  tmpscr.ExtractStrings2;
  tmptrs := TTranslation.Create;
  for i := 0 to tmpscr.GetExtractedStringsCount - 1 do
    tmptrs.Add(tmpscr.GetExtractedString(i), '');
  tmptrs.WriteToFullText('Kiosk.txt');
  tmptrs.Free;
  tmpscr.Free;
end;

procedure TMainForm.wfn2bdfClick(Sender: TObject);
var
  wfn: TWFNFont;
  newf: AnsiString;
begin
  if odWFN.Execute then
  begin
    wfn := TWFNFont.Create(odWFN.FileName);
    newf := ChangeFileExt(odWFN.FileName, '.bdf');
    wfn.WriteToBDF(newf);
    wfn.Free;
    Application.MessageBox('Font file created!', 'Info', MB_OK);
  end;
end;

procedure TMainForm.CompileTRSClick(Sender: TObject);
var
  newf: AnsiString;
begin
  if odTRS.Execute then
  begin
    gamepath := ExtractFilePath(odTRS.FileName);
    if ReadGameProj(gamepath + '\game.id') then
    begin
      trans := TTranslation.Create(odTRS.FileName);
      newf := ChangeFileExt(odTRS.FileName, '.tra');
      trans.bVersion2 := chkTRAV2.Checked;
      trans.SetGameName(GameName);
      trans.SetGameID(GameID);
      trans.WriteToCompiled(newf);
      trans.Free;
      Application.MessageBox('Compiling done!', 'Info', MB_OK);
    end else
      Application.MessageBox('File "game.id" not found!', 'Error', MB_OK);
  end;
end;

procedure TMainForm.CreateFrom2TXTClick(Sender: TObject);
var
  origtxt, transtxt, newtrs: AnsiString;
begin
  if odTXT.Execute then
  begin
    origtxt := odTXT.FileName;
    if odTXT.Execute then
    begin
      transtxt := odTXT.FileName;
      if sdTRS.Execute then
      begin
        newtrs := sdTRS.FileName;
        trans := TTranslation.Create;
        trans.ReadFrom2Texts(origtxt, transtxt);
        trans.WriteToDecompiled(newtrs);
        trans.Free;
        Application.MessageBox('Creating TRS is done!', 'Info', MB_OK);
      end;
    end;
  end;
end;

procedure TMainForm.CreateTXT2TRSClick(Sender: TObject);
var
  newf: AnsiString;
begin
  if odTXT.Execute then
  begin
    gamepath := ExtractFilePath(odTXT.FileName);
    newf := ChangeFileExt(odTXT.FileName, '.trs');
    trans := TTranslation.Create;
    trans.ReadFullText(odTXT.FileName);
    trans.WriteToDecompiled(newf);
    trans.Free;
    Application.MessageBox('Creating TRS is done!', 'Info', MB_OK);
  end;
end;

procedure TMainForm.DecompileTRAClick(Sender: TObject);
var
  newf: AnsiString;
begin
  if odTRA.Execute then
  begin
    gamepath := ExtractFilePath(odTRA.FileName);
    trans := TTranslation.Create(odTRA.FileName);
    newf := ChangeFileExt(odTRA.FileName, '.trs');
    trans.WriteToDecompiled(newf);
    GameName := trans.GetGameName;
    GameID := trans.GetGameID;
    WriteGameProj(gamepath + '\game.id');
    trans.Free;
    Application.MessageBox('Decompiled!', 'Info', MB_OK);
  end;
end;

function TMainForm.ReadGameProj(FileName: string): Boolean;
var
  uid: TFileStream;
  names: Byte;
begin
  Result := False;
  if FileExists(FileName) then
  begin
    uid := TFileStream.Create(FileName, fmOpenRead);
    uid.Read(names, 1);
    SetLength(GameName, names);
    uid.Read(PAnsiChar(GameName)^, names);
    uid.Read(GameID, 4);
    uid.Free;
    Result := True;
  end;
end;

procedure TMainForm.RoomBtnClick(Sender: TObject);
begin
  RoomForm := TRoomForm.Create(self);
  RoomForm.Show;
end;

procedure TMainForm.SaveTXTClick(Sender: TObject);
var
  newf: AnsiString;
begin
  if odTRS.Execute then
  begin
    gamepath := ExtractFilePath(odTRS.FileName);
    ForceDirectories(gamepath + '\outbox');
    trans := TTranslation.Create(odTRS.FileName);
//    newf := ChangeFileExt(odTRS.FileName, '.txt');
    trans.WriteToText(gamepath + '\outbox');
    trans.Free;
    Application.MessageBox('Extracting from TRS is done!', 'Info', MB_OK);
  end;
end;

procedure TMainForm.TRSAdderClick(Sender: TObject);
var
  oldtrs, newtrs: AnsiString;
  newtr: TTranslation;
begin
  oldtrs := ''; newtrs := '';
  if odTRS.Execute then
  begin
    oldtrs := odTRS.FileName;
    if odTRS.Execute then
    begin
      newtrs := odTRS.FileName;
      if not FileExists(oldtrs) then Exit;
      if not FileExists(newtrs) then Exit;
      trans := TTranslation.Create(oldtrs);
      newtr := TTranslation.Create(newtrs);
      trans.AddDict(newtr.GetDict, newtr.GetDictSize);
      trans.WriteToDecompiled(oldtrs);
      newtr.Free;
      trans.Free;
      Application.MessageBox('Adding new strings is done!', 'Info', MB_OK);
    end;
  end;
end;

end.
