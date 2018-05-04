unit AGS.GameDta;

interface

uses
  Winapi.Windows, System.Variants, System.Classes, System.SysUtils, System.Types, AGS.Script;

type
  GUIListBox = record
    numItems, selected, topItem, mousexp, mouseyp: Integer;
    rowheight, num_items_fit: Integer;
    font, textcol, backcol, exflags: Integer;
  end;
  GUIObject = record
    guin, objn: Integer;
    flags: uInt;
    x, y: Integer;
    wid, hit: Integer;
    zorder: Integer;
    activated: Integer;
    scriptName: array [0..25] of AnsiChar;
    eventHandlers: array [0..9] of array [0..30] of AnsiChar;
  end;
  GUIMain = record
    Id:                   Integer;
    Name:                 AnsiString;
    Flags:                Integer;
    X, Y, Width, Height:  Integer;
    BgColor:              Integer;
    BgImage:              Integer;
    FgColor:              Integer;
    Padding:              Integer;
    PopupStyle:           Integer;
    PopupAtMouseY:        Integer;
    Transparency:         Integer;
    ZOrder:               Integer;
    FocusCtrl:            Integer;
    HighlightCtrl:        Integer;
    MouseOverCtrl:        Integer;
    MouseWasAtX:          Integer;
    MouseWasAtY:          Integer;
    MouseDownCtrl:        Integer;
    clickEventHandler:    AnsiString;
    ControlCount:         Integer;
    Visibility:           Integer;
  end;
  DialogTopic = record
    optionnames: array [0..29] of array [0..149] of AnsiChar;
    optionflags: array [0..29] of Integer;
    optionscripts: Integer;
    entrypoints: array [0..29] of Short;
    startupentrypoint: Short;
    codesize: Short;
    numoptions: Integer;
    topicFlags: Integer;
  end;
  CharacterInfo = record
    defview: Integer;
    talkview: Integer;
    view: Integer;
    room, prevroom: Integer;
    x, y, wait: Integer;
    flags: Integer;
    following: Short;
    followinfo: Short;
    idleview: Integer;
    idletime, idleleft: Short;
    transparency: Short;
    baseline: Short;
    activeinv: Integer;
    talkcolor: Integer;
    thinkview: Integer;
    blinkview, blinkinterval: Short;
    blinktimer, blinkframe: Short;
    walkspeed_y, pic_yoffs: Short;
    z: Integer;
    walkwait: Integer;
    speech_anim_speed, reserved1: Short;
    blocking_width, blocking_height: Short;
    index_id: Integer;
    pic_xoffs, walkwaitcounter: Short;
    loop, frame: Short;
    walking, animating: Short;
    walkspeed, animspeed: Short;
    inv: array [0..300] of Short;
    actx, acty: Short;
    name: array [0..39] of AnsiChar;
    scrname: array [0..19] of AnsiChar;
    onn: AnsiChar;
  end;
  ViewFrame = record
    pic: Integer;
    xoffs, yoffs: Short;
    speed: Short;
    flags: Integer;
    sound: Integer;
    reserved_for_future: array [0..1] of Integer;
  end;
  ViewLoopNew = record
    numFrames: Short;
    flags: Integer;
    frames: array of ViewFrame;
  end;
  ViewStruct = record
    numLoops: Short;
    loops: array of ViewLoopNew;
  end;
  ViewStruct272 = record
    numLoops: Short;
    numframes: array [0..15] of Short;
    loopFlags: array [0..15] of Integer;
    frames: array [0..15] of array [0..19] of ViewFrame;
  end;
  WordsDictionary = record
    num_words: Integer;
    wordd: array of AnsiString;
    wordnum: array of Short;
  end;
  InteractionScripts = record
    NumEvents: Integer;
    ScriptFuncNames: array of AnsiString;
  end;
  InteractionVariable = record
    name: array [0..22] of AnsiChar;
    vtype: Byte;
    value: Integer;
  end;
  NewInteractionValue = record
    valType: Integer;
    val: Integer;
    extra: Integer;
  end;
  NewInteractionCommand = record
    Reserved: Integer;
    ctype: Integer;
    data: array [0..4] of NewInteractionValue;
    children: Integer;
    parent: Integer;
  end;
  NewInteractionCommandList = record
    numCommands: Integer;
    command: array [0..39] of NewInteractionCommand;
    timesRun: Integer;
  end;
  NewInteraction = record
    NumEvents: Integer;
    EventTypes: array [0..29] of Integer;
    TimesRun: array [0..29] of Integer;
    Response: array [0..29] of Integer;
  end;
  MouseCursor = record
    Pic: Integer;
    HotX: Word;
    HotY: Word;
    View: Word;
    Name: array [0..9] of AnsiChar;
    Flags: Byte;
  end;
  InventoryItemInfo = record
    Name: array [0..24] of AnsiChar;
    Pic: Integer;
    CursorPic: Integer;
    HotX: Integer;
    HotY: Integer;
    Reserved: array [0..4] of Integer;
    Flags: ShortInt;
  end;
  GameSetupStructBase = record
    GameName:           array [0..49] of AnsiChar;
    Options:            array [0..99] of Integer;
    PalUses:            array [0..255] of AnsiChar;
    DefPal:             array [0..255] of Integer;
    NumViews:           Integer;
    NumCharacters:      Integer;
    PlayerCharacter:    Integer;
    TotalScore:         Integer;
    NumInvItems:        Word;
    NumDialog:          Integer;
    NumDlgMessage:      Integer;
    NumFonts:           Integer;
    Color_Depth:        Integer;
    Target_Win:         Integer;
    Dialog_Bullet:      Integer;
    HotDot:             Word;
    HotDotOuter:        Word;
    UniqueID:           Integer;
    NumGUI:             Integer;
    NumCursors:         Integer;
    Default_Resolution: Integer;
    Default_Lipsync_Frame: Integer;
    InvHotDotSprite:    Integer;
    Reserved:           array [0..16] of Integer;
    Messages:           array [0..499] of Integer;
    Dict:               Integer;
    GlobalScript:       Integer;
    Chars:              Integer;
    Compiled_Script:    Integer;
  end;
  GameSetupStruct = record
    Base:               GameSetupStructBase;
    GUID:               array [0..39] of AnsiChar;
    saveGameFileExtension: array [0..19] of AnsiChar;
    saveGameFolderName: array [0..49] of AnsiChar;
    FontFlags:          array [0..14] of AnsiChar;
    FontOutline:        array [0..14] of AnsiChar;
    SpriteFlags:        array [0..29999] of AnsiChar;
    InvInfo:            array [0..300] of InventoryItemInfo;
    MCurs:              array [0..19] of MouseCursor;
    IntrChar:           array of NewInteraction;
    CharScripts:        array of InteractionScripts;
    InvScripts:         array of InteractionScripts;
    Dict:               WordsDictionary;
    views:              array of ViewStruct;
    chars:              array of CharacterInfo;
    lipSyncFrameLetters: array [0..19] of array [0..49] of AnsiChar;
    messages:           array of AnsiString;
    dialog:             array of DialogTopic;
  end;
  TGameDta = class(TObject)
    private
      DtaF: TFileStream;
      DtaVer, DtaErr, numstr, ndmp: Integer;
      DtaVerStr: AnsiString;
      GSS: GameSetupStruct;
      datstr: array of AnsiString;
      function ReadInt32(FS: TFileStream): Integer;
      function ReadInt16(FS: TFileStream): Word;
      function ReadInt8(FS: TFileStream): Byte;
      function ReadString(FS: TFileStream): AnsiString;
      function fgetstring(FS: TFileStream): AnsiString;
      function fgetstring_limit(FS: TFileStream; BufSize: Integer): AnsiString;
      function deserialize_interaction_scripts(FS: TFileStream): InteractionScripts;
      function deserialize_new_interaction(FS: TFileStream): NewInteraction;
      function deserialize_command_list(FS: TFileStream): NewInteractionCommandList;
      function read_string_decrypt(FS: TFileStream): AnsiString;
      function decrypt_text(Str: AnsiString): AnsiString;
      procedure fread_script(FS: TFileStream);
      function read_dictionary(FS: TFileStream): WordsDictionary;
      procedure strcpy(var arr: array of AnsiChar; str: AnsiString);
      procedure AddExtrString(Str: AnsiString);
      procedure GUIObjectRead(FS: TFileStream; Version: Integer);
      procedure read_gui(FS: TFileStream);
      procedure CustomPropertySchema(FS: TFileStream);
      procedure CustomProperties(FS: TFileStream);
      procedure ReadPluginsFromDisk(FS: TFileStream);
      procedure read_dialogs(FS: TFileStream);
      procedure ReadFromStream(FS: TFileStream);
      procedure ReadGameSetupStructBase(FS: TFileStream);
      procedure ReadInventoryItemInfo(FS: TFileStream);
      procedure ReadMouseCursor(FS: TFileStream);
      procedure ReadFromStream2(FS: TFileStream);
      procedure ReadFromFile(FileName: AnsiString);
    public
      constructor Create(FileName: AnsiString); overload;
      constructor Create(FS: TFileStream); overload;
      destructor Free; overload;
      function GetExtractedString(Index: Integer): AnsiString;
      function GetExtractedStringsCount: Integer;
      function GetGameName: AnsiString;
      function GetGameID: uInt;
      function GetDtaErr: UINT;
      function GetDtaVersion: UINT;
      function GetDtaVersionString: AnsiString;
  end;

const
  game_file_sig = 'Adventure Creator Game File v2';
  passwencstring = 'Avis Durgan';

  // Game version
  kGameVersion_Undefined      = 0;
  kGameVersion_230            = 12;
  kGameVersion_240            = 12;
  kGameVersion_250            = 18;
  kGameVersion_251            = 19; // same as 2.52
  kGameVersion_253            = 20;
  kGameVersion_254            = 21;
  kGameVersion_255            = 22;
  kGameVersion_256            = 24;
  kGameVersion_260            = 25;
  kGameVersion_261            = 26;
  kGameVersion_262            = 27;
  kGameVersion_270            = 31;
  kGameVersion_272            = 32;
  kGameVersion_300            = 35;
  kGameVersion_301            = 36;
  kGameVersion_310            = 37;
  kGameVersion_311            = 39;
  kGameVersion_312            = 40;
  kGameVersion_320            = 41;
  kGameVersion_321            = 42;
  kGameVersion_330            = 43;
  kGameVersion_331            = 44;
  kGameVersion_340_1          = 45;
  kGameVersion_340_2          = 46;
  kGameVersion_340_4          = 47;
  kGameVersion_Current        = kGameVersion_340_4;

  // Game resolution
  kGameResolution_Undefined   = -1;
    // definition of 320x200 in very old versions of the engine (somewhere pre-2.56)
  kGameResolution_Default     = 0;
  kGameResolution_320x200     = 1;
  kGameResolution_320x240     = 2;
  kGameResolution_640x400     = 3;
  kGameResolution_640x480     = 4;
  kGameResolution_800x600     = 5;
  kGameResolution_1024x768    = 6;
  kGameResolution_1280x720    = 7;
  kGameResolution_Custom      = 8;

  kGameResolution_LastLoRes   = kGameResolution_320x240;
  kGameResolution_FirstHiRes  = kGameResolution_640x400;

  // GUI version
  kGuiVersion_Initial     = 0;
  kGuiVersion_214         = 100;
  kGuiVersion_222         = 101;
  kGuiVersion_230         = 102;
  kGuiVersion_unkn_103    = 103;
  kGuiVersion_unkn_104    = 104;
  kGuiVersion_260         = 105;
  kGuiVersion_unkn_106    = 106;
  kGuiVersion_unkn_107    = 107;
  kGuiVersion_unkn_108    = 108;
  kGuiVersion_unkn_109    = 109;
  kGuiVersion_270         = 110;
  kGuiVersion_272a        = 111;
  kGuiVersion_272b        = 112;
  kGuiVersion_272c        = 113;
  kGuiVersion_272d        = 114;
  kGuiVersion_272e        = 115;
  kGuiVersion_330         = 116;
  kGuiVersion_331         = 117;
  kGuiVersion_340         = 118;
  kGuiVersion_Current     = kGuiVersion_340;
  // Defines the oldest version of gui data that is complying to current
  // savedgame format; if the loaded game data is of this version or lower,
  // then this value will be written to savedgame instead of current version.
  kGuiVersion_ForwardCompatible = kGuiVersion_272e;

  MAX_INTER_FUNCTION_NAME_LENGTH = 200;
  MAXGLOBALMES = 500;
  MAX_FONTS = 15;
  MAX_SPRITES = 30000;
  MAX_CURSOR = 20;
  MAX_INV = 301;
  MAX_GUID_LENGTH = 40;
  MAX_SG_EXT_LENGTH = 20;
  MAX_SG_FOLDER_LEN = 50;
  MAX_NEWINTERACTION_EVENTS = 30;
  GUI_MAGIC = $cafebeef;
  GUI_VERSION = 116;
  MAX_CUSTOM_PROPERTY_VALUE_LENGTH = 500;
  MAX_OBJS_ON_GUI = 30;
  BASEGOBJ_SIZE = 7;
  MAX_GUIOBJ_SCRIPTNAME_LEN = 25;
  MAX_GUIOBJ_EVENTHANDLER_LEN = 30;
  MAXVIEWNAMELENGTH = 15;
  MAX_GUIOBJ_EVENTS = 10;
  GLF_SGINDEXVALID = 4;

  LEGACY_MAX_OBJS_ON_GUI = 30;

  GUIMAIN_RESERVED_INTS = 5;
  GUIMAIN_NAME_LENGTH = 16;
  GUIMAIN_EVENTHANDLER_LENGTH = 20;
  GUIMAIN_LEGACY_TW_FLAGS_SIZE = 4;

  //Errors
  DTA_ERROR_NULL  = 0;
  DTA_ERROR_INC_FILE = 1;
  DTA_ERROR_INC_VER = 2;

implementation

procedure TGameDta.strcpy(var arr: array of AnsiChar; str: AnsiString);
var
  i: Integer;
begin
  for i := 1 to Length(str) do
    arr[i-1] := str[i];
end;

constructor TGameDta.Create(FileName: AnsiString);
begin
  ReadFromFile(FileName);
end;

constructor TGameDta.Create(FS: TFileStream);
begin
  ReadFromStream(FS);
end;

destructor TGameDta.Free;
begin
  FillChar(GSS.Base, SizeOf(GameSetupStructBase), 0);
end;

function TGameDta.ReadInt32(FS: TFileStream): Integer;
begin
  FS.ReadBuffer(Result, SizeOf(Integer));
end;

function TGameDta.ReadInt16(FS: TFileStream): Word;
begin
  FS.ReadBuffer(Result, SizeOf(Word));
end;

function TGameDta.ReadInt8(FS: TFileStream): Byte;
begin
  FS.ReadBuffer(Result, SizeOf(Integer));
end;

function TGameDta.ReadString(FS: TFileStream): AnsiString;
var
  len: Integer;
begin
  len := ReadInt32(FS);
  if len > 0 then
  begin
    SetLength(Result, len);
    FS.ReadBuffer(PAnsiString(Result)^, len);
  end;
end;

function TGameDta.fgetstring(FS: TFileStream): AnsiString;
begin
  Result := fgetstring_limit(FS, 50000000);
end;

function TGameDta.fgetstring_limit(FS: TFileStream; BufSize: Integer): AnsiString;
var
  buf: AnsiChar;
  curc: Integer;
begin
  Result := ''; curc := 0;
  while True do
  begin
    if (FS.Position = FS.Size) or (curc = BufSize) then
      Break;
    FS.ReadBuffer(buf, SizeOf(buf));
    inc(curc);
    if buf = #0 then
      Break
    else
      Result := Result + buf;
  end;
end;

function TGameDta.deserialize_interaction_scripts(FS: TFileStream): InteractionScripts;
var
  i: Integer;
begin
  Result.NumEvents := ReadInt32(FS);
  SetLength(Result.ScriptFuncNames, Result.NumEvents);
  for i := 0 to Result.NumEvents - 1 do
    Result.ScriptFuncNames[i] := fgetstring_limit(FS, MAX_INTER_FUNCTION_NAME_LENGTH);
end;

function TGameDta.deserialize_new_interaction(FS: TFileStream): NewInteraction;
var
  aa: Integer;
begin
  if ReadInt32(FS) <> 1 then
    Exit;
  Result.NumEvents := ReadInt32(FS);
  if Result.NumEvents > MAX_NEWINTERACTION_EVENTS then
    Exit;
  for aa := 0 to Result.NumEvents - 1 do
    Result.EventTypes[aa] := ReadInt32(FS);
  for aa := 0 to Result.NumEvents - 1 do
    Result.Response[aa] := ReadInt32(FS);
  for aa := 0 to Result.NumEvents - 1 do
    if Result.Response[aa] <> 0 then
      deserialize_command_list(FS);
end;

function TGameDta.deserialize_command_list(FS: TFileStream): NewInteractionCommandList;
var
  aa: Integer;
begin
  FillChar(Result, SizeOf(NewInteractionCommandList), 0);
  Result.numCommands := ReadInt32(FS);
  Result.timesRun := ReadInt32(FS);
  for aa := 0 to Result.numCommands - 1 do
    FS.Read(Result.command[aa], sizeof(NewInteractionCommand));
  for aa := 0 to Result.numCommands - 1 do
    if Result.command[aa].children <> 0 then
      deserialize_command_list(FS);
end;

function TGameDta.read_string_decrypt(FS: TFileStream): AnsiString;
var
  ss: Integer;
  buf: array of Byte;
begin
  Result := '';
  ss := ReadInt32(FS);
  if (ss < 0) or (ss > 5000000) then
  begin
    FS.Seek(-4, soFromCurrent);
    Exit;
  end;
  SetLength(buf, ss);
  FS.Read(PChar(buf)^, ss);
  Result := decrypt_text(AnsiString(buf));
  SetLength(buf, 0);
end;

function TGameDta.decrypt_text(Str: AnsiString): AnsiString;
var
  i, adx: Integer;
  nc: AnsiChar;
begin
  Result := '';
  adx := 1;
  for i := 1 to Length(Str) do
  begin
    nc := AnsiChar(Byte(Str[i]) - Byte(passwencstring[adx]));
    if nc <> #0 then
      Result := Result + nc;
    inc(adx);
    if adx = 12 then
      adx := 1;
  end;
end;

function TGameDta.read_dictionary(FS: TFileStream): WordsDictionary;
var
  i: Integer;
begin
  Result.num_words := ReadInt32(FS);
  SetLength(Result.wordd, Result.num_words);
  SetLength(Result.wordnum, Result.num_words);
  for i := 0 to Result.num_words - 1 do
  begin
    Result.wordd[i] := read_string_decrypt(FS);
    FS.Read(Result.wordnum[i], 2);
  end;
end;

procedure TGameDta.fread_script(FS: TFileStream);
var
  tmpscr: TScript;
  i, strcnt: Integer;
begin
  tmpscr := TScript.Create(FS);
  tmpscr.ExtractStrings3;
  strcnt := tmpscr.GetExtractedStringsCount;
  for i := 0 to strcnt - 1 do
    AddExtrString(tmpscr.GetExtractedString(i));
//  tmpscr.Dump('new.dmp');
  tmpscr.Free;
end;

function TGameDta.GetExtractedString(Index: Integer): AnsiString;
begin
  Result := datstr[Index];
end;

function TGameDta.GetExtractedStringsCount: Integer;
begin
  Result := numstr;
end;

procedure TGameDta.AddExtrString(Str: AnsiString);
var
  Found: Boolean;
  i: Integer;
  newstr: AnsiString;
begin
  Found := False;
  newstr := '';
  for i := 1 to Length(Str) do
    if Str[i] <> #0 then
      newstr := newstr + Str[i];
  if (newStr = '') or (newStr[1] = #0) then
    Exit;
  for i := 0 to numstr - 1 do
  begin
    if datstr[i] = newStr then
    begin
      Found := True;
      Break;
    end;
  end;
  if not Found then
  begin
    inc(numstr);
    SetLength(datstr, numstr);
    datstr[numstr-1] := newStr;
  end;
end;

procedure TGameDta.GUIObjectRead(FS: TFileStream; Version: Integer);
var
  numev, i: Integer;
  tmpobj: GUIObject;
begin
  FillChar(tmpobj, SizeOf(GUIObject), 0);
  FS.Read(tmpobj, 4*BASEGOBJ_SIZE);
  if Version > 106 then
    strcpy(tmpobj.scriptName, fgetstring_limit(FS, MAX_GUIOBJ_SCRIPTNAME_LEN));
  if Version > 108 then
  begin
    i := FS.Position;
    numev := ReadInt32(FS);
    for i := 0 to numev - 1 do
      fgetstring_limit(FS, MAX_GUIOBJ_EVENTHANDLER_LEN + 1);
  end;
end;

procedure TGameDta.read_gui(FS: TFileStream);
var
  gui_version, exflags, numgui, numguibuts, numguilabels, numguiinv, numguislider, numguitext, numguilist, numItems, i, m, buflen: Integer;
  buf: array of AnsiChar;
  tmpgui: GUIMain;
  tmplb: GUIListBox;
begin
  if uInt(ReadInt32(FS)) <> GUI_MAGIC then
    Exit;
  gui_version := ReadInt32(FS);
  if gui_version < kGuiVersion_214 then
    numgui := gui_version
  else if gui_version > kGuiVersion_Current then
    Exit
  else numgui := ReadInt32(FS);
  if (numgui < 0) or (numgui > 1000) then
    Exit;
  SetLength(buf, 1000);
  // import the main GUI elements
  for i := 0 to numgui - 1 do
  begin
    FS.Seek(GUIMAIN_LEGACY_TW_FLAGS_SIZE, soFromCurrent);
    if gui_version < kGuiVersion_340 then
    begin
      SetLength(tmpgui.name, GUIMAIN_NAME_LENGTH);
      FS.ReadBuffer(PAnsiString(tmpgui.name)^, GUIMAIN_NAME_LENGTH);
      SetLength(tmpgui.clickEventHandler, GUIMAIN_EVENTHANDLER_LENGTH);
      FS.ReadBuffer(PAnsiString(tmpgui.clickEventHandler)^, GUIMAIN_EVENTHANDLER_LENGTH);
    end else begin
      tmpgui.name := ReadString(FS);
      tmpgui.clickEventHandler := ReadString(FS);
    end;
    tmpgui.X := ReadInt32(FS);
    tmpgui.Y := ReadInt32(FS);
    tmpgui.Width := ReadInt32(FS);
    tmpgui.Height := ReadInt32(FS);
    tmpgui.FocusCtrl := ReadInt32(FS);
    tmpgui.ControlCount := ReadInt32(FS);
    tmpgui.PopupStyle := ReadInt32(FS);
    tmpgui.PopupAtMouseY := ReadInt32(FS);
    tmpgui.BgColor := ReadInt32(FS);
    tmpgui.BgImage := ReadInt32(FS);
    tmpgui.FgColor := ReadInt32(FS);
    tmpgui.MouseOverCtrl := ReadInt32(FS);
    tmpgui.MouseWasAtX := ReadInt32(FS);
    tmpgui.MouseWasAtY := ReadInt32(FS);
    tmpgui.MouseDownCtrl := ReadInt32(FS);
    tmpgui.HighlightCtrl := ReadInt32(FS);
    tmpgui.Flags := ReadInt32(FS);
    tmpgui.Transparency := ReadInt32(FS);
    tmpgui.ZOrder := ReadInt32(FS);
    tmpgui.Id := ReadInt32(FS);
    tmpgui.Padding := ReadInt32(FS);
    FS.Seek(SizeOf(Integer)*GUIMAIN_RESERVED_INTS, soFromCurrent);
    tmpgui.Visibility := ReadInt32(FS);
    if gui_version < kGuiVersion_340 then
    begin
      FS.Seek(SizeOf(Integer)*LEGACY_MAX_OBJS_ON_GUI, soFromCurrent);
      FS.Seek(SizeOf(Integer)*LEGACY_MAX_OBJS_ON_GUI, soFromCurrent);
    end else begin
      if tmpgui.ControlCount > 0 then
        FS.Seek(SizeOf(Integer)*tmpgui.ControlCount, soFromCurrent);
    end;
  //  FS.Read(tmpgui, SizeOf(GUIMain));
  end;
  // import the buttons
  numguibuts := ReadInt32(FS);
  for i := 0 to numguibuts - 1 do
  begin
    GUIObjectRead(FS, gui_version);
    FS.Seek(4 * 12, soFromCurrent);
    SetLength(buf, 50);
    FS.Read(PAnsiChar(buf)^, 50);
    AddExtrString(AnsiString(buf));
    if gui_version >= 111 then
      FS.Seek(8, soFromCurrent);
    SetLength(buf, 0);
  end;
  // labels
  numguilabels := ReadInt32(FS);
  for i := 0 to numguilabels - 1 do
  begin
    GUIObjectRead(FS, gui_version);
    if gui_version < 113 then
    begin
      SetLength(buf, 200);
      FS.Read(PAnsiChar(buf)^, 200);
    end else begin
      buflen := ReadInt32(FS);
      SetLength(buf, buflen);
      FS.Read(PAnsiChar(buf)^, buflen);
    end;
    AddExtrString(AnsiString(buf));
    FS.Seek(4*3, soFromCurrent);
    SetLength(buf, 0);
  end;
  // inv controls
  numguiinv := ReadInt32(FS);
  for i := 0 to numguiinv - 1 do
  begin
    GUIObjectRead(FS, gui_version);
    if gui_version >= 109 then
    begin
      FS.Seek(16, soFromCurrent);
    end;
  end;
  // sliders
  if gui_version >= 100 then
  begin
    numguislider := ReadInt32(FS);
    for i := 0 to numguislider - 1 do
    begin
      GUIObjectRead(FS, gui_version);
      if gui_version >= 104 then
        FS.Seek(7*4, soFromCurrent)
      else
        FS.Seek(4*4, soFromCurrent);
    end;
  end;
  // text boxes
  if gui_version >= 101 then
  begin
    numguitext := ReadInt32(FS);
    for i := 0 to numguitext - 1 do
    begin
      GUIObjectRead(FS, gui_version);
      SetLength(buf, 200);
      FS.Read(PAnsiChar(buf)^, 200);
      FS.Seek(12, soFromCurrent);
      AddExtrString(AnsiString(buf));
      SetLength(buf, 0);
    end;
  end;
  // list boxes
  if gui_version >= 101 then
  begin
    numguilist := ReadInt32(FS);
    for i := 0 to numguilist - 1 do
    begin
      GUIObjectRead(FS, gui_version);
      FS.Read(tmplb, SizeOf(tmplb));
//      FS.Seek(40, soFromCurrent);
      if gui_version >= 112 then
        FS.Seek(8, soFromCurrent);
      if gui_version >= 107 then
        FS.Seek(4, soFromCurrent);
      for m := 0 to tmplb.numItems - 1 do
      begin
        SetLength(buf, 1000);
        PAnsiString(buf)^ := fgetstring(FS);
        AddExtrString(AnsiString(buf));
        SetLength(buf, 0);
      end;
      if (gui_version >= 114) and ((tmplb.exflags and GLF_SGINDEXVALID) = GLF_SGINDEXVALID) then
        FS.Seek(2*tmplb.numItems, soFromCurrent);
    end;
  end;
  SetLength(buf, 0);
end;

procedure TGameDta.CustomPropertySchema(FS: TFileStream);
var
  numProps, i: Integer;
  buf: AnsiString;
begin
  if ReadInt32(FS) <> 1 then
    Exit;
  numProps := ReadInt32(FS);
  for i := 0 to numProps - 1 do
  begin
    buf := fgetstring_limit(FS, 20);
    buf := fgetstring_limit(FS, 100);
    buf := fgetstring_limit(FS, MAX_CUSTOM_PROPERTY_VALUE_LENGTH);
    AddExtrString(buf);
    ReadInt32(FS);
  end;
end;

procedure TGameDta.CustomProperties(FS: TFileStream);
var
  numProps, i: Integer;
  buf: AnsiString;
begin
  if ReadInt32(FS) <> 1 then
    Exit;
  numProps := ReadInt32(FS);
  for i := 0 to numProps - 1 do
  begin
    buf := fgetstring_limit(FS, 200);
    buf := fgetstring_limit(FS, MAX_CUSTOM_PROPERTY_VALUE_LENGTH);
    AddExtrString(buf);
  end;
end;

procedure TGameDta.ReadPluginsFromDisk(FS: TFileStream);
var
  numPlug, datasize, i: Integer;
begin
  if ReadInt32(FS) <> 1 then
    Exit;
  numPlug := ReadInt32(FS);
  for i := 0 to numPlug - 1 do
  begin
    fgetstring(FS);
    datasize := ReadInt32(FS);
    FS.Seek(datasize, soFromCurrent);
  end;
end;

procedure TGameDta.read_dialogs(FS: TFileStream);
begin

end;

procedure TGameDta.ReadFromStream(FS: TFileStream);
var
  buf: array [0..39] of AnsiChar;
  bufstr: array of AnsiChar;
  stlen, numScriptModules, numGlobalVars, i, m, n, newlen: Integer;
  numToRead, numToRead2: UINT;
  news, buf2: AnsiString;
begin
  FS.Read(buf, 30);
  buf[30] := #0;
  if buf <> game_file_sig then
  begin
    DtaErr := DTA_ERROR_INC_FILE;
    Exit;
  end;
  DtaVer := ReadInt32(FS);
  if (DtaVer > kGameVersion_Current) or (DtaVer < 0) then
  begin
    DtaErr := DTA_ERROR_INC_VER;
    Exit;
  end;
  stlen := ReadInt32(FS);
  SetLength(DtaVerStr, stlen);
  FS.ReadBuffer(PAnsiString(DtaVerStr)^, stlen);
//  FS.Seek(stlen, soFromCurrent);
  ReadGameSetupStructBase(FS);
  if DtaVer >= kGameVersion_272 then
  begin
    FS.Read(GSS.GUID, MAX_GUID_LENGTH);
    FS.Read(GSS.saveGameFileExtension, MAX_SG_EXT_LENGTH);
    FS.Read(GSS.saveGameFolderName, MAX_SG_FOLDER_LEN);
  end;
  FS.ReadBuffer(GSS.FontFlags, GSS.Base.NumFonts);
  FS.ReadBuffer(GSS.FontOutline, GSS.Base.NumFonts);
  if DtaVer < kGameVersion_256 then
    numToRead := 6000
  else
    numToRead := ReadInt32(FS);
  FS.Seek(numToRead, soFromCurrent); // SpriteFlags
  ReadInventoryItemInfo(FS);
  for i := 0 to GSS.Base.NumInvItems - 1 do
    AddExtrString(GSS.InvInfo[i].Name);
  ReadMouseCursor(FS);
  SetLength(GSS.CharScripts, GSS.Base.NumCharacters);
  SetLength(GSS.InvScripts, GSS.Base.NumInvItems);
  if DtaVer > kGameVersion_272 then
  begin
    for i := 0 to GSS.Base.NumCharacters-1 do
      GSS.CharScripts[i] := deserialize_interaction_scripts(FS);
    for i := 0 to GSS.Base.NumInvItems-2 do
      GSS.InvScripts[i] := deserialize_interaction_scripts(FS);
  end else begin
    numGlobalVars := FS.Position;
    for i := 0 to GSS.Base.NumCharacters-1 do
      deserialize_new_interaction(FS);
    for i := 0 to GSS.Base.NumInvItems-1 do
      deserialize_new_interaction(FS);
    numGlobalVars := FS.Position;
    numGlobalVars := ReadInt32(FS);
    FS.Seek(SizeOf(InteractionVariable)*numGlobalVars, soFromCurrent); // MCurs
  end;
  if GSS.Base.Dict > 0 then
    GSS.Dict := read_dictionary(FS);
  // Loading scripts
  fread_script(FS); // Global Script
  if DtaVer > kGameVersion_310 then
    fread_script(FS); // Dialog Script
  if DtaVer >= kGameVersion_270 then
  begin
    numScriptModules := ReadInt32(FS);
    for i := 0 to numScriptModules - 1 do
      fread_script(FS);
  end;
//  SetLength(GSS.views, GSS.Base.NumViews);
  if DtaVer > kGameVersion_272 then
  begin
    for i := 0 to GSS.Base.NumViews - 1 do
    begin
      FS.Read(numToRead, 2);
      for m := 0 to numToRead - 1 do
      begin
        FS.Read(numToRead2, 2);
        FS.Seek(4, soFromCurrent);
        FS.Seek(SizeOf(ViewFrame)*numToRead2, soFromCurrent);
      end;
    end;
  end else begin
    FS.Seek(SizeOf(ViewStruct272)*GSS.Base.NumViews, soFromCurrent);
  end;
  // <= 2.1 skip unknown data
  if DtaVer <= kGameVersion_251 then
    FS.Seek(ReadInt32(FS) * $204, soFromCurrent);

  SetLength(GSS.chars, GSS.Base.NumCharacters);
  for i := 0 to GSS.Base.NumCharacters - 1 do
  begin
    FS.Read(GSS.chars[i], SizeOf(CharacterInfo));
    AddExtrString(GSS.chars[i].name);
  end;
  FS.Seek(20 * 50, soFromCurrent); // lipSyncFrameLetters
  SetLength(GSS.messages, MAXGLOBALMES);
  for i := 0 to MAXGLOBALMES - 1 do
  begin
    GSS.messages[i] := read_string_decrypt(FS);
  end;
  // Reading dialogs;
  SetLength(GSS.dialog, GSS.Base.NumDialog);
  numScriptModules := FS.Position;
  for i := 0 to GSS.Base.NumDialog - 1 do
  begin
    FS.Read(GSS.dialog[i], SizeOf(DialogTopic));
    numScriptModules := FS.Position;
    for m := 0 to GSS.dialog[i].numoptions - 1 do
      AddExtrString(GSS.dialog[i].optionnames[m]);
  end;
  numScriptModules := FS.Position;
  if DtaVer < kGameVersion_272 then
  begin
      for i := 0 to GSS.Base.NumDialog - 1 do
      begin
        if GSS.dialog[i].optionscripts <> 0 then
        begin
          SetLength(bufstr, GSS.dialog[i].codesize + 10);
          FS.Read(PChar(bufstr)^, GSS.dialog[i].codesize);
          SetLength(bufstr, 0);
        end;
        NumToRead := ReadInt32(FS);
        if NumToRead <= 1 then
        FS.Seek(1, soFromCurrent);
        SetLength(bufstr, NumToRead + 1);
        FS.Read(PChar(bufstr)^, NumToRead);
  //      AddExtrString(decrypt_text(AnsiString(bufstr)));
        SetLength(bufstr, 0);
      end;
//      for i := 0 to GSS.Base.NumDlgMessage - 1 do
//        if (DtaVer >= 26) then
//          AddExtrString(read_string_decrypt(FS));
    if DtaVer < kGameVersion_260 then
    begin
    end else begin
      while True do
      begin
        numToRead := FS.Position;
        NumToRead := ReadInt32(FS);
        if NumToRead = $CAFEBEEF then
        begin
          FS.Seek(-4, soFromCurrent);
          Break;
        end;
        SetLength(bufstr, NumToRead + 1);
        FS.Read(PChar(bufstr)^, NumToRead);
        Inc(i);
      end;
    end;
  end;
  read_gui(FS);
  i := FS.Position;
  ReadPluginsFromDisk(FS);
  CustomPropertySchema(FS);
  for i := 0 to GSS.Base.NumCharacters - 1 do
    CustomProperties(FS);
  for i := 0 to GSS.Base.NumInvItems - 1 do
    CustomProperties(FS);
//  for i := 0 to GSS.Base.NumViews - 1 do
//  begin
//    news := fgetstring_limit(FS, MAXVIEWNAMELENGTH);
//    AddExtrString(news);
//  end;
end;

procedure TGameDta.ReadGameSetupStructBase(FS: TFileStream);
var
  i: Integer;
  curpos, curr: UINT;
begin
//  FS.ReadBuffer(GSS.Base.GameName, Length(GSS.Base.GameName)*SizeOf(GSS.Base.GameName[0]));
  FS.ReadBuffer(GSS.Base.GameName, 52);
  for i := 0 to 99 do
    GSS.Base.Options[i] := ReadInt32(FS);
  FS.ReadBuffer(GSS.Base.PalUses, Length(GSS.Base.PalUses)*SizeOf(GSS.Base.PalUses[i]));
  for i := 0 to 255 do
    GSS.Base.DefPal[i] := ReadInt32(FS);
  GSS.Base.NumViews := ReadInt32(FS);
  GSS.Base.NumCharacters := ReadInt32(FS);
  GSS.Base.PlayerCharacter := ReadInt32(FS);
  GSS.Base.TotalScore := ReadInt32(FS);
  GSS.Base.NumInvItems := ReadInt32(FS);
//  GSS.Base.NumInvItems := ReadInt16(FS);
  GSS.Base.NumDialog := ReadInt32(FS);
  GSS.Base.NumDlgMessage := ReadInt32(FS);
  GSS.Base.NumFonts := ReadInt32(FS);
  GSS.Base.Color_Depth := ReadInt32(FS);
  GSS.Base.Target_Win := ReadInt32(FS);
  GSS.Base.Dialog_Bullet := ReadInt32(FS);
//  GSS.Base.HotDot := ReadInt32(FS);
//  GSS.Base.HotDotOuter := ReadInt32(FS);
  GSS.Base.HotDot := ReadInt16(FS);
  GSS.Base.HotDotOuter := ReadInt16(FS);
  GSS.Base.UniqueID := ReadInt32(FS);
  GSS.Base.NumGUI := ReadInt32(FS);
  GSS.Base.NumCursors := ReadInt32(FS);
  GSS.Base.Default_Resolution := ReadInt32(FS);
  if (GSS.Base.Default_Resolution = kGameResolution_Custom) and (DtaVer >= kGameVersion_340_1) then
  begin
    ReadInt32(FS);
    ReadInt32(FS);
  end;
  GSS.Base.Default_Lipsync_Frame := ReadInt32(FS);
  GSS.Base.InvHotDotSprite := ReadInt32(FS);
  for I := 0 to 16 do
    GSS.Base.Reserved[i] := ReadInt32(FS);
  for I := 0 to MAXGLOBALMES-1 do
    GSS.Base.Messages[i] := ReadInt32(FS);
  GSS.Base.Dict := ReadInt32(FS);
  GSS.Base.GlobalScript := ReadInt32(FS);
  GSS.Base.Chars := ReadInt32(FS);
  GSS.Base.Compiled_Script := ReadInt32(FS);
end;

procedure TGameDta.ReadInventoryItemInfo(FS: TFileStream);
var
  i, m: Integer;
begin
  for I := 0 to GSS.Base.NumInvItems - 1 do
  begin
    FS.ReadBuffer(GSS.InvInfo[i].Name, 28);
    GSS.InvInfo[i].Pic := ReadInt32(FS);
    GSS.InvInfo[i].CursorPic := ReadInt32(FS);
    GSS.InvInfo[i].HotX := ReadInt32(FS);
    GSS.InvInfo[i].HotY := ReadInt32(FS);
    for m := 0 to 4 do
      GSS.InvInfo[i].Reserved[m] := ReadInt32(FS);
    GSS.InvInfo[i].Flags := ReadInt8(FS);
  end;
end;

procedure TGameDta.ReadMouseCursor(FS: TFileStream);
var
  i: Integer;
begin
  for I := 0 to GSS.Base.NumCursors - 1 do
  begin
    GSS.MCurs[i].Pic := ReadInt32(FS);
    GSS.MCurs[i].HotX := ReadInt16(FS);
    GSS.MCurs[i].HotY := ReadInt16(FS);
    GSS.MCurs[i].View := ReadInt16(FS);
    FS.ReadBuffer(GSS.MCurs[i].Name, 10);
    GSS.MCurs[i].Flags := ReadInt8(FS);
  end;
end;

procedure TGameDta.ReadFromStream2(FS: TFileStream);
var
  buf: array [0..39] of AnsiChar;
  stlen, numToRead, numScriptModules, i, m, n: Integer;
begin
  FS.Read(buf, 30);
  buf[30] := #0;
  if buf <> game_file_sig then
  begin
    Exit;
  end;
  DtaVer := ReadInt32(FS);
  if DtaVer <> kGameVersion_321 then
  begin
    Exit;
  end;
  stlen := ReadInt32(FS);
  FS.Seek(stlen, soFromCurrent);
  FS.Read(GSS.Base, SizeOf(GameSetupStructBase));
  FS.Read(GSS.GUID, MAX_GUID_LENGTH);
  FS.Read(GSS.saveGameFileExtension, MAX_SG_EXT_LENGTH);
  FS.Read(GSS.saveGameFolderName, MAX_SG_FOLDER_LEN);
  FS.Read(GSS.FontFlags, GSS.Base.NumFonts);
  FS.Read(GSS.FontOutline, GSS.Base.NumFonts);
  numToRead := ReadInt32(FS);
  FS.Read(GSS.SpriteFlags, numToRead);
  FS.Read(GSS.InvInfo, SizeOf(InventoryItemInfo)*GSS.Base.NumInvItems);
  FS.Read(GSS.MCurs, SizeOf(MouseCursor)*GSS.Base.NumCursors);
  SetLength(GSS.CharScripts, GSS.Base.NumCharacters);
  SetLength(GSS.InvScripts, GSS.Base.NumInvItems);
  for i := 0 to GSS.Base.NumCharacters-1 do
    GSS.CharScripts[i] := deserialize_interaction_scripts(FS);
  for i := 0 to GSS.Base.NumInvItems-2 do
    GSS.InvScripts[i] := deserialize_interaction_scripts(FS);
  if GSS.Base.Dict > 0 then
    GSS.Dict := read_dictionary(FS);
  fread_script(FS); // Global Script
  fread_script(FS); // Dialog Script
  numScriptModules := ReadInt32(FS);
  for i := 0 to numScriptModules - 1 do
    fread_script(FS);
  SetLength(GSS.views, GSS.Base.NumViews);
  for i := 0 to GSS.Base.NumViews - 1 do
  begin
    FS.Read(GSS.views[i].numLoops, 2);
    SetLength(GSS.views[i].loops, GSS.views[i].numLoops);
    for m := 0 to GSS.views[i].numLoops - 1 do
    begin
      FS.Read(GSS.views[i].loops[m].numFrames, 2);
      GSS.views[i].loops[m].flags := ReadInt32(FS);
      SetLength(GSS.views[i].loops[m].frames, GSS.views[i].loops[m].numFrames);
      for n := 0 to GSS.views[i].loops[m].numFrames - 1 do
        FS.Read(GSS.views[i].loops[m].frames[n], SizeOf(ViewFrame))
    end;
  end;
  SetLength(GSS.chars, GSS.Base.NumCharacters);
  for i := 0 to GSS.Base.NumCharacters - 1 do
  begin
    FS.Read(GSS.chars[i], SizeOf(CharacterInfo));
    AddExtrString(GSS.chars[i].name);
  end;
  FS.Read(GSS.lipSyncFrameLetters, 20 * 50);
  SetLength(GSS.messages, MAXGLOBALMES);
  for i := 0 to MAXGLOBALMES - 1 do
  begin
    GSS.messages[i] := read_string_decrypt(FS);
  end;
  SetLength(GSS.dialog, GSS.Base.NumDialog);
  for i := 0 to GSS.Base.NumDialog - 1 do
  begin
    FS.Read(GSS.dialog[i], SizeOf(DialogTopic));
    for m := 0 to GSS.dialog[i].numoptions - 1 do
      AddExtrString(GSS.dialog[i].optionnames[m]);
  end;
end;


procedure TGameDta.ReadFromFile(FileName: AnsiString);
begin
  DtaF := TFileStream.Create(FileName, fmOpenRead);
  ReadFromStream(DtaF);
  DtaF.Free;
end;

function TGameDta.GetGameName: AnsiString;
begin
  Result := GSS.Base.GameName;
end;

function TGameDta.GetGameID: uInt;
begin
  Result := GSS.Base.UniqueID;
end;

function TGameDta.GetDtaErr: UINT;
begin
  Result := DtaErr;
end;

function TGameDta.GetDtaVersion: UINT;
begin
  Result := DtaVer;
end;

function TGameDta.GetDtaVersionString: AnsiString;
begin
  Result := DtaVerStr;
end;

end.