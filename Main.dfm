object MainForm: TMainForm
  Left = 0
  Top = 0
  BorderIcons = [biSystemMenu]
  BorderStyle = bsSingle
  Caption = 'AGS Tool'
  ClientHeight = 394
  ClientWidth = 313
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnClose = FormClose
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 167
    Top = 133
    Width = 115
    Height = 13
    Caption = 'Version 1.8 by SileNTViP'
  end
  object GroupBox1: TGroupBox
    Left = 159
    Top = 8
    Width = 145
    Height = 87
    Caption = 'For Notabenoid'
    TabOrder = 2
    object ImportTXT: TButton
      Left = 8
      Top = 51
      Width = 128
      Height = 25
      Caption = 'Create TRS from Inbox'
      TabOrder = 0
      OnClick = ImportTXTClick
    end
    object SaveTXT: TButton
      Left = 8
      Top = 20
      Width = 128
      Height = 25
      Caption = 'Extract TXT from TRS'
      TabOrder = 1
      OnClick = SaveTXTClick
    end
  end
  object GroupBox2: TGroupBox
    Left = 159
    Top = 152
    Width = 145
    Height = 121
    Caption = 'Work with game files'
    TabOrder = 3
    object ExtrLIB: TButton
      Left = 8
      Top = 16
      Width = 128
      Height = 25
      Caption = 'Extract from game lib'
      TabOrder = 0
      OnClick = ExtrLIBClick
    end
    object ExtrEXE: TButton
      Left = 8
      Top = 47
      Width = 128
      Height = 25
      Caption = 'Extract from EXE'
      TabOrder = 1
      OnClick = ExtrEXEClick
    end
    object MakeEXE: TButton
      Left = 8
      Top = 78
      Width = 128
      Height = 25
      Caption = 'Build EXE'
      TabOrder = 2
      Visible = False
      OnClick = MakeEXEClick
    end
  end
  object Button1: TButton
    Left = 167
    Top = 399
    Width = 19
    Height = 22
    Caption = 'Button1'
    TabOrder = 1
    Visible = False
    OnClick = Button1Click
  end
  object GroupBox3: TGroupBox
    Left = 8
    Top = 8
    Width = 145
    Height = 119
    Caption = 'Create TRS from game'
    TabOrder = 0
    object ExtractEXE: TButton
      Left = 7
      Top = 20
      Width = 129
      Height = 25
      Caption = 'Create TXT from game'
      TabOrder = 0
      OnClick = ExtractEXEClick
    end
    object CreateTXT2TRS: TButton
      Left = 8
      Top = 82
      Width = 128
      Height = 25
      Caption = 'Create TRS from TXT'
      TabOrder = 1
      OnClick = CreateTXT2TRSClick
    end
    object CreateFromDTA: TButton
      Left = 8
      Top = 51
      Width = 128
      Height = 25
      Caption = 'TXT from DTA&&CRM'
      TabOrder = 2
      OnClick = CreateFromDTAClick
    end
  end
  object GroupBox4: TGroupBox
    Left = 8
    Top = 133
    Width = 145
    Height = 142
    Caption = 'Work with translations'
    TabOrder = 4
    object DecompileTRA: TButton
      Left = 8
      Top = 17
      Width = 128
      Height = 25
      Caption = 'Decompile TRA'
      TabOrder = 0
      OnClick = DecompileTRAClick
    end
    object CompileTRS: TButton
      Left = 8
      Top = 48
      Width = 128
      Height = 25
      Caption = 'Compile TRS'
      TabOrder = 1
      OnClick = CompileTRSClick
    end
    object TRSAdder: TButton
      Left = 8
      Top = 79
      Width = 128
      Height = 25
      Caption = 'TRS Adder diff'
      TabOrder = 2
      OnClick = TRSAdderClick
    end
    object CreateFrom2TXT: TButton
      Left = 8
      Top = 110
      Width = 128
      Height = 25
      Caption = 'Create TRS from 2'#1093' TXT'
      TabOrder = 3
      OnClick = CreateFrom2TXTClick
    end
  end
  object GroupBox5: TGroupBox
    Left = 8
    Top = 279
    Width = 295
    Height = 50
    Caption = 'WFN fonts'
    TabOrder = 5
    object wfn2bdf: TButton
      Left = 8
      Top = 16
      Width = 128
      Height = 25
      Caption = 'WFN to BDF'
      TabOrder = 0
      OnClick = wfn2bdfClick
    end
    object bdf2wfn: TButton
      Left = 159
      Top = 16
      Width = 128
      Height = 25
      Caption = 'BDF to ext. WFN'
      TabOrder = 1
      OnClick = bdf2wfnClick
    end
  end
  object GroupBox6: TGroupBox
    Left = 8
    Top = 335
    Width = 295
    Height = 50
    Caption = 'Sprites'
    TabOrder = 6
    object ExtrSpr: TButton
      Left = 8
      Top = 14
      Width = 128
      Height = 25
      Caption = 'Decompile sprites'
      TabOrder = 0
      OnClick = ExtrSprClick
    end
    object GenSpr: TButton
      Left = 159
      Top = 16
      Width = 128
      Height = 25
      Caption = 'Compile sprites'
      TabOrder = 1
      OnClick = GenSprClick
    end
  end
  object RoomBtn: TButton
    Left = 177
    Top = 427
    Width = 128
    Height = 25
    Caption = #1050#1086#1084#1085#1072#1090#1099
    Enabled = False
    TabOrder = 7
    OnClick = RoomBtnClick
  end
  object btn1: TButton
    Left = 15
    Top = 427
    Width = 75
    Height = 25
    Caption = 'btn1'
    Enabled = False
    TabOrder = 8
    OnClick = btn1Click
  end
  object btn2: TButton
    Left = 96
    Top = 427
    Width = 75
    Height = 25
    Caption = 'btn2'
    Enabled = False
    TabOrder = 9
    OnClick = btn2Click
  end
  object chkTRAV2: TCheckBox
    Left = 167
    Top = 101
    Width = 115
    Height = 17
    Caption = 'Compile TRA for V2'
    TabOrder = 10
  end
  object odGame: TOpenDialog
    DefaultExt = '*.exe;ac2game.dat'
    Filter = 'AGS Game|*.exe;ac2game.dat'
    Left = 264
    Top = 200
  end
  object odTRS: TOpenDialog
    DefaultExt = '*.trs'
    Filter = 'Translation TRS|*.trs'
    Left = 72
    Top = 144
  end
  object odTRA: TOpenDialog
    DefaultExt = '*.tra'
    Filter = 'Translation TRA|*.tra'
    Left = 72
    Top = 192
  end
  object sdTRS: TSaveDialog
    DefaultExt = '*.trs'
    Filter = 'AGS Translation TRS|*.trs'
    Left = 112
    Top = 144
  end
  object odLIB: TOpenDialog
    DefaultExt = '*.vox;ac2game.dat'
    Filter = 'All Supported Files|*.vox;ac2game.dat'
    Left = 264
    Top = 160
  end
  object odTXT: TOpenDialog
    DefaultExt = '*.txt'
    Filter = 'Translation Text|*.txt'
    Left = 120
  end
  object odWFN: TOpenDialog
    DefaultExt = '*.wfn'
    Filter = 'AGS WFN Fonts|agsfnt*.wfn'
    Left = 136
    Top = 288
  end
  object odBDF: TOpenDialog
    DefaultExt = '*.bdf'
    Filter = 'Bitmap Font File|*.bdf'
    Left = 264
    Top = 288
  end
  object sdEXE: TSaveDialog
    DefaultExt = '*.exe'
    Filter = 'AGS Game Executable|*.exe'
    Left = 264
    Top = 240
  end
  object odSPR: TOpenDialog
    DefaultExt = 'acsprset.spr'
    Filter = 'AGS Sprites|acsprset.spr'
    Left = 136
    Top = 336
  end
  object sdSPR: TSaveDialog
    DefaultExt = 'acsprset.spr'
    FileName = 'acsprset.spr'
    Filter = 'AGS Sprites|acsprset.spr'
    Left = 264
    Top = 344
  end
  object odDTACRM: TOpenDialog
    DefaultExt = '*.crm;*.dta'
    Filter = 'Game Main File & Rooms|*.crm;*.dta'
    Title = 'Choose Main Game File DTA or Room file CRM'
    Left = 119
    Top = 48
  end
  object sdDTACRM: TSaveDialog
    DefaultExt = '*.txt'
    Filter = 'Text Files|*.txt'
    Title = 'Save Extracted text to file'
    Left = 63
    Top = 48
  end
end
