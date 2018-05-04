object RoomForm: TRoomForm
  Left = 0
  Top = 0
  BorderIcons = [biSystemMenu, biMinimize]
  BorderStyle = bsSingle
  Caption = #1050#1086#1084#1085#1072#1090#1099
  ClientHeight = 322
  ClientWidth = 571
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poMainFormCenter
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 8
    Top = 8
    Width = 72
    Height = 13
    Caption = #1058#1077#1082#1091#1097#1080#1081' '#1092#1086#1085':'
  end
  object BackImg: TImage
    Left = 8
    Top = 51
    Width = 555
    Height = 240
    AutoSize = True
    Enabled = False
  end
  object Backcb: TComboBox
    Left = 8
    Top = 24
    Width = 145
    Height = 21
    Enabled = False
    TabOrder = 0
  end
  object ORbtn: TButton
    Left = 168
    Top = 20
    Width = 75
    Height = 25
    Caption = #1054#1090#1082#1088#1099#1090#1100'...'
    TabOrder = 1
    OnClick = ORbtnClick
  end
  object ScrollBar1: TScrollBar
    Left = 8
    Top = 297
    Width = 555
    Height = 17
    Enabled = False
    PageSize = 0
    TabOrder = 2
  end
  object odCRM: TOpenDialog
    DefaultExt = '*.crm'
    Filter = 'AGS Rooms|*.crm'
    Left = 472
    Top = 72
  end
end
