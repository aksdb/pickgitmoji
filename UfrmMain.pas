unit UfrmMain;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls,
  Menus, JSONPropStorage, IDEWindowIntf, Types, fgl;

type

  TGitmoji = class(TCollectionItem)
  private
    FName: UTF8String;
    FDescription: UTF8String;
    FCode: UTF8String;
    FEmoji: UTF8String;
  published
    property name: UTF8String read FName write FName;
    property description: UTF8String read FDescription write FDescription;
    property code: UTF8String read FCode write FCode;
    property emoji: UTF8String read FEmoji write FEmoji;
  end;

  { TGitmojiData }

  TGitmojiData = class(TPersistent)
  private
    FGitmojis: TCollection;
  published
    property gitmojis: TCollection read FGitmojis;
  public
    constructor Create;
    destructor Destroy; override;
  end;

  TTagData = array of record
    Tags: array of string;
    Gitmoji: TGitmoji;
  end;

  { TfrmMain }

  TfrmMain = class(TForm)
    ApplicationProperties1: TApplicationProperties;
    cbCopyEmoji: TCheckBox;
    edSearch: TEdit;
    JSONPropStorage1: TJSONPropStorage;
    lbGitmojis: TListBox;
    mnuShow: TMenuItem;
    mnuExit: TMenuItem;
    PopupMenu1: TPopupMenu;
    TrayIcon1: TTrayIcon;
    procedure ApplicationProperties1Deactivate(Sender: TObject);
    procedure edSearchChange(Sender: TObject);
    procedure edSearchKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState
      );
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormShow(Sender: TObject);
    procedure JSONPropStorage1RestoreProperties(Sender: TObject);
    procedure JSONPropStorage1SavingProperties(Sender: TObject);
    procedure lbGitmojisDblClick(Sender: TObject);
    procedure lbGitmojisDrawItem(Control: TWinControl; Index: Integer;
      ARect: TRect; State: TOwnerDrawState);
    procedure lbGitmojisKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure mnuExitClick(Sender: TObject);
    procedure mnuShowClick(Sender: TObject);
    procedure TrayIcon1Click(Sender: TObject);
  private
    FGitmojiData: TGitmojiData;
    FTagData: TTagData;

    FIconSize: Integer;
    FBigSize: Integer;
    FSmallSize: Integer;

    function DetermineFontSize(ACanvas: TCanvas; AHeight: Integer): Integer;
  public

  end;

var
  frmMain: TfrmMain;

implementation

{$R *.lfm}

uses
  fpjson, jsonparser, jsonscanner, fpjsonrtti, LCLType, Clipbrd;

{ TGitmojiData }

constructor TGitmojiData.Create;
begin
  inherited Create;
  FGitmojis := TCollection.Create(TGitmoji);
end;

destructor TGitmojiData.Destroy;
begin
  FreeAndNil(FGitmojis);
  inherited Destroy;
end;

{ TfrmMain }

procedure TfrmMain.FormCreate(Sender: TObject);
var
  fs: TResourceStream;
  parser: TJSONParser;
  jsondata: TJSONData;
  destreamer: TJSONDeStreamer;
  gitmoji: TGitmoji;
  i, j, off: Integer;
  tags1, tags2: array of String;
  configDir: String;
begin
  JSONPropStorage1.JSONFileName := GetAppConfigFile(False);
  configDir := ExtractFilePath(JSONPropStorage1.JSONFileName);
  if not DirectoryExists(configDir) then
  begin
    ForceDirectories(configDir);
  end;

  try
    fs := TResourceStream.Create(HINSTANCE, 'GITMOJIS', RT_RCDATA);
    parser := TJSONParser.Create(fs, DefaultOptions);
    jsondata := parser.Parse;
    destreamer := TJSONDeStreamer.Create(nil);
    FGitmojiData := TGitmojiData.Create;
    destreamer.JSONToObject(jsondata as TJSONObject, FGitmojiData);
  finally
    fs.Free;
    parser.Free;
    jsondata.Free;
    destreamer.Free;
  end;

  SetLength(FTagData, FGitmojiData.FGitmojis.Count);
  for i := 0 to FGitmojiData.FGitmojis.Count - 1 do
  begin
    gitmoji := TGitmoji(FGitmojiData.FGitmojis.Items[i]);
    FTagData[i].Gitmoji := gitmoji;
    tags1 := Trim(gitmoji.FName).Split(' ');
    tags2 := Trim(gitmoji.FDescription).Split(' ');
    SetLength(FTagData[i].Tags, Length(tags1) + Length(tags2));
    off := 0;
    for j := 0 to Length(tags1) - 1 do
      FTagData[i].Tags[off + j] := tags1[j].ToLower;
    off := Length(tags1);
    for j := 0 to Length(tags2) - 1 do
      FTagData[i].Tags[off + j] := tags2[j].ToLower;
  end;

  FIconSize := DetermineFontSize(lbGitmojis.Canvas, lbGitmojis.ItemHeight);
  FBigSize := DetermineFontSize(lbGitmojis.Canvas, lbGitmojis.ItemHeight div 2);
  FSmallSize := DetermineFontSize(lbGitmojis.Canvas, lbGitmojis.ItemHeight div 3);

  {$IFDEF WithTrayIcon}
  TrayIcon1.Visible := True;
  {$ENDIF WithTrayIcon}
end;

type
  TMatch = class
    Count: Integer;
    Gitmoji: TGitmoji;
  end;

function CompareMatches(const AMatch1, AMatch2: TMatch): Integer;
begin
  Result := AMatch1.Count - AMatch2.Count;
  if Result = 0 then
    Result := CompareStr(AMatch1.Gitmoji.FName, AMatch2.Gitmoji.FName);
end;

type
  TMatches = specialize TFPGObjectList<TMatch>;

procedure TfrmMain.edSearchChange(Sender: TObject);
var
  s: String;
  token: array of string;
  i, j, k, matchCount: Integer;
  matches: TMatches;
  match: TMatch;
begin
  s := Trim(AnsiLowerCase(edSearch.Text));
  token := s.Split(' ');
  lbGitmojis.Items.BeginUpdate;
  lbGitmojis.Items.Clear;
  if s = '' then
  begin
    for i := 0 to Length(FTagData) - 1 do
      lbGitmojis.Items.AddObject('', FTagData[i].Gitmoji);
  end else
  begin
    matches := TMatches.Create;
    for i := 0 to Length(FTagData) - 1 do
    begin
      matchCount := 0;
      for j := 0 to Length(FTagData[i].Tags) - 1 do
        for k := 0 to Length(token) - 1 do
          if Pos(token[k], FTagData[i].Tags[j]) > 0 then
            Inc(matchCount);

      if matchCount > 0 then
      begin
        match := TMatch.Create;
        match.Count := matchCount;
        match.Gitmoji := FTagData[i].Gitmoji;
        matches.Add(match);
      end;
    end;

    matches.Sort(@CompareMatches);
    for match in matches do
      lbGitmojis.Items.AddObject('', match.Gitmoji);
    matches.Free;
  end;
  lbGitmojis.Items.EndUpdate;
end;

procedure TfrmMain.ApplicationProperties1Deactivate(Sender: TObject);
begin
  Close;
end;

procedure TfrmMain.edSearchKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Key = VK_DOWN then
  begin
    lbGitmojis.SetFocus;
    if lbGitmojis.Items.Count > 0 then
      lbGitmojis.ItemIndex := 0;
    Key := 0;
  end;
end;

procedure TfrmMain.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  {$IFDEF WithTrayIcon}
  CloseAction := caHide;
  {$ENDIF}
end;

procedure TfrmMain.FormDestroy(Sender: TObject);
begin
  FreeAndNil(FGitmojiData);
end;

procedure TfrmMain.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Key = VK_ESCAPE then
  begin
    Key := 0;
    Close;
  end;
end;

procedure TfrmMain.FormShow(Sender: TObject);
begin
  Left := Mouse.CursorPos.X - (Width div 2);
  Top := Mouse.CursorPos.Y - (Height div 2);

  if Left + Width > Screen.DesktopLeft + Screen.DesktopWidth then
    Left := Screen.DesktopLeft + Screen.DesktopWidth - Width
  else if Left < Screen.DesktopLeft then
    Left := Screen.DesktopLeft;

  if Top + Height > Screen.DesktopTop + Screen.DesktopHeight then
    Top := Screen.DesktopTop + Screen.DesktopHeight - Height
  else if Top < Screen.DesktopTop then
    Top := Screen.DesktopTop;

  edSearch.Text := '';
  edSearchChange(Sender);

  BringToFront;
  Activate;
end;

procedure TfrmMain.JSONPropStorage1RestoreProperties(Sender: TObject);
begin
  Width := StrToInt(JSONPropStorage1.StoredValue['Width']);
  Height := StrToInt(JSONPropStorage1.StoredValue['Height']);
  cbCopyEmoji.Checked := StrToBool(JSONPropStorage1.StoredValue['CopyEmoji']);
end;

procedure TfrmMain.JSONPropStorage1SavingProperties(Sender: TObject);
begin
  JSONPropStorage1.StoredValue['Width'] := IntToStr(Width);
  JSONPropStorage1.StoredValue['Height'] := IntToStr(Height);
  JSONPropStorage1.StoredValue['CopyEmoji'] := BoolToStr(cbCopyEmoji.Checked, True);
end;

procedure TfrmMain.lbGitmojisDblClick(Sender: TObject);
begin
  if cbCopyEmoji.Checked then
    Clipboard.AsText := TGitmoji(lbGitmojis.Items.Objects[lbGitmojis.ItemIndex]).FEmoji
  else
    Clipboard.AsText := TGitmoji(lbGitmojis.Items.Objects[lbGitmojis.ItemIndex]).FCode;
  Close;
end;

procedure TfrmMain.lbGitmojisDrawItem(Control: TWinControl; Index: Integer;
  ARect: TRect; State: TOwnerDrawState);
var
  gitmoji: TGitmoji;
  textStyle: TTextStyle;
begin
  if Index > -1 then
  begin
    gitmoji := TGitmoji(lbGitmojis.Items.Objects[Index]);

    textStyle := lbGitmojis.Canvas.TextStyle;
    textStyle.Opaque := True;

    lbGitmojis.Canvas.Font.Size := FIconSize;
    lbGitmojis.Canvas.Font.Style := [];
    lbGitmojis.Canvas.TextRect(ARect, ARect.Left, arect.top, gitmoji.FEmoji, textStyle);

    lbGitmojis.Canvas.Font.Size := FBigSize;
    lbGitmojis.Canvas.Font.Style := [fsBold];
    lbGitmojis.Canvas.TextRect(ARect, ARect.Left + lbGitmojis.ItemHeight + 8, ARect.Top, gitmoji.code, textStyle);

    lbGitmojis.Canvas.Font.Size := FSmallSize;
    lbGitmojis.Canvas.Font.Style := [];
    lbGitmojis.Canvas.TextRect(ARect, ARect.Left + lbGitmojis.ItemHeight + 8, ARect.Top + lbGitmojis.ItemHeight div 2, gitmoji.FDescription, textStyle);
  end;
end;

procedure TfrmMain.lbGitmojisKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if (Key = VK_UP) and (lbGitmojis.ItemIndex = 0) then
    edSearch.SetFocus
  else if (Key = VK_RETURN) and (lbGitmojis.ItemIndex > -1) then
  begin
    lbGitmojisDblClick(Sender);
  end;
end;

procedure TfrmMain.mnuExitClick(Sender: TObject);
begin
  Application.Terminate;
end;

procedure TfrmMain.mnuShowClick(Sender: TObject);
begin
  Show;
end;

procedure TfrmMain.TrayIcon1Click(Sender: TObject);
begin
  if Visible then
    Close
  else
    Show;
end;

function TfrmMain.DetermineFontSize(ACanvas: TCanvas; AHeight: Integer): Integer;
var
  currentHeight: Integer;
begin
  currentHeight := ACanvas.TextHeight('M');
  if currentHeight < AHeight then
    repeat
      Result := ACanvas.Font.Size;
      ACanvas.Font.Size := ACanvas.Font.Size + 1;
      currentHeight := ACanvas.TextHeight('M');
    until currentHeight > AHeight
  else if currentHeight > AHeight then
    repeat
      Result := ACanvas.Font.Size;
      ACanvas.Font.Size := ACanvas.Font.Size - 1;
      currentHeight := ACanvas.TextHeight('M');
    until currentHeight < AHeight
  else
    Result := ACanvas.Font.Size;
end;

end.

