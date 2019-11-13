program pickgitmoji;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, UfrmMain,
  Classes, simpleipc
  { you can add units after this };

{$R *.res}

type

  { TMessageHandler }

  TMessageHandler = class(TComponent)
    procedure MessageReceived(ASender: TObject);
    procedure Poll(ASender: TObject; var Done: Boolean);
  end;

var
  ipcServer: TSimpleIPCServer;
  ipcClient: TSimpleIPCClient;
  msgHandler: TMessageHandler;

{ TMessageHandler }

procedure TMessageHandler.MessageReceived(ASender: TObject);
begin
  if ipcServer.StringMessage = 'show' then
    frmMain.Show;
end;

procedure TMessageHandler.Poll(ASender: TObject; var Done: Boolean);
begin
  repeat until not ipcServer.PeekMessage(1, True);
  Done := False;
end;

procedure RunApp;
begin
  RequireDerivedFormResource := True;
  Application.Scaled := True;
  Application.ShowMainForm := False;
  Application.Initialize;
  Application.CreateForm(TfrmMain, frmMain);

  Application.Run;
end;

begin
  ipcServer := TSimpleIPCServer.Create(Application);
  ipcServer.ServerID := 'pickgitmoji';
  ipcServer.Global := True;
  ipcClient := TSimpleIPCClient.Create(Application);
  ipcClient.ServerID := ipcServer.ServerID;
  msgHandler := TMessageHandler.Create(Application);

  if ipcClient.ServerRunning then
  begin
    ipcClient.Active := True;
    ipcClient.SendStringMessage('show');
  end else
  begin
    ipcServer.StartServer(False);

    Application.AddOnIdleHandler(@msgHandler.Poll, False);
    ipcServer.OnMessage := @msgHandler.MessageReceived;

    RunApp;
  end;
end.

