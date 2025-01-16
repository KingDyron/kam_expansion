unit KM_FormLoading;
{$I KaM_Remake.inc}
interface
uses
  Classes,
  {$IFDEF FPC} LResources, {$ENDIF}
  Vcl.Forms, Vcl.Controls, Vcl.ComCtrls, Vcl.ExtCtrls, Vcl.StdCtrls, Vcl.Graphics,
  Vcl.Imaging.pngimage, SysUtils;

type
  TFormLoading = class(TForm)
    Bar1: TProgressBar;
    Image1: TImage;
    Panel1: TPanel;
    Label1: TLabel;
    Image2: TImage;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    Label9: TLabel;
    Label10: TLabel;
    Label11: TLabel;
    procedure FormHide(Sender: TObject);
    procedure FormShow(Sender: TObject);
  public
    function Steps : Integer;
    procedure LoadingStep;
    procedure LoadingText(const aData: UnicodeString);
  end;


implementation
//{$IFDEF WDC}
  {$R *.dfm}
//{$ENDIF}


{ TFormLoading }
procedure TFormLoading.FormHide(Sender: TObject);
begin
  //Put loading screen out of screen bounds, while hidden
  //This fixes bug, when on loading with fullscreen mode, user Alt-Tab into other window.
  //Then loading screen will be invisible, but prevents user interaction (block clicks etc)
  Left := -4000;
  Top := -4000;
end;

procedure TFormLoading.FormShow(Sender: TObject);
begin
  Refresh;
end;

procedure TFormLoading.LoadingStep;
begin
  if not Visible then Exit;
  Bar1.StepIt;
  Refresh;
end;


procedure TFormLoading.LoadingText(const aData: UnicodeString);
begin
  if not Visible then Exit;
  Label11.Caption := aData;
  Refresh;
end;

function TFormLoading.Steps: Integer;
begin
  Result := Bar1.Position;
end;
{$IFDEF FPC}
initialization
{$I KM_FormLoading.lrs}
{$ENDIF}

end.
