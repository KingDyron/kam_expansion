unit JsonForm;
{$I ..\..\KaM_Remake.inc}

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls;

type
  TForm3 = class(TForm)
    Button1: TButton;
    Button2: TButton;
    OpenDialog1: TOpenDialog;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form3: TForm3;

implementation
uses
  IOUtils,
  KM_JsonHelpers;


{$R *.dfm}

procedure TForm3.Button1Click(Sender: TObject);
var obj : TKMJsonObject;
begin
  OpenDialog1.Filter := 'TKMJson|*.tkmjson';
  If OpenDialog1.Execute then
  begin
    obj := TKMJsonObject.Create;
    obj.LoadFromStream(OpenDialog1.FileName);
    obj.SaveToFile( TPath.ChangeExtension(OpenDialog1.FileName, '_C.json') );
    obj.Free;
  end;
end;

procedure TForm3.Button2Click(Sender: TObject);
var obj : TKMJsonObject;
begin
  OpenDialog1.Filter := 'Json|*.json';
  If OpenDialog1.Execute then
  begin
    obj := TKMJsonObject.Create;
    obj.LoadFromFile(OpenDialog1.FileName);
    obj.SaveToStream( TPath.ChangeExtension(OpenDialog1.FileName, '_C.tkmjson') );
    obj.Free;
  end;
end;


end.
