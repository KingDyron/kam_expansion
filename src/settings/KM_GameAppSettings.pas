unit KM_GameAppSettings;
{$I KaM_Remake.inc}
interface
uses
  Generics.Collections,
  KM_Settings, KM_SettingsXML, KM_GameSettings, KM_MainSettings, KM_KeysSettings,
  KM_IoXML;


type
  // GameApp settings, stored in the XML
  // Loaded and saved only once
  TKMGameAppSettings = class(TKMSettingsXML)
  private
    function GetGameSettings: TKMGameSettings;
    function GetKeySettings: TKMKeysSettings;
    function GetMainSettings: TKMainSettings;
  protected
    procedure LoadFromFile(const aPath: string); override;
    procedure SaveToFile(const aPath: string); override;
    function GetDefaultSettingsName: string; override;
    function GetSettingsName: string; override;
  public
    constructor Create(aScreenWidth, aScreenHeight: Integer);
    destructor Destroy; override;

    property MainSettings: TKMainSettings read GetMainSettings;
    property GameSettings: TKMGameSettings read GetGameSettings;
    property KeySettings: TKMKeysSettings read GetKeySettings;

    property Root: TKMXmlNode read fRoot;
  end;

var
  gGameAppSettings: TKMGameAppSettings;


implementation
uses
  SysUtils, INIfiles, Math,
  KM_Defaults;


{ TKMGameAppSettings }
constructor TKMGameAppSettings.Create(aScreenWidth, aScreenHeight: Integer);
begin
  gMainSettings := TKMainSettings.Create(aScreenWidth, aScreenHeight);
  gGameSettings := TKMGameSettings.Create;
  gKeySettings := TKMKeysSettings.Create;

  inherited Create;
end;


destructor TKMGameAppSettings.Destroy;
begin
  inherited;

  FreeAndNil(gKeySettings);
  FreeAndNil(gGameSettings);
  FreeAndNil(gMainSettings);
end;


procedure TKMGameAppSettings.LoadFromFile(const aPath: string);
begin
  inherited;

  gMainSettings.Root := Root;
  gGameSettings.Root := Root;
  gKeySettings.Root := Root;

  gMainSettings.LoadFromXML;
  gGameSettings.LoadFromXML;
  gKeySettings.LoadFromXML;
end;


procedure TKMGameAppSettings.SaveToFile(const aPath: string);
begin
  if SKIP_SETTINGS_SAVE then Exit;

  gMainSettings.SaveToXML;
  gGameSettings.SaveToXML;
  gKeySettings.SaveToXML;

  inherited;
end;


function TKMGameAppSettings.GetDefaultSettingsName: string;
begin
  Result := SETTINGS_FILE;
end;


function TKMGameAppSettings.GetGameSettings: TKMGameSettings;
begin
  Result := gGameSettings;
end;


function TKMGameAppSettings.GetKeySettings: TKMKeysSettings;
begin
  Result := gKeySettings;
end;


function TKMGameAppSettings.GetMainSettings: TKMainSettings;
begin
  Result := gMainSettings;
end;


function TKMGameAppSettings.GetSettingsName: string;
const
  GAME_APP_SETTINGS_NAME = 'GameApp settings';
begin
  Result := GAME_APP_SETTINGS_NAME;
end;


end.

