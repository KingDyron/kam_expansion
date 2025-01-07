unit KM_GUIMenuOptions;
{$I KaM_Remake.inc}
interface
uses
  Classes, Math, SysUtils,
  KM_Controls,
  KromOGLUtils,
  KM_MainSettings,
  KM_GUICommonOptions,
  KM_Pics, KM_Resolutions, KM_ResKeyFuncs, KM_GUICommonKeys,
  KM_InterfaceDefaults, KM_InterfaceTypes, KM_CommonTypes;


type
  TKMMenuOptions = class(TKMMenuPageCommon)
  private
    fGUICommonOptions: TKMGUICommonOptions;
    fOnPageChange: TKMMenuChangeEventText; // will be in ancestor class

    procedure BackClick;
    procedure EscKeyDown(Sender: TObject);
  public
    constructor Create(aParent: TKMPanel; aOnPageChange: TKMMenuChangeEventText; aOnOptionsChange, aOnKeysUpdated: TEvent);
    destructor Destroy; override;

    property GUICommonOptions: TKMGUICommonOptions read fGUICommonOptions;


    procedure Refresh;
    function Visible: Boolean;
    procedure Show;
  end;


implementation
uses
  KM_Main, KM_Music, KM_Sound, KM_RenderUI, KM_Resource, KM_ResTexts, KM_ResLocales, KM_ResFonts, KM_ResSound, KM_Video,
  KM_ResTypes,
  KM_GameSettings,
  KM_GameAppSettings;


{ TKMGUIMainOptions }
constructor TKMMenuOptions.Create(aParent: TKMPanel; aOnPageChange: TKMMenuChangeEventText; aOnOptionsChange, aOnKeysUpdated: TEvent);
begin
  inherited Create(gpOptions);

  fOnPageChange := aOnPageChange;
  fGUICommonOptions := TKMGUICommonOptions.Create(aParent, guiOptMenu, BackClick, aOnKeysUpdated);
  fGUICommonOptions.OnOptionsChange := aOnOptionsChange;

  OnEscKeyDown := EscKeyDown;
  // We cant pass pointers to Settings in here cos on GUI creation fMain/gGameApp are not initialized yet
end;


destructor TKMMenuOptions.Destroy;
begin
  fGUICommonOptions.Free;

  inherited;
end;


function TKMMenuOptions.Visible: Boolean;
begin
  Result := fGUICommonOptions.Visible;
end;


procedure TKMMenuOptions.Show;
begin
  fGUICommonOptions.Show;
end;


procedure TKMMenuOptions.BackClick;
begin
  // Return to MainMenu
  fOnPageChange(gpMainMenu);
end;


procedure TKMMenuOptions.EscKeyDown(Sender: TObject);
begin
  if not fGUICommonOptions.GuiCommonKeys.Visible then
    BackClick;
end;


procedure TKMMenuOptions.Refresh;
begin
  fGUICommonOptions.Refresh;
end;


end.

