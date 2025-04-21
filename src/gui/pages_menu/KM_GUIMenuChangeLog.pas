unit KM_GUIMenuChangeLog;
{$I KaM_Remake.inc}
interface
uses
  Math, SysUtils, Classes,
  KM_Defaults,
  KM_Controls, KM_ControlsBase, KM_ControlsMemo,
  KM_CommonTypes, KM_InterfaceDefaults, KM_InterfaceTypes,
  KM_ResTypes, KM_ResHouses;

type

  TKMMenuChangeLog = class(TKMMenuPageCommon)
  private
    fOnPageChange: TKMMenuChangeEventText;

    procedure BackClick(Sender: TObject);
  protected
    Panel_ChangeLog: TKMPanel;
      Memo_ChangeLog : TKMMemo;
      Button_T_Back : TKMButton;
  public
    constructor Create(aParent: TKMPanel; aChangeLog : String; aOnPageChange: TKMMenuChangeEventText);

    procedure Show;
    procedure Hide;
  end;


implementation
uses
  KM_ResTexts, KM_ResFonts, KM_Resource,
  KM_CommonUtils, KM_RenderUI;

{ TKMGUIMenuSingleMap }
constructor TKMMenuChangeLog.Create(aParent: TKMPanel; aChangeLog : String; aOnPageChange: TKMMenuChangeEventText);
begin
  inherited Create(gpChangeLog);

  fOnPageChange := aOnPageChange;
  OnEscKeyDown := BackClick;
  Panel_ChangeLog := TKMPanel.Create(aParent, 0, 0, aParent.Width, aParent.Height);

    Memo_ChangeLog := TKMMemo.Create(Panel_ChangeLog, 30, 40, Panel_ChangeLog.Width - 60, Panel_ChangeLog.Height - 100, fntMetal, bsGame, false);
    Memo_ChangeLog.WordWrap := false;
    Memo_ChangeLog.Text := aChangeLog;

    Button_T_Back   := TKMButton.Create(Panel_ChangeLog, Panel_ChangeLog.Width div 2 - 175,Panel_ChangeLog.Bottom - 40 ,350,30,gResTexts[TX_MENU_BACK],bsMenu);
    Button_T_Back.OnClick   := BackClick;
end;

procedure TKMMenuChangeLog.Show;
begin
  Panel_ChangeLog.Show;
end;

procedure TKMMenuChangeLog.Hide;
begin
  Panel_ChangeLog.Hide;
end;


procedure TKMMenuChangeLog.BackClick(Sender: TObject);
begin
  fOnPageChange(gpMainMenu);
end;

end.
