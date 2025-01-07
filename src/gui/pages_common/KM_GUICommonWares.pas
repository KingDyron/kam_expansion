unit KM_GUICommonWares;
{$I KaM_Remake.inc}
interface
uses
  Classes,
  KM_Controls, KM_ControlsBase, KM_ControlsScroll,
  KM_CommonTypes, KM_Defaults, KM_ResTypes;

type
  TKMGUICommonWares = class
  private
    fIsVirtual : Boolean;
    function GetButton(aIndex : Integer) : TKMButtonFlat;
  protected
    PanelWares : TKMExpandPanelCollection;
      Button_Ware : array of TKMButtonFlat;
  public

    procedure SetWareCount(aWare : TKMWareType; aCount : integer); overload;
    procedure SetWareCount(aWareID : Integer; aCount : integer); overload;

    property Ware[aIndex : Integer] : TKMButtonFlat read GetButton; default;
    Property Panel : TKMExpandPanelCollection read PanelWares;

    constructor Create(aParent: TKMPanel; Left, Top, Width, Height : Integer; aOnClick : TNotifyEvent;  IsClickable : Boolean = true; IsVirtualWares : Boolean = false);
    function Visible: Boolean;
    procedure Show;
    procedure Hide;
  end;

implementation
uses
  SysUtils, Math, KM_UtilsExt,
  KM_ResSound, KM_InterfaceGame,
  KM_ResTexts, KM_RenderUI, KM_ResFonts, KM_Resource;


{ TKMGUICommonKeys }

constructor TKMGUICommonWares.Create(aParent: TKMPanel; Left, Top, Width, Height : Integer; aOnClick : TNotifyEvent;
                                      IsClickable : Boolean = true; IsVirtualWares : Boolean = false);
var MaxColumnCount : Integer;
  I, K, J, C, topB: Integer;
  W : TKMWareType;
begin
  inherited Create;
  PanelWares := TKMExpandPanelCollection.Create(aParent,  Left, Top, Width, Height);
  PanelWares.Hitable := false;
  MaxColumnCount := (Width div 37);
  fIsVirtual := IsVirtualWares;

  if fIsVirtual then
    SetLength(Button_Ware, gRes.Wares.VirtualWares.Count);

  if fIsVirtual then
    for I := 0 to High(Button_Ware) do
    begin
      Button_Ware[I] := TKMButtonFlat.Create(PanelWares, (I mod MaxColumnCount) * 37, (I div MaxColumnCount) * 38, 32, 36, 0);
      Button_Ware[I].Tag := I;
      Button_Ware[I].Clickable := IsClickable;
      Button_Ware[I].OnClick := aOnClick;
      Button_Ware[I].TexID := gRes.Wares.VirtualWares.Ware[I].GUIIcon;
      Button_Ware[I].Hint := gResTexts[gRes.Wares.VirtualWares.Ware[I].TextID];
    end;

  C := 0;
  J := 0;
  K := 0;
  topB := 0;
  if not fIsVirtual then
    for I := 1 to STORE_RES_COUNT do
    begin
      W := StoreResType[I];
      if not gRes.Wares[W].IsValid then
      begin
        C := 0;
        if J > 0 then
          topB := Button_Ware[J - 1].Bottom;

        with TKMLabel.Create(PanelWares, 0, topB, PanelWares.Width, 15, gResTexts[1657 + K], fntOutline, taCenter) do
          Hitable := false;
        inc(topB, 17);
        inc(K);
        Continue;
      end;
      SetLength(Button_Ware, J + 1);

      Button_Ware[J] := TKMButtonFlat.Create(PanelWares, (C mod MaxColumnCount) * 37, topB + (C div MaxColumnCount) * 37, 32, 36, 0);
      Button_Ware[J].Tag := ord(W);
      Button_Ware[J].Clickable := IsClickable;
      Button_Ware[J].OnClick := aOnClick;
      Button_Ware[J].TexID := gRes.Wares[W].GUIIcon;
      Button_Ware[J].Hint := gRes.Wares[W].Title;

      PanelWares.Height := Button_Ware[J].Bottom + 3;
      inc(C);
      inc(J);
    end;
end;

procedure TKMGUICommonWares.Hide;
begin
  PanelWares.Hide;
end;

procedure TKMGUICommonWares.Show;
begin
  PanelWares.Show;
end;

function TKMGUICommonWares.Visible: Boolean;
begin
  Result := PanelWares.Visible;
end;


function TKMGUICommonWares.GetButton(aIndex: Integer): TKMButtonFlat;
begin
  Result := nil;

  if (aIndex >= 0) and (aIndex <= high(Button_Ware)) then
    Result := Button_Ware[aIndex];

end;
procedure TKMGUICommonWares.SetWareCount(aWare : TKMWareType; aCount : integer);
var I : Integer;
begin
  for I := 0 to High(Button_Ware) do
    if Button_Ware[I].Tag = ord(aWare) then
    begin
      Button_Ware[I].Caption := IntToKStr(aCount);
      Exit;
    end;
    

end;

procedure TKMGUICommonWares.SetWareCount(aWareID : Integer; aCount : integer);
begin
  Button_Ware[aWareID].Caption := IntToKStr(aCount);
end;

end.

