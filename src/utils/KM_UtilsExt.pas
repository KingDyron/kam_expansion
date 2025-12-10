unit KM_UtilsExt;
{$I KaM_Remake.inc}
interface
uses
  Classes,
  KM_Controls, KM_ControlsBase,
  Vcl.Controls
  {$IFDEF MSWindows}
  , Windows
  {$ENDIF}
  {$IFDEF Unix}
  , unix, baseunix, UnixUtil
  {$ENDIF}
  ;

const
  DEF_K_LIMIT = 10000;


  function GetShiftState(aButton: TMouseButton): TShiftState;
  function IsRMBInShiftState(aShift: TShiftState): Boolean;
  function GetMultiplicator(aButton: TMouseButton): Word; overload;
  function GetMultiplicator(aShift: TShiftState; const aMultiplier: Integer = 10; aMultiply : Boolean = false): Word; overload;
  function IntToKStr(const aValue: Integer; aLimit: Integer = DEF_K_LIMIT): String;
  procedure SortVisibleControls(const aLeft, aTop, aWidth, aMargin: Integer;
                                const aControls : TKMControlArray; aSortUpward : Boolean = false; Centerize : Boolean = false);
  procedure SortControls(const aLeft, aTop, aWidth, aMargin: Integer;
                                const aControls : array of TKMControl; aSortUpward : Boolean = false; Centerize : Boolean = false); overload;
  procedure SortControls(const aLeft, aTop, aWidth, aHeight, aMargin: Integer;
                                const aControls : array of TKMControl); overload;
implementation
uses
  SysUtils, Math,
  KM_Defaults;
//  {$IFDEF FPC} FileUtil, {$ENDIF}
//  {$IFDEF WDC} IOUtils {$ENDIF};


const
  // Alt + LMB is considered as RMB
  // We can use it when hotkey is pressed (which is considered as LMB)
  RMB_ALTERNATIVE_SHIFT_STATE: TShiftState = [ssAlt, ssLeft];

function GetShiftState(aButton: TMouseButton): TShiftState;
begin
  Result := [];
  case aButton of
    mbLeft:   Include(Result, ssLeft);
    mbRight:  Include(Result, ssRight);
  end;

  if GetKeyState(VK_SHIFT) < 0 then
    Include(Result, ssShift);

  if GetKeyState(VK_MENU) < 0 then
    Include(Result, ssAlt);

  if GetKeyState(VK_CONTROL) < 0 then
    Include(Result, ssCtrl);
end;


function IsRMBInShiftState(aShift: TShiftState): Boolean;
begin
  Result := (ssRight in aShift) or (RMB_ALTERNATIVE_SHIFT_STATE <= aShift);
end;


function GetMultiplicator(aButton: TMouseButton): Word;
begin
  Result := GetMultiplicator(GetShiftState(aButton));
end;


function GetMultiplicator(aShift: TShiftState; const aMultiplier: Integer = 10; aMultiply : Boolean = false): Word;
begin
  if aMultiply then
  begin
    Result := 1;
    if ssCtrl in aShift then
      Result := Result * aMultiplier;
    if ssShift in aShift then
      Result := Result * aMultiplier;
    if ssAlt in aShift then
      Result := Result * aMultiplier;
    if ssRight in aShift then
      Result := Result * aMultiplier;
    Exit;
  end;
  Exclude(aShift, ssCtrl); //Ignore Ctrl
  // Consider Alt + LMB as a replacer for RMB
  if IsRMBInShiftState(aShift) then
    aShift := aShift + [ssRight] - [ssAlt, ssLeft];

  Result := Byte(aShift = [ssLeft])
          + Byte(aShift = [ssRight]) * aMultiplier
          + Byte(aShift = [ssShift,ssLeft]) * aMultiplier * aMultiplier
          + Byte(aShift = [ssShift,ssRight]) * aMultiplier * aMultiplier * aMultiplier;
end;
// 10123 -> '10k'
function IntToKStr(const aValue: Integer; aLimit: Integer = DEF_K_LIMIT): String;
begin
  if SHOW_RES_CNT_K_FOR_10000 and (aValue >= aLimit) then
    Result := IntToStr(aValue div 1000) + 'k'
  else
    Result := IntToStr(aValue);
end;

procedure SortVisibleControls(const aLeft, aTop, aWidth, aMargin: Integer; const
                              aControls : TKMControlArray; aSortUpward : Boolean = false; Centerize : Boolean = false);
var I, lastBottom, top, left,
    col, lastHeight: Integer;
    //newRowIndex : Integer;
    rowCtrls : TKMControlArray;

  procedure DoCenterize;
  var K : integer;
    nleft : Integer;
  begin
    if length(rowCtrls) <= 1 then
      Exit;
    nLeft := aWidth - (rowCtrls[high(rowCtrls)].Right - rowCtrls[0].Left);
    nLeft := aLeft + nLeft div 2;
    for K := 0 to high(rowCtrls) do
      if K = 0 then
        rowCtrls[K].Left := nLeft
      else
        rowCtrls[K].Left := rowCtrls[K-1].Right + aMargin;
  end;

begin
  col := 0;
  left := aLeft;
  top := aTop;
  lastBottom := 0;
  lastHeight := 0;
  SetLength(rowCtrls, 0);

  for I := Low(aControls) to High(aControls) do
  begin
    //Only visible objects
    if not aControls[I].Visible then
      Continue;
    //don't check these things on first object
    if col > 0 then
    begin
      //check if new object will fit in aWidth

      If (left + aControls[I].Width) > (aLeft + aWidth) then
      begin
        //make new row
        if aSortUpward then
        begin
          top := top - lastHeight + aMargin;
          left := aLeft;
          lastHeight := 0;
        end else
        begin
          top := lastBottom + aMargin;
          left := aLeft;

          //centerize controls
          if Centerize then
            DoCenterize;
          SetLength(rowCtrls, 0);
        end;
      end;
    end;
    SetLength(rowCtrls, length(rowCtrls) + 1);
    rowCtrls[high(rowCtrls)] := aControls[I];


    if aSortUpward then
      aControls[I].Top := top - aControls[I].Height
    else
      aControls[I].Top := top;

    aControls[I].Left := left;

    if aSortUpward then
    begin
      if aControls[I].Height > lastHeight then
        lastHeight := aControls[I].Height;
    end else
    if aControls[I].Bottom > lastBottom then
      lastBottom := aControls[I].Bottom;


    inc(left, aControls[I].Width + aMargin);

    inc(col);
  end;
  if Centerize then
    DoCenterize;
end;

procedure SortControls(const aLeft, aTop, aWidth, aMargin: Integer; const
                              aControls : array of TKMControl; aSortUpward : Boolean = false; Centerize : Boolean = false);
var I, lastBottom, top, left,
    col, lastHeight: Integer;
    //newRowIndex : Integer;
    rowCtrls : TKMControlArray;

  procedure DoCenterize;
  var K : integer;
    nleft : Integer;
  begin
    if length(rowCtrls) <= 1 then
      Exit;
    nLeft := aWidth - (rowCtrls[high(rowCtrls)].Right - rowCtrls[0].Left);
    nLeft := aLeft + nLeft div 2;
    for K := 0 to high(rowCtrls) do
      if K = 0 then
        rowCtrls[K].Left := nLeft
      else
        rowCtrls[K].Left := rowCtrls[K-1].Right + aMargin;
  end;

begin
  col := 0;
  left := aLeft;
  top := aTop;
  lastBottom := 0;
  lastHeight := 0;
  SetLength(rowCtrls, 0);

  for I := Low(aControls) to High(aControls) do
  begin
    //Only visible objects
    if not aControls[I].Visible then
      Continue;
    //don't check these things on first object
    if col > 0 then
    begin
      //check if new object will fit in aWidth

      If (left + aControls[I].Width) > (aLeft + aWidth) then
      begin
        //make new row
        if aSortUpward then
        begin
          top := top - lastHeight + aMargin;
          left := aLeft;
          lastHeight := 0;
        end else
        begin
          top := lastBottom + aMargin;
          left := aLeft;

          //centerize controls
          if Centerize then
            DoCenterize;
          SetLength(rowCtrls, 0);
        end;
      end;
    end;
    SetLength(rowCtrls, length(rowCtrls) + 1);
    rowCtrls[high(rowCtrls)] := aControls[I];


    if aSortUpward then
      aControls[I].Top := top - aControls[I].Height
    else
      aControls[I].Top := top;

    aControls[I].Left := left;

    if aSortUpward then
    begin
      if aControls[I].Height > lastHeight then
        lastHeight := aControls[I].Height;
    end else
    if aControls[I].Bottom > lastBottom then
      lastBottom := aControls[I].Bottom;


    inc(left, aControls[I].Width + aMargin);

    inc(col);
  end;
  if Centerize then
    DoCenterize;
end;

procedure SortControls(const aLeft, aTop, aWidth, aHeight, aMargin: Integer;
                       const aControls : array of TKMControl);
var I, J, left, top, ctrlHeight : Integer;
  rows : array of record
    Width : Integer;
    Ctrl : TKMControlArray;
  end;
  procedure NewRow;
  begin
    J := length(rows);
    SetLength(rows, J + 1);
  end;
  procedure AddToLastRow(aCtrl : TKMControl);
  var K : integer;
  begin
    K := length(rows[J].Ctrl);
    SetLength(rows[J].Ctrl, K + 1);
    rows[J].Ctrl[K] := aCtrl;
    If K = 0 then
      Inc(rows[J].Width, aCtrl.Width)
    else
      Inc(rows[J].Width, aCtrl.Width + aMargin);
  end;
var K : Integer;
begin
  ctrlHeight := 0;
  NewRow;
  left := aLeft;
  for I := low(aControls) to High(aControls) do
  begin
    //Only visible objects
    if not aControls[I].Visible then
      Continue;

    If (left + aControls[I].Width) > (aLeft + aWidth) then
    begin
      left := aLeft;
      NewRow;
    end;
    AddToLastRow(aControls[I]);
    Inc(left, aControls[I].Width + aMargin);
    ctrlHeight := Max(ctrlHeight, aControls[I].Height);
  end;

  top := (aHeight div 2) - (ctrlHeight * length(rows) + (aMargin * (length(rows) - 1))) div 2;
  //centerize controls
  for I := 0 to High(rows) do
  begin
    left := (aWidth div 2) - (rows[I].Width div 2);
    for K := 0 to High(rows[I].Ctrl) do
    begin
      rows[I].Ctrl[K].Left := left;
      inc(left, rows[I].Ctrl[K].Width + aMargin);
      rows[I].Ctrl[K].Top := top + I * (ctrlHeight + aMargin);
    end;
  end;


end;


end.
