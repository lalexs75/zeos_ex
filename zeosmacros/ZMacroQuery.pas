{ ZMacroQuery unit

  Copyright (C) 2005-2015 Lagunov Aleksey alexs@yandex.ru
  original conception for macros from rx library for Delphi (c)

  This library is free software; you can redistribute it and/or modify it
  under the terms of the GNU Library General Public License as published by
  the Free Software Foundation; either version 2 of the License, or (at your
  option) any later version with the following modification:

  As a special exception, the copyright holders of this library give you
  permission to link this library with independent modules to produce an
  executable, regardless of the license terms of these independent modules,and
  to copy and distribute the resulting executable under terms of your choice,
  provided that you also meet, for each linked independent module, the terms
  and conditions of the license of that module. An independent module is a
  module which is not derived from or based on this library. If you modify
  this library, you may extend this exception to your version of the library,
  but you are not obligated to do so. If you do not wish to do so, delete this
  exception statement from your version.

  This program is distributed in the hope that it will be useful, but WITHOUT
  ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
  FITNESS FOR A PARTICULAR PURPOSE. See the GNU Library General Public License
  for more details.

  You should have received a copy of the GNU Library General Public License
  along with this library; if not, write to the Free Software Foundation,
  Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
}
unit ZMacroQuery;

{$I Z.inc}
{.$H+}
interface

uses
  {$IFDEF FPC}

//  Forms, Dialogs, //debug!!

  {$ELSE}
  Windows, Messages, Consts, Controls, Forms,
  SyncObjs,
{$IFDEF ZVCL5}
  DbCommon,
{$ENDIF}
{$IFDEF ZVCL6}
  Variants, RTLConsts,
{$ENDIF}
{$IFDEF ZVCL71}
  Contnrs, System.Runtime.InteropServices,
{$ENDIF}
  {$ENDIF}
  SysUtils, Classes, Db, ZDataset;

const
  DefaultQueryMacroChar	= '%';
  DefaultQueryMacroExprConst	= '0=0';

type

  { TZMacroQuery }

  TZMacroQuery = class(TZQuery)
  private
    FMacroExprDef: string;
    FMacroChar: Char;
    FMacros: TParams;
    FMacrosExpanding: Boolean;
    FSQLPattern: TStrings;		// (unexpanded) statement with macros
    FSaveQueryChanged: TNotifyEvent;
    function GetMacros: TParams;
    function GetMacroCount: Word;
    procedure PatternChanged(Sender : TObject);
    procedure QueryChanged(Sender : TObject);
    procedure CreateMacros;
    procedure SetMacroChar(const Value : Char);
    procedure SetMacros(const Value : TParams);
    procedure SetSQL(Value : TStrings);
  protected
    procedure OpenCursor(InfoQuery : Boolean = False); override;
  public
    constructor Create(AOwner : TComponent); override;
    destructor Destroy; override;
    procedure ExecSQL; override;
    procedure ExpandMacros;
    function MacroByName(const MacroName : string): TParam;
    property Macros : TParams read GetMacros write SetMacros;

    procedure CloseOpen(AFetchAll:boolean);
  published
    property MacroChar : Char read FMacroChar write SetMacroChar default DefaultQueryMacroChar;
    property MacroCount : Word read GetMacroCount;
    property SQL : TStrings read FSQLPattern write SetSQL;
    property MacroExprDef:string read FMacroExprDef write FMacroExprDef;
  end;

procedure CreateParamsFromSQL(List : TParams; const SQL : string; ParamPrefix : Char);
function IsNameDelimiter(C : Char) : Boolean;
function IsLiteral(C : Char) : Boolean;
function StripLiterals(const Str : string) : string;
function LocateText(const Substr, S : string) : Integer;
function StrFindFromPos(const Substr, S : string; StartPos : Integer) : Integer;
function ReplaceString(Once : Boolean; OldStr, NewStr : string; var ResultStr : string) : Boolean;

procedure Register;

implementation

{$R zeos_img_m.res}

constructor TZMacroQuery.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FMacroExprDef:=DefaultQueryMacroExprConst;

  FMacroChar := DefaultQueryMacroChar;

  FSaveQueryChanged := TStringList(inherited SQL).OnChange;
  TStringList(inherited SQL).OnChange := {$IFDEF FPC}@{$ENDIF}QueryChanged;

  FSQLPattern := TStringList.Create;
  TStringList(FSQLPattern).OnChange := {$IFDEF FPC}@{$ENDIF}PatternChanged;

  FMacros := TParams.Create; // {$IFDEF ZVCL4} (Self) {$ENDIF};
end;

destructor TZMacroQuery.Destroy;
begin
  inherited;
  FMacros.Free;
  FSQLPattern.Free;
end;

procedure TZMacroQuery.OpenCursor(InfoQuery: Boolean = False);
begin
  ExpandMacros;
  inherited OpenCursor(InfoQuery);
end;

procedure TZMacroQuery.ExecSQL;
begin
  ExpandMacros;
  inherited ExecSQL;
end;

function TZMacroQuery.GetMacroCount: Word;
begin
  Result := FMacros.Count;
end;

function TZMacroQuery.GetMacros: TParams;
begin
  Result := FMacros;
end;

function TZMacroQuery.MacroByName(const MacroName: string): TParam;
begin
  Result := FMacros.ParamByName(MacroName);
end;

procedure TZMacroQuery.CloseOpen(AFetchAll: boolean);
begin
  Active:=false;
  Active:=true;
  if AFetchAll then
    FetchAll;
end;

procedure TZMacroQuery.SetMacroChar(const Value: Char);
begin
  if Value <> FMacroChar then
  begin
    FMacroChar := Value;
    CreateMacros;
  end;
end;

procedure TZMacroQuery.SetMacros(const Value: TParams);
begin
  FMacros.AssignValues(Value);
end;

{ Sets an unexpanded(FSQLPattern) and a real(expanded)(inherited SQL) statements }

procedure TZMacroQuery.SetSQL(Value: TStrings);
begin
  if FSQLPattern.Text <> Value.Text then
  begin
    FSQLPattern.BeginUpdate;
    try
      FSQLPattern.Assign(Value);
    finally
      FSQLPattern.EndUpdate;
    end;

    inherited SQL := Value;
  end;
end;

procedure TZMacroQuery.QueryChanged(Sender: TObject);
begin
  if csLoading in ComponentState then Exit;
  FSaveQueryChanged(Sender);
// to exclude recursion
  if not FMacrosExpanding then SQL := inherited SQL;
end;

procedure TZMacroQuery.PatternChanged(Sender: TObject);
begin
  CreateMacros;
  ExpandMacros;
end;

{ parses a statement for macros and fills FMacros list }

procedure TZMacroQuery.CreateMacros;
var
  List: TParams;
  i: Integer;
begin
  if not (csReading in ComponentState) then
  begin
    List := TParams.Create {$IFDEF ZVCL4}(Self){$ENDIF};
    try
      CreateParamsFromSQL(List, FSQLPattern.Text, FMacroChar);

// assign the current parameter values

      List.AssignValues(FMacros);

// set FMacros[i].DataType to ftString and a default value

      for i:=0 to List.Count-1 do List[i].AsString := FMacroExprDef;

      FMacros.Clear;
      FMacros.Assign(List);
    finally
      List.Free;
    end;
  end
  else
  begin
    FMacros.Clear;
    CreateParamsFromSQL(FMacros, FSQLPattern.Text, FMacroChar);
    for i:=0 to FMacros.Count-1 do FMacros[i].AsString := FMacroExprDef;
  end;
end;

{ Expands macros and assign a real SQL text }
procedure TZMacroQuery.ExpandMacros;
var
  i : Integer;
  s : string;
begin
  s := FSQLPattern.Text;
  //ShowMessageFmt('%s '#13'----'#13' %s ', [FSQLPattern.Text, S]);
  for i := 0 to MacroCount - 1 do
  begin
    if Macros[i].DataType = ftUnknown then Continue;
//    ShowMessageFmt('%s  =  %s', [Macros[i].Name, Macros[i].AsString]);
    ReplaceString(False, MacroChar + Macros[i].Name, Macros[i].AsString, s);
  end;

// to exclude a recursion

  FMacrosExpanding := True;
//  try
    inherited SQL.Text := s;
//  except
//    on E:Exception do
//  end;
  FMacrosExpanding := False;

//  for i := 0 to MacroCount - 1 do   ShowMessageFmt('%s  =  %s', [Macros[i].Name, Macros[i].AsString]);
//  ShowMessageFmt('%s '#13'----'#13' %s '#13' , len= %d, len2 = %d', [FSQLPattern.Text, inherited SQL.Text, Length(S) , Length(inherited SQL.Text)]);
end;

{ Parse SQL-statement and create instances of TParam }

procedure CreateParamsFromSQL(List: TParams; const SQL: string; ParamPrefix: Char);
var
  CurPos, StartPos, Len : Integer;
  CurChar : Char;
  Literal : Char; // #0, when the current character is not quoted, else it's equal the last significant quote
  EmbeddedLiteral : Boolean;
  s, sName : string;
begin
  S := SQL;
  Len := Length(S);
  if Len = 0 then Exit;
  CurPos := 1;
  Literal := #0; // #0 - not inside a quoted string
  EmbeddedLiteral := False;
  repeat
    CurChar := S[CurPos];

// Is it parameter name ?

    if (CurChar = ParamPrefix) and not Boolean(Literal) and (CurPos < Len) and (S[CurPos + 1] <> ParamPrefix) then
    begin
      StartPos := CurPos;

// locate end of parameter name

      while (CurChar <> #0) and (CurPos <= Len) and (Boolean(Literal) or not IsNameDelimiter(CurChar)) do
      begin
        Inc(CurPos);
        if CurPos > Len then Break;
        CurChar := S[CurPos];
        if IsLiteral(CurChar) then
        begin

// To process a quote, which is inside a parameter name like :"param's name"

          if not Boolean(Literal) then Literal := CurChar
          else if Literal = CurChar then Literal := #0; // the quoted string has to be finished by the same quote character
            
// if a parameter name is quoted

          if CurPos = StartPos + 1 then EmbeddedLiteral := True;
        end;
      end;

      if EmbeddedLiteral then
      begin
        sName := StripLiterals(Copy(S, StartPos + 1, CurPos - StartPos - 1));
        EmbeddedLiteral := False;
      end
      else
      begin
        sName := Copy(S, StartPos + 1, CurPos - StartPos - 1);
      end;

      if Assigned(List) then
      begin
        if not Assigned(List.FindParam(sName)) then
        begin
{$IFDEF ZVCL4}
          TParam(List.Add).Name := sName;
          List.ParamByName(sName).ParamType := ptInput;
{$ELSE}
          List.CreateParam(ftUnknown, sName, ptInput);
{$ENDIF}
        end;
      end;
    end
    else
    if (CurChar = ParamPrefix) and not Boolean(Literal) and (CurPos < Len) and (S[CurPos + 1] = ParamPrefix) then
    begin
      // remove one colon (ParamPrefix), which happens twice
       Delete(S, CurPos, 1)
    end
    else if IsLiteral(CurChar) then
    begin
      // To process a quote, which is inside a quoted string like "param's :name"
      if not Boolean(Literal) then
        Literal := CurChar
      else
      if Literal = CurChar then
        Literal := #0;
    end;
    Inc(CurPos);
  until (CurChar = #0) or (CurPos > Len);
end;

function IsNameDelimiter(C : Char) : Boolean;
begin
  Result := AnsiChar(C) in [' ', ',', ';', ')', #13, #10];
end;

function IsLiteral(C : Char) : Boolean;
begin
  Result := AnsiChar(C) in ['''', '"'];
end;

function StripLiterals(const Str : string) : string;
var
  Len : Integer;
begin
  Result := Str;
  Len := Length(Result);
  if (Len >= 1) and IsLiteral(Result[1]) then Result := Copy(Result, 2, Length(Result) - 1);
  Len := Length(Result);
  if IsLiteral(Result[Len]) then SetLength(Result, Len - 1);
end;

function LocateText(const Substr, S : string) : Integer;
begin
  Result := Pos(Substr, S);

// when strings are present in different cases

  if Result = 0 then Result := Pos(UpperCase(Substr), UpperCase(S));
end;

function StrFindFromPos(const Substr, S : string; StartPos : Integer) : Integer;
var
  Str : string;
begin
  if StartPos > 1 then
  begin

// Copy returns empty string when StartPos > Length(S)

    Str := Copy(S, StartPos, Length(S) - StartPos + 1);
  end
  else
  begin
    Str := S;
    StartPos := 1;
  end;

  Result := LocateText(Substr, Str);

// return an index of Pascal string

  if Result > 0 then Result := StartPos + (Result - 1);
end;

function ReplaceString(Once : Boolean; OldStr, NewStr : string; var ResultStr : string) : Boolean;
var
  i, FoundPos, Literals : Integer;
  bFound : Boolean;
begin
  Result := False;

  repeat
    FoundPos := 0;
    repeat

// pass the first char of the similar string, which was found earlier

      if FoundPos > 0 then Inc(FoundPos);
      FoundPos := StrFindFromPos(OldStr, ResultStr, FoundPos);

// check whether OldStr at the end of ResultStr or has a delimiter after itself

      bFound :=
        (FoundPos > 0) and
        (
          (Length(ResultStr) = FoundPos + Length(OldStr) - 1) or
          IsNameDelimiter(ResultStr[FoundPos + Length(OldStr)])
        );

      if bFound then
      begin
        Literals := 0;
        for i := 1 to FoundPos - 1 do
        begin
          if IsLiteral(ResultStr[i]) then Inc(Literals);
        end;
        bFound := Literals mod 2 = 0;	// OldStr has not to be quoted
        if bFound then
        begin
          Delete(ResultStr, FoundPos, Length(OldStr));
          Insert(NewStr, ResultStr, FoundPos);
          FoundPos := FoundPos + Length(NewStr) - 1;
          Result := True;
          if Once then Exit;
        end;
      end;
    until FoundPos = 0;
  until not bFound;
end;

const
  ZEOS_DB_PALETTE = 'Zeos Access';

procedure Register;
begin
  RegisterComponents(ZEOS_DB_PALETTE, [TZMacroQuery]);
end;

end.
