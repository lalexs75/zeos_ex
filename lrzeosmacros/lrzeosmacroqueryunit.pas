{ lrZeosMacroQueryUnit unit

  Copyright (C) 2011 - 2015 Lagunov Aleksey alexs@yandex.ru

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
unit lrZeosMacroQueryUnit;

{$mode objfpc}{$H+}

interface

uses
    Classes, SysUtils, LResources, Graphics, DB, LR_Class, LR_DBComponent,
    LR_DB_Zeos, ZMacroQuery;

type

  { TLRZMacroQuery }

  TLRZMacroQuery = class(TLRDataSetControl)
  private
    FDatabase: string;
    //FMacroChar: Char;
    FParams: TQueryParamList;
    FSaveSQL:string;
    function GetMacroChar: Char;
    function GetMacroCount: Word;
    procedure SetDatabase(AValue: string);
    procedure SetMacroChar(AValue: Char);
    procedure ZQueryBeforeOpen(ADataSet: TDataSet);
    procedure DoMakeParams;
    procedure DoEditParams;
  protected
    function GetSQL: string;
    procedure SetSQL(AValue: string);
    procedure SetDataSource(AValue: string); override;
    procedure AfterLoad;override;
  public
    constructor Create(AOwnerPage:TfrPage); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;

    procedure LoadFromXML(XML: TLrXMLConfig; const Path: String); override;
    procedure SaveToXML(XML: TLrXMLConfig; const Path: String); override;
  published
    property SQL:string read GetSQL write SetSQL;
    property Database:string read FDatabase write SetDatabase;
    property Params:TQueryParamList read FParams write FParams;
    property MacroChar : Char read GetMacroChar write SetMacroChar default DefaultQueryMacroChar;
    property MacroCount : Word read GetMacroCount;
  end;


  TLRZMacroQueryReg = class(TComponent)
  end;

procedure Register;
implementation
uses LR_Utils, Forms, Controls, ZConnection, lr_EditParams, PropEdits,
    DBPropEdits;

{$R lr_zeos_img_m.res}

procedure Register;
begin
  RegisterComponents('LazReport',[TLRZMacroQueryReg]);
end;

var
  lrBMP_ZQuery:TBitmap = nil;

procedure InitLRComp;
begin
  if not assigned(lrBMP_ZQuery) then
  begin
    lrBMP_ZQuery := TbitMap.Create;
    lrBMP_ZQuery.LoadFromResourceName(HInstance, 'TLRZMacroQuery');
    frRegisterObject(TLRZMacroQuery, lrBMP_ZQuery, 'TLRZMacroQuery', nil, otlUIControl, nil);
  end;
end;

{ TLRZMacroQuery }

procedure TLRZMacroQuery.SetDatabase(AValue: string);
var
  D:TComponent;
begin
  if FDatabase=AValue then Exit;
  FDatabase:=AValue;

  DataSet.Active:=false;
  D:=frFindComponent(TZMacroQuery(DataSet).Owner, FDatabase);
  if Assigned(D) and (D is TZConnection)then
    TZMacroQuery(DataSet).Connection:=TZConnection(D);
end;

function TLRZMacroQuery.GetMacroCount: Word;
begin
  Result:=TZMacroQuery(DataSet).MacroCount;
end;

function TLRZMacroQuery.GetMacroChar: Char;
begin
  Result:=TZMacroQuery(DataSet).MacroChar;
end;

procedure TLRZMacroQuery.SetMacroChar(AValue: Char);
begin
  TZMacroQuery(DataSet).MacroChar:=AValue;
end;

procedure TLRZMacroQuery.ZQueryBeforeOpen(ADataSet: TDataSet);
var
  i: Integer;
  s: String;
  SaveView: TfrView;
  SavePage: TfrPage;
  SaveBand: TfrBand;
  Q:TZMacroQuery;
  P:TQueryParam;
  V:Variant;
begin
  Q:=TZMacroQuery(DataSet);
  SaveView := CurView;
  SavePage := CurPage;
  SaveBand := CurBand;

  CurView := Self;
  CurPage := OwnerPage;
  CurBand := nil;

  for i := 0 to Q.Params.Count - 1 do
  begin
    S:=Q.Params[i].Name;
    P:=FParams.ParamByName(S);
    if Assigned(P) and (P.ParamValue <> '') and (OwnerPage.Report.DocMode = dmPrinting) then
    begin
      V:=frParser.Calc(P.ParamValue);
      if V = null then
        Q.Params[i].Clear
      else
      case P.ParamType of
        ftDate,
        ftDateTime:Q.Params[i].AsDateTime := V;//frParser.Calc(P.ParamValue);
        ftInteger:Q.Params[i].AsInteger := V;//frParser.Calc(P.ParamValue);
        ftFloat:Q.Params[i].AsFloat := V;//frParser.Calc(P.ParamValue);
        ftString:Q.Params[i].AsString := V;//frParser.Calc(P.ParamValue);
      else
        Q.Params[i].Value := V;//frParser.Calc(P.ParamValue);
      end;
    end;
  end;

  for i := 0 to Q.Macros.Count - 1 do
  begin
    S:=Q.Macros[i].Name;
    P:=FParams.ParamByName(S);
    if Assigned(P) and (P.ParamValue <> '') and (OwnerPage.Report.DocMode = dmPrinting) then
    begin
      case P.ParamType of
        ftDate,
        ftDateTime:Q.Macros[i].AsDateTime := frParser.Calc(P.ParamValue);
        ftInteger:Q.Macros[i].AsInteger := frParser.Calc(P.ParamValue);
        ftFloat:Q.Macros[i].AsFloat := frParser.Calc(P.ParamValue);
        ftString:Q.Macros[i].AsString := frParser.Calc(P.ParamValue);
      else
        Q.Macros[i].Value := frParser.Calc(P.ParamValue);
      end;
    end;
  end;

  if Assigned(Q.Connection) then
    if not Q.Connection.Connected then Q.Connection.Connect;

  CurView := SaveView;
  CurPage := SavePage;
  CurBand := SaveBand;
end;

procedure TLRZMacroQuery.DoMakeParams;
var
  Q:TZMacroQuery;
  i:integer;
begin
  Q:=TZMacroQuery(DataSet);
  if (Q.Params.Count > 0) or (Q.Macros.Count>0) then
  begin
    //Add new params...
    for i:=0 to Q.Params.Count-1 do
    begin
      if not Assigned(FParams.ParamByName(Q.Params[i].Name)) then
        FParams.Add(ftUnknown, Q.Params[i].Name, '');
    end;

    for i:=0 to Q.Macros.Count-1 do
    begin
      if not Assigned(FParams.ParamByName(Q.Macros[i].Name)) then
        FParams.Add(ftUnknown, Q.Macros[i].Name, '');
    end;

    //Delete not exists params
    for i:=FParams.Count-1 downto 0 do
    begin
      if
          (not Assigned(Q.Params.FindParam(TQueryParam(FParams[i]).ParamName)))
        and
          (not Assigned(Q.Macros.FindParam(TQueryParam(FParams[i]).ParamName))) then
        FParams.Delete(i);
    end;
  end
  else
    FParams.Clear;
end;

procedure TLRZMacroQuery.DoEditParams;
begin
  lrEditParamsForm:=TlrEditParamsForm.Create(Application);
  lrEditParamsForm.LoadParamList(FParams);
  if lrEditParamsForm.ShowModal = mrOk then
  begin
    lrEditParamsForm.SaveParamList(FParams);
    if Assigned(frDesigner) then
      frDesigner.Modified:=true;
  end;
  lrEditParamsForm.Free;
end;

function TLRZMacroQuery.GetSQL: string;
begin
  Result:=TZMacroQuery(DataSet).SQL.Text;
end;

procedure TLRZMacroQuery.SetSQL(AValue: string);
begin
  DataSet.Active:=false;
  TZMacroQuery(DataSet).SQL.Text:=AValue;
  DoMakeParams;
  if Assigned(frDesigner) then
    frDesigner.Modified:=true;
end;

procedure TLRZMacroQuery.SetDataSource(AValue: string);
var
  D:TComponent;
begin
  inherited SetDataSource(AValue);
  D:=frFindComponent(OwnerForm, AValue);
  if Assigned(D) and (D is TDataSource)then
    TZMacroQuery(DataSet).DataSource:=TDataSource(D);
end;

procedure TLRZMacroQuery.AfterLoad;
var
  D:TComponent;
begin

  D:=frFindComponent(OwnerForm, DataSource);
  if Assigned(D) and (D is TDataSource)then
    TZMacroQuery(DataSet).DataSource:=TDataSource(D);

  D:=frFindComponent(DataSet.Owner, FDatabase);
  if Assigned(D) and (D is TZConnection)then
  begin
    TZMacroQuery(DataSet).Connection:=TZConnection(D);
//    DataSet.Active:=FActive;
  end;

  //temp fix
  TZMacroQuery(DataSet).SQL.Text:=FSaveSQL;


  if Assigned(TZMacroQuery(DataSet).Connection) then
    DataSet.Active:=FActive;
end;

constructor TLRZMacroQuery.Create(AOwnerPage: TfrPage);
begin
  inherited Create(AOwnerPage);
  BaseName := 'lrZMacroQuery';
  FParams:=TQueryParamList.Create;

  DataSet:=TZMacroQuery.Create(OwnerForm);
  MacroChar:='@';
  TZMacroQuery(DataSet).MacroExprDef:='';
  DataSet.BeforeOpen:=@ZQueryBeforeOpen;
  DataSet.BeforeRefresh:=@ZQueryBeforeOpen;
end;

destructor TLRZMacroQuery.Destroy;
begin
  FreeAndNil(FParams);
  inherited Destroy;
end;

procedure TLRZMacroQuery.Assign(Source: TPersistent);
var
  i: Integer;
begin
  inherited Assign(Source);
  if Source is TLRZMacroQuery then
  begin
    MacroChar:=TLRZMacroQuery(Source).MacroChar;
    SQL:=TLRZMacroQuery(Source).SQL;
    Database:=TLRZMacroQuery(Source).Database;

    FParams.Clear;
    for i:=0 to TLRZMacroQuery(Source).Params.Count - 1 do
      FParams.Add(TQueryParam(TLRZMacroQuery(Source).Params[i]).ParamType,
                  TQueryParam(TLRZMacroQuery(Source).Params[i]).ParamName,
                  TQueryParam(TLRZMacroQuery(Source).Params[i]).ParamValue);
  end;
end;

function StrToFieldType(AStrTypeName:string):TFieldType;
var
  i:TFieldType;
begin
  Result:=ftUnknown;
  AStrTypeName:=UpperCase(AStrTypeName);
  for i in TFieldType do
  begin
    if UpperCase(Fieldtypenames[i]) = AStrTypeName then
    begin
      Result:=i;
      exit;
    end;
  end;
end;

procedure TLRZMacroQuery.LoadFromXML(XML: TLrXMLConfig; const Path: String);
var
  C, I:integer;
  S:string;
begin
  inherited LoadFromXML(XML, Path);

  //TZMacroQuery(DataSet).SQL.Text:=XML.GetValue(Path+'SQL/Value'{%H-}, '');
  FSaveSQL:=XML.GetValue(Path+'SQL/Value'{%H-}, '');

  FDatabase:= XML.GetValue(Path+'Database/Value'{%H-}, '');

  C:=XML.GetValue(Path+'Params/Count/Value', 0);
  for i:=0 to C-1 do
    FParams.Add(
        StrToFieldType(XML.GetValue(Path+'Params/Item'+IntToStr(i)+'/ParamType', '')),
        XML.GetValue(Path+'Params/Item'+IntToStr(i)+'/Name', ''),
        XML.GetValue(Path+'Params/Item'+IntToStr(i)+'/Value', '')
        );

  S:=XML.GetValue(Path+'MacroChar/Value', MacroChar);
  if S<>'' then
    MacroChar:=S[1];
end;

procedure TLRZMacroQuery.SaveToXML(XML: TLrXMLConfig; const Path: String);
var
  i:integer;
  P:TQueryParam;
begin
  inherited SaveToXML(XML, Path);
  XML.SetValue(Path+'SQL/Value', TZMacroQuery(DataSet).SQL.Text);
  XML.SetValue(Path+'Database/Value', FDatabase);

  XML.SetValue(Path+'Params/Count/Value', FParams.Count);
  for i:=0 to FParams.Count-1 do
  begin
    P:=TQueryParam(FParams[i]);
    XML.SetValue(Path+'Params/Item'+IntToStr(i)+'/Name', P.ParamName);
    XML.SetValue(Path+'Params/Item'+IntToStr(i)+'/Value', P.ParamValue);
    XML.SetValue(Path+'Params/Item'+IntToStr(i)+'/ParamType', Fieldtypenames[P.ParamType]);
  end;
  XML.SetValue(Path+'MacroChar/Value', MacroChar);
end;

type
  { TLRZQueryParamsProperty }

  TLRZQueryParamsProperty = class(TPropertyEditor)
  public
    function  GetAttributes: TPropertyAttributes; override;
    function GetValue: ansistring; override;
    procedure Edit; override;
  end;

  { TLRZQueryDataBaseProperty }

  TLRZQueryDataBaseProperty = class(TFieldProperty)
  public
    procedure FillValues(const Values: TStringList); override;
  end;

  { TLRZQuerySQLProperty }

  TLRZQuerySQLProperty = class(TStringPropertyEditor)
  public
    function GetAttributes: TPropertyAttributes; override;
    function GetValue: ansistring; override;
    procedure Edit; override;
  end;

function TLRZQuerySQLProperty.GetAttributes: TPropertyAttributes;
begin
  Result:=[paDialog, paReadOnly];
end;

function TLRZQuerySQLProperty.GetValue: ansistring;
begin
  Result:='(SQL)';
end;

procedure TLRZQuerySQLProperty.Edit;
var
  TheDialog : TStringsPropEditorDlg;
  AString : string;
begin
  AString := GetStrValue;
  TheDialog := TStringsPropEditorDlg.Create(nil);
  try
    TheDialog.Editor := Self;
    TheDialog.Memo.Text := AString;
    TheDialog.MemoChange(nil);
    if (TheDialog.ShowModal = mrOK) then
    begin
      AString := TheDialog.Memo.Text;
      //erase the last lineending if any
      if Copy(AString, length(AString) - length(LineEnding) + 1, length(LineEnding)) = LineEnding then
        Delete(AString, length(AString) - length(LineEnding) + 1, length(LineEnding));
      SetStrValue(AString);
    end;
  finally
    TheDialog.Free;
  end;
end;

procedure TLRZQueryDataBaseProperty.FillValues(const Values: TStringList);
begin
  if (GetComponent(0) is TLRZMacroQuery) then
    frGetComponents(nil, TZConnection, Values, nil);
end;

function TLRZQueryParamsProperty.GetAttributes: TPropertyAttributes;
begin
  Result:=[paDialog, paReadOnly];
end;

function TLRZQueryParamsProperty.GetValue: ansistring;
begin
  Result:='(Params)';
end;

procedure TLRZQueryParamsProperty.Edit;
begin
  if (GetComponent(0) is TLRZMacroQuery) then
    TLRZMacroQuery(GetComponent(0)).DoEditParams;
end;

initialization
  InitLRComp;

  RegisterPropertyEditor(TypeInfo(string), TLRZMacroQuery, 'Database', TLRZQueryDataBaseProperty);
  RegisterPropertyEditor(TypeInfo(string), TLRZMacroQuery, 'SQL', TLRZQuerySQLProperty);
  RegisterPropertyEditor(TypeInfo(TQueryParamList), TLRZMacroQuery, 'Params', TLRZQueryParamsProperty);

finalization
  if Assigned(lrBMP_ZQuery) then
    FreeAndNil(lrBMP_ZQuery);
end.

