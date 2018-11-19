
{*******************************************************}
{							}
{       Delphi ZeosLib Component Library		}
{       ZeosLib include file				}
{                                                       }
{*******************************************************}

{$A+}	{+ Aligned record fields }
{$B-}	{- Complete boolean eval }
{$P+}	{- Open string params }
{$R-}	{- Range-Checking }
{$V-}	{- Var-String Checking }
{$X+}	{- Extended syntax }
{.$IFDEF WIN32}
{$J+}	{- Writeable structured consts }
{$H+}	{- Use long strings by default }
{.$ENDIF}

{Include it for DEBUGGING}
{$D+}	{ DEBUGINFO ON }
{.$L+}	{ LOCALSYMBOLS ON }
{$O-}	{ OPTIMIZATION OFF }

{$IFDEF VER100} { Borland Delphi 3.0 }
  {$DEFINE ZD3}
{$ENDIF}

{$IFDEF VER110} { Borland C++Builder 3.0 }
  {$DEFINE ZC3}
  {$ObjExportAll On}
{$ENDIF}

{$IFDEF VER120} { Borland Delphi 4.0 }
  {$DEFINE ZD4}
  {$DEFINE ZVCL4}
{$ENDIF}

{$IFDEF VER125} { Borland C++Builder 4.0 }
  {$DEFINE ZC4}
  {$DEFINE ZVCL4}
  {$ObjExportAll On}
{$ENDIF}

{$IFDEF VER130} { Borland Delphi 5.0 & Borland C++Builder 5.0 }
  {$DEFINE ZD4}
  {$DEFINE ZVCL4}
  {$DEFINE ZVCL5}
{$ENDIF}

{$IFDEF VER140} { Borland Delphi 6.0 & Borland C++Builder 6.0 }
  {$DEFINE ZD4}
  {$DEFINE ZVCL4}
  {$DEFINE ZVCL5}
  {$DEFINE ZVCL6}
{$ENDIF}

{$IFDEF VER150} { Borland Delphi 7.0 }
  {$DEFINE ZD4}
  {$DEFINE ZVCL4}
  {$DEFINE ZVCL5}
  {$DEFINE ZVCL6}
{$ENDIF}

{$IFDEF VER160}	{ Borland Delphi 8.0 for .NET Framework } {also CIL name is defined}
  {$DEFINE ZD4}
  {$DEFINE ZCLR} // Common Language Runtime
  {$DEFINE ZVCL4}
  {$DEFINE ZVCL5}
  {$DEFINE ZVCL6}
  {$DEFINE ZVCL71}
{$ENDIF}

{$IFDEF FPC} {FPC & Lazarus}
  {$DEFINE ZD4}
  {$DEFINE ZVCL4}
  {$DEFINE ZVCL5}
  {$DEFINE ZVCL6}
  {$mode objfpc}
  {$H+}
{$ENDIF}
