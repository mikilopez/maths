unit mathglobals;

{$mode objfpc}{$H+}

interface

uses
	{$IFDEF UNIX}
		{$IFDEF UseCThreads}
			cthreads,
		{$ENDIF}
		cmem,
	{$ENDIF}
	Crt,
	Classes,
	SysUtils;

type
    TPoint = record
		x: double;
		y: double;
	end;

procedure showError(mensaje: string);
function getInt(val: string): int64;
function getDouble(val: string): double;
function getPoint(val: string): TPoint;
function formatReal(val: double): string;
function formatImaginary(val: double): string;

implementation

procedure showError(mensaje: string);
begin
	textColor(red);
	write('ERROR: ');
	textColor(white);
	writeLn(mensaje);
	normVideo();
end;

function getInt(val: string): int64;
begin
	try
		result := StrToInt64(trim(val));
	except
		showError('Value is not a valid integer: ' + val);
	end;
end;

function getDouble(val: string): double;
begin
	try
		result := StrToFloat(trim(val));
	except
		showError('Value is not a valid decimal: ' + val);
	end;
end;

function getPoint(val: string): TPoint;
var
	p: TPoint;
	s: string;
	d: TStringList;
begin
	try
		s := StringReplace(val, '(', '', [rfReplaceAll]);
		s := StringReplace(s, ')', '', [rfReplaceAll]);
		d := TStringList.Create;
		d.delimiter := ',';
		d.strictDelimiter := true;
		d.delimitedText := s;
		p.x := StrToFloat(d[0]);
		p.y := StrToFloat(d[1]);
		d.free;
		result := p;
	except
		showError('Value is not a valid point: ' + val);
	end;
end;

function formatReal(val: double): string;
begin
	result := formatFloat(' + 0.###############; - 0.###############; ', val);
end;

function formatImaginary(val: double): string;
begin
	result := formatFloat(' + 0.###############i; - 0.###############i; ', val);
end;

end.
