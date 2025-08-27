unit movertex;

{$mode objfpc}{$H+}

interface

uses
	{$IFDEF UNIX}
		{$IFDEF UseCThreads}
			cthreads,
		{$ENDIF}
		cmem,
	{$ENDIF}
    Classes,
	SysUtils,

	mathoperation,
	mathglobals;

type
    TMOVertex = class(TMathOperation)
	public
        constructor create();
		function getDescription(): string; override;
		procedure perform(input: TStrings); override;

		function getVertex(a, b, c: double): TPoint;
	end;

implementation

constructor TMOVertex.create();
begin
	inherited create();
end;

function TMOVertex.getDescription(): string;
begin
    result := 'vertice a b c'
            + #13#10#9 + 'Calcula el vértice de una parábola y = ax² + bx + c';
end;

procedure TMOVertex.perform(input: TStrings);
var
	a, b, c: double;
	v: TPoint;
begin
	a := getDouble(input[0]);
	b := getDouble(input[1]);
	c := getDouble(input[2]);
	
	v := getVertex(a, b, c);
	writeLn('Vértice en (', floatToStr(v.x), ',', floatToStr(v.y), ')');
end;

function TMOVertex.getVertex(a, b, c: double): TPoint;
var
	x, y: double;
	p: TPoint;
begin
	if a = 0 then
	begin
		showError('la parábola no es válida (a=0)');
		exit;
	end;

	x := -b/(2*a);
	y := a*x*x + b*x+c;
	p.x := x;
	p.y := y;
	result := p;
end;

end.