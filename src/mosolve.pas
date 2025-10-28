unit mosolve;

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
    TMOSolve = class(TMathOperation)
	public
        constructor create();
		function getDescription(): string; override;
		procedure perform(input: TStrings); override;

		procedure solveEquation(const a, b, c: double; var x1r, x1i, x2r, x2i: double; var u: boolean);
	end;

implementation

constructor TMOSolve.create();
begin
	inherited create();
end;

function TMOSolve.getDescription(): string;
begin
    result := 'ecuacion a b'
            + #13#10#9 + 'Resuelve una ecuación lineal ax + b = 0'
            + #13#10
            + 'ecuacion a b c'
            + #13#10#9 + 'Resuelve una ecuación de segundo grado ax² + bx + c = 0';
end;

procedure TMOSolve.perform(input: TStrings);
var
	  a, b, c, x1r, x2r, x1i, x2i: double;
	  unica: boolean;
begin
    if input.count = 2 then
    begin
        input.insert(0, '0');
    end;
	a := getDouble(input[0]);
	b := getDouble(input[1]);
	c := getDouble(input[2]);
	
	// Validar a, b y c
	if (a=0) and (b=0) and (c=0) then
	begin
		showError('alguno de los coeficientes ha de ser distinto de cero');
        exit;
	end
	else if (a=0) and (b=0) then
	begin
		showError('uno de los coeficientes A o B ha de ser distinto de cero');
        exit;
	end;
	
	// Calcular ...
	solveEquation(a, b, c, x1r, x1i, x2r, x2i, unica);

	// Imprimir solución por pantalla
	write(floatToStr(x1r));
	writeLn(formatImaginary(x1i));
	if not unica then
	begin
		write(floatToStr(x2r));
		writeLn(formatImaginary(x2i));
	end;
end;

procedure TMOSolve.solveEquation(const a, b, c: double; var x1r, x1i, x2r, x2i: double; var u: boolean);
var
	v: double;
begin
	u := false;

	// Casos  ax² = 0  y  bx = 0
	if ((a=0) or (b=0)) and (c=0) then
	begin
		x1r := 0; x1i := 0; x2r := 0; x2i := 0;
		u := a = 0;
	end;
	
	// Caso  bx + c = 0
	if (a=0) and (c<>0) then
	begin
		x1r := -c/b; x1i:=0;
		u := true;
	end;
	
	// Caso  ax² + c = 0
	if (b=0) and (c<>0) then
	begin
		v := -c/a;
		if v > 0 then
		begin
			x1r := sqrt(v);
			x2r := -x1r;
			x1i := 0;
			x2i := 0;
		end
		else
		begin
			x1i := sqrt(-v);
			x2i := -x1i;
			x1r := 0;
			x2r := 0;
		end;
	end;
	
	// Caso  ax² + bx = 0
	if (a<>0) and (b<>0) and (c=0) then
	begin
		x1r := 0;
		x1i := 0;
		x2i := 0;
		x2r := -b/a;
	end;

	// Caso general (fórmula)
	if (a<>0) and (b<>0) and (c<>0) then
	begin
		if (b*b -4*a*c > 0) then
		begin
			x1r := (-b + sqrt(b*b - 4*a*c))/(2*a);
			x2r := (-b - sqrt(b*b - 4*a*c))/(2*a);
			x1i := 0;
			x2i := 0;
		end
		else
		begin
			x1r := -b/(2*a);
			x2r := x1r;
			x1i := sqrt(4*a*c - b*b)/(2*a);
			x2i := -x1i;
		end;
	end;
end;

end.