unit moparabola;

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
	mathglobals,
    movertex;

type
    TMOParabola = class(TMathOperation)
	public
        constructor create();
		function getDescription(): string; override;
		procedure perform(input: TStrings); override;
	end;

implementation

constructor TMOParabola.create();
begin
	inherited create();
end;

function TMOParabola.getDescription(): string;
begin
    result := 'parabola P Q R [vertice]'
            + #13#10#9 + 'Interpola la parábola de la forma y = ax² + bx + c que pasa por tres puntos P, Q y R.'
            + #13#10#9 + 'Si se indica [vertice] al final calcula también el vértice de la parábola';
end;

procedure TMOParabola.perform(input: TStrings);
var
	a, b, c, den: double;
	p1, p2, p3: TPoint;
    vertice: boolean;
    opVertex: TMOVertex;
    v: TPoint;
begin
	p1 := getPoint(input[0]);
	p2 := getPoint(input[1]);
	p3 := getPoint(input[2]);
    vertice := (input.count = 4) and (input[3] = 'vertice');
	
	// Comprobación - No hay coordenadas X repetidas
	if (p1.x = p2.x) or (p2.x = p3.x) or (p1.x = p3.x) then
	begin
		showError('no pueden haber dos puntos con la misma coordenada X');
		exit;
	end;

	// Comprobación - Puntos no alineados
    // TODO hacerlo con operación de alineación
	if ((p2.y - p1.y)/(p2.x - p1.x)) = ((p3.y - p2.y)/(p3.x - p2.x)) then
	begin
		showError('los puntos de una parábola no pueden estar alineados');
		exit;
	end;

	// Cálculo de la parábola
	// Código correcto pero optimizable
	//den := p1.x*p1.x*p2.x + p1.x*p3.x*p3.x + p2.x*p2.x*p3.x - p3.x*p3.x*p2.x - p2.x*p2.x*p1.x - p1.x*p1.x*p3.x;
	//a := (p2.x*p1.y + p1.x*p3.y + p3.x*p2.y - p2.x*p3.y - p1.x*p2.y - p3.x*p1.y)/den;
	//b := (p1.y - a*p1.x*p1.x - p2.y + a*p2.x*p2.x)/(p1.x - p2.x);
	//c := p1.y - a*p1.x*p1.x - b*p1.x;
	den := p1.x*(p2.x*(p1.x - p2.x) + p3.x*(p3.x - p1.x)) + p2.x*p3.x*(p2.x - p3.x);
	a := (p2.x*(p1.y-p3.y) + p1.x*(p3.y-p2.y) + p3.x*(p2.y-p1.y))/den;
	b := (p1.y - p2.y + a*(p2.x*p2.x - p1.x*p1.x))/(p1.x - p2.x);
	c := p1.y - p1.x*(a*p1.x + b);

	writeLn('Parábola resultante: y = ',
			floatToStr(a), 'x²',
			formatReal(b), 'x',
			formatReal(c));
	
	// Cálculo del vértice
	if vertice then
	begin
		opVertex := TMOVertex.create();
        v := opVertex.getVertex(a, b, c);
        writeLn('Vértice en (', floatToStr(v.x), ',', floatToStr(v.y), ')');
	end;
end;

end.