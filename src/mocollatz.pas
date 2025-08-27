unit mocollatz;

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
    TMOCollatz = class(TMathOperation)
	public
        constructor create();
		function getDescription(): string; override;
		procedure perform(input: TStrings); override;
	end;

implementation

constructor TMOCollatz.create();
begin
	inherited create();
end;

function TMOCollatz.getDescription(): string;
begin
    result := 'collatz N'
            + #13#10#9 + 'Obtiene la secuencia de Collatz del número N';
end;

procedure TMOCollatz.perform(input: TStrings);
var
	i, last, cota: int64;
begin
	last := getInt(input[0]);
	cota := last;
	// TODO validación positivo?
	i := 0;
	repeat
		inc(i);
		writeLn(last);
		if last mod 2 = 0 then
		begin
			last := last div 2;
		end
		else
		begin
			last := 3*last + 1;
		end;
		if last > cota then cota := last;
	until last = 1;
	writeLn(last);
	writeLn;
	writeLn('Proceso completado en ', i, ' iteraciones.');
	writeLn('Cota máxima ha sido ', cota);
end;

end.