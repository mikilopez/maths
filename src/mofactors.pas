unit mofactors;

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
    moprime;

type
    TMOFactors = class(TMathOperation)
	public
        constructor create();
		function getDescription(): string; override;
		procedure perform(input: TStrings); override;
	end;

implementation

constructor TMOFactors.create();
begin
	inherited create();
end;

function TMOFactors.getDescription(): string;
begin
    result := 'factores N'
            + #13#10#9 + 'Obtiene los factores primos de N';
end;

procedure TMOFactors.perform(input: TStrings);
var
	n, p: int64;
	nprimo: boolean;
    opPrime: TMOPrime;
begin
	n := getInt(input[0]);
	
	if n < 1 then
	begin
		showError('Solo se pueden factorizar números naturales: ' + IntToStr(n));
		exit;
	end;

    opPrime := TMOPrime.create();
	p := 2;
	nprimo := opPrime.isPrime(n);
	while not nprimo do
	begin
		if n mod p = 0 then
		begin
			WriteLn(p);
			n := n div p;
			nprimo := opPrime.isPrime(n);
		end
		else
		begin
			p := opPrime.nextPrime(p);
		end;
	end;
    writeLn(n); // Imprimir el último factor
end;

end.