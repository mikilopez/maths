unit moprime;

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
    TMOPrime = class(TMathOperation)
	public
        constructor create();
		function getDescription(): string; override;
		procedure perform(input: TStrings); override;

        function isPrime(n: int64): boolean;
		procedure getPrimeNumbers(n1, n2: int64);
		function nextPrime(n: int64): int64;
	end;

implementation

constructor TMOPrime.create();
begin
	inherited create();
end;

function TMOPrime.getDescription(): string;
begin
    result := 'primo N [M]' + #13#10#9 + 'Indica si el número N es primo. Si M está presente, calcula los primos entre N y M.';
end;

procedure TMOPrime.perform(input: TStrings);
var
    n1: int64;
    n2: int64;
begin
    if input.count = 1 then
    begin
        n1 := getInt(input[0]);
		if n1 < 1 then
		begin
			writeLn('El valor debe ser un número natural: ' + input[0]);
		end
		else if isPrime(n1) then
			writeLn('true')
		else
			writeLn('false')
    end
    else if input.count = 2 then
    begin
        n1 := getInt(input[0]);
        n2 := getInt(input[1]);
		if (n1 < 1) or (n2 < 1) then
			writeLn('Los valores deben ser un números naturales: ' + input[0] + ' ' + input[1])
		else if n1 > n2 then
			writeLn('M debe ser mayor que N')
		else
			getPrimeNumbers(n1, n2);
    end
    else begin
        writeLn('Número de parámetros incorrecto: ' + intToStr(input.count));
    end;
end;

function TMOPrime.isPrime(n: int64): boolean;
var
	max, d: int64;
	divisible: boolean;
begin
	if (n=1) or (n=2) or (n=3) then
	begin
		result := true;
		exit;
	end
	else if (n mod 2 = 0) then
	begin
		result := false;
		exit;
	end
	else
	begin
		d := 1;
		divisible := false;
		max := trunc(sqrt(n));
		repeat
			d += 2;
			divisible := (n mod d) = 0;
		until divisible or (d > max);
	end;
	result := not divisible;
end;

procedure TMOPrime.getPrimeNumbers(n1, n2: int64);
var
    i: int64;
begin
    i := n1;
	if isPrime(i) then writeLn(i);
	i := nextPrime(i);
    while i <= n2 do
	begin
		writeLn(i);
        i := nextPrime(i);
	end;
end;

function TMOPrime.nextPrime(n: int64): int64;
var
	i: int64;
begin
	i := n;

	if i = 1 then
	begin
		result := 2;
	end
	else
	begin
		if i mod 2 = 0 then
		begin
			inc(i);
		end
		else
		begin
			i += 2;
		end;
		while not isPrime(i) do
		begin
			i += 2;
		end;
		result := i;
	end;
end;

end.