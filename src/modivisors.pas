unit modivisors;

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
    TMODivisors = class(TMathOperation)
	public
        constructor create();
		function getDescription(): string; override;
		procedure perform(input: TStrings); override;
	end;

implementation

constructor TMODivisors.create();
begin
	inherited create();
end;

function TMODivisors.getDescription(): string;
begin
    result := 'divisores N'
            + #13#10#9 + 'Obtiene los divisores de N';
end;

procedure TMODivisors.perform(input: TStrings);
var
	n, i, h: int64;
	parejas: TList;
	raiz: boolean;
begin
	n := getInt(input[0]);
	
	if n < 1 then
	begin
		showError('El número ha de ser natural: ' + IntToStr(n));
		exit;
	end;
	
	parejas := TList.Create;
	h := trunc(sqrt(n));
	raiz := h*h = n;
	if raiz then dec(h);
	for i := 1 to h do
	begin
		if n mod i = 0 then
		begin
			writeLn(i);
			parejas.add(Pointer(n div i));
		end;
	end;
	if raiz then writeLn(h + 1);
	for i := parejas.count - 1 downto 0 do
	begin
		writeLn(int64(parejas[i]));
	end;
	parejas.Free;
end;

end.