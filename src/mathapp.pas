unit mathapp;

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
	UComplex,
	FGL,

	mathglobals,
	mathoperation,

	moprime,
	moparabola,
	movertex,
	mofactors,
	modivisors,
	mocollatz,
	mosolve;

type
	TMathOperationMap = specialize TFPGMap<string, TMathOperation>;
	TMathsApp = class
	private
		data: TStringList;
		operations: TMathOperationMap;
		procedure performOperation(cmd: string);
		procedure getParamsFromExe();
		procedure getParamsFromConsole(cmdData: string);
		procedure enterConsoleMode();
		procedure writeHelp();
		procedure version();
	public
		constructor create();
		destructor destroy(); override;
		procedure run();
	end;

implementation

constructor TMathsApp.create();
begin
	inherited create();
	data := TStringList.create();
	operations := TMathOperationMap.create();
	operations.add('primo', TMOPrime.create());
	operations.add('factores', TMOFactors.create());
	operations.add('divisores', TMODivisors.create());
	operations.add('parabola', TMOParabola.create());
	operations.add('vertice', TMOVertex.create());
	operations.add('collatz', TMOCollatz.create());
	operations.add('ecuacion', TMOSolve.create());
end;

destructor TMathsApp.destroy();
begin
	data.free();
	operations.free();
	inherited destroy();
end;

procedure TMathsApp.run();
var
	cmd: string;
begin
	if paramCount = 0 then
	begin
		writeHelp();
		showError('No se ha indicado ninguna operación a realizar');
		exit;
	end
	else
	begin
		cmd := paramStr(1);
		getParamsFromExe();
		case cmd of
			'console': enterConsoleMode();
			'help': writeHelp();
			'version': version();
			else performOperation(cmd);
		end;
	end;
end;

procedure TMathsApp.performOperation(cmd: string);
var
	op: TMathOperation;
begin
	operations.tryGetData(cmd, op);
	if op = nil then
	begin
		writeLn('Operación desconocida: ' + cmd);
	end
	else
	begin
		op.perform(data);
	end;
end;

procedure TMathsApp.getParamsFromExe();
var
	i: int64;
begin
	data.clear();
	if paramCount > 1 then
	for i := 2 to paramCount do
	begin
		data.add(paramStr(i));
	end;
end;

procedure TMathsApp.getParamsFromConsole(cmdData: string);
begin
	data.clear();
	data.delimiter := ' ';
	data.strictDelimiter := true;
	data.delimitedText := cmdData;
end;

procedure TMathsApp.enterConsoleMode();
var
	history: TStringList;
	line, cmd, cmdData: string;
	p: int64;
	exitConsole: boolean;
begin
	writeLn('Entrando en modo consola.');
	writeLn();
	history := TStringList.create();
	
	exitConsole := false;
	repeat
		write('> ');
		readLn(line);
		p := pos(' ', line);
		if (p > 0) then
		begin
			cmd := copy(line, 0, p-1);
			cmdData := copy(line, p+1, length(line));
		end
		else
		begin
			cmd := line;
			cmdData := '';
		end;
		
		history.add(line);

		getParamsFromConsole(cmdData);
		case cmd of
			'': ; // no hacer nada
			'exit': exitConsole := true;
			'help': writeHelp();
			'version': version();
			'console': writeLn('Ya estamos en el modo consola :)');
			else performOperation(cmd);
		end;
	until exitConsole;
	writeLn('Saliendo del modo consola');
	
	history.free();
end;

procedure TMathsApp.writeHelp();
var
	op: TMathOperation;
	i: int64;
begin
	version();
	writeLn();
	writeLn('Programa para realizar pequeños cálculos matemáticos');
	writeLn();
	writeLn('Uso del programa: maths OPCION [datos]');
	writeLn();
	if data.count = 0 then
	begin
		writeLn('Posibles opciones:');
		writeLn('help' + #13#10#9 + 'Muestra esta ayuda.');
		writeLn('version'  + #13#10#9 +  'Indica la versión y fecha del programa.');
		writeLn('console'  + #13#10#9 +  'Inicia la aplicación en modo consola.');
		writeLn();

		for i := 0 to operations.count - 1 do
		begin
			op := operations.data[i];
			writeLn(op.getDescription());
		end;
		
		writeLn();
	end
	else
	begin
		operations.tryGetData(data[0], op);
		if op = nil then
		begin
			writeLn('Operación desconocida: ' + data[0]);
		end
		else
		begin
			writeLn(op.getDescription());
		end;
	end;
end;

procedure TMathsApp.version();
begin
	writeLn('maths v0.0.6 ', {$i %DATE%});
end;

end.
