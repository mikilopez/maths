program maths;

{$mode objfpc}{$H+}

uses
	{$IFDEF UNIX}
		{$IFDEF UseCThreads}
			cthreads,
		{$ENDIF}
		cmem,
	{$ENDIF}
	mathapp;

var
	app: TMathsApp;

begin
	app := TMathsApp.create();
	app.run;
	app.free;
end.
