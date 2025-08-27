unit mathoperation;

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
	SysUtils;

type
	TMathOperation = class abstract
	public
		function getDescription(): string; virtual; abstract;
		procedure perform(input: TStrings); virtual; abstract;
	end;

implementation

end.